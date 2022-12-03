import Lean.Data.HashSet

open IO
open IO.FS
open Lean (HashSet)
open System

def try? [Monad m] [MonadExcept e m] (x : m α) : m (Option α) :=
  try
    some <$> x
  catch _ =>
    return none

def ignoringNoFileOrDirectory (x : IO Unit) : IO Unit :=
  try
    x
  catch e =>
    if let .noFileOrDirectory .. := e then
      return
    else
      throw e

def removeFileIfExists (fp : FilePath) : IO Unit :=
  ignoringNoFileOrDirectory (removeFile fp)

def removeDirAllIfExists (fp : FilePath) : IO Unit :=
  ignoringNoFileOrDirectory (removeDirAll fp)

def isIgnoredDirPath (fp : FilePath) : Bool :=
  if let some dirName := fp.fileName then
    dirName.startsWith "."
  else
    false

structure Toolchain where
  toString : String
  deriving BEq, Hashable

partial def walkFilesystem [Monad m] [MonadLiftT IO m] (root : FilePath)
    (init : α)
    (onFile : α → FilePath → Metadata → m α)
    (onDir : α → FilePath → Metadata → m (α × Bool) :=
      λ acc _ _ => return (acc, true))
    (onSymlink : α → FilePath → Metadata → m (α × Bool) :=
      λ acc _ _ => return (acc, true))
    (onOther : α → FilePath → Metadata → m α :=
      λ acc _ _ => return acc)
     :
    m α := do
  let (_, (acc, _)) ← go root |>.run (init, {})
  return acc
where
  go (fp : FilePath) : StateT (α × HashSet FilePath) m Unit := do
    if let some mdata ← show IO _ from try? fp.metadata then
      match mdata.type with
      | .file =>
        let (acc, seenDirs) ← get
        let acc ← onFile acc fp mdata
        set (acc, seenDirs)
      | .other =>
        let (acc, seenDirs) ← get
        let acc ← onOther acc fp mdata
        set (acc, seenDirs)
      | .dir =>
        let (acc, _) ← get
        let (acc, cont) ← onDir acc fp mdata
        modify λ (_, seenDirs) => (acc, seenDirs.insert fp)
        if cont then
          (← fp.readDir).forM λ entry => go entry.path
      | .symlink =>
        let (acc, seenDirs) ← get
        let (acc, cont) ← onSymlink acc fp mdata
        set (acc, seenDirs)
        if cont then
          go (← realPath fp)

def getUsedToolchains (root : FilePath) : IO (HashSet Toolchain) :=
  walkFilesystem root {}
    (onFile := λ toolchains fp _ => do
      if let some fname := fp.fileName then
        if fname == "lean-toolchain" then
          let toolchain :=
            (← readFile fp).takeWhile λ c => c != '\n' && c != '\r'
          println! "found toolchain '{toolchain}' (in {fp})"
          return toolchains.insert ⟨toolchain⟩
      return toolchains)
    (onDir := λ toolchains fp _ => do
      if isIgnoredDirPath fp then
        return (toolchains, false)
      else
        return (toolchains, true))

def getHomeDir : IO FilePath := do
  let home? ← getEnv "HOME"
  if let some home := home? then
    return home
  else
    throw $ userError s!"unable to determine home directory: $HOME environment variable is not set"

def getElanRoot : IO FilePath := do
  let homeDir ← getHomeDir
  let elanRoot := homeDir / ".elan"
  if ! (← elanRoot.isDir) then
    throw $ userError s!"elan root directory '{elanRoot}' is not a directory"
  return elanRoot

def Toolchain.dirName (toolchain : Toolchain) : String :=
  toolchain.toString.replace "/" "--" |>.replace ":" "---"

def Toolchain.ofDirName (dirName : String) : Toolchain :=
  ⟨dirName.replace "---" ":" |>.replace "--" "/"⟩

def toolchainsDir (elanRoot : FilePath) : FilePath :=
  elanRoot / "toolchains"

def updateHashesDir (elanRoot : FilePath) : FilePath :=
  elanRoot / "update-hashes"

def Toolchain.toolchainDir (elanRoot : FilePath) (toolchain : Toolchain) :
    FilePath :=
  toolchainsDir elanRoot / toolchain.dirName

def Toolchain.updateHashFile (elanRoot : FilePath) (toolchain : Toolchain) :
    FilePath :=
  updateHashesDir elanRoot / toolchain.dirName

def getInstalledToolchains (elanRoot : FilePath) : IO (Array Toolchain) :=
  walkFilesystem (updateHashesDir elanRoot) #[] λ toolchains fp _ =>
    if let some fname := fp.fileName then
      return toolchains.push $ .ofDirName fname
    else
      return toolchains

def removeToolchainsExcept (elanRoot : FilePath) (keep : HashSet Toolchain) :
    IO Unit := do
  let installedToolchains ← getInstalledToolchains elanRoot
  for toolchain in installedToolchains do
    if ! keep.contains toolchain then
      println! "erasing {toolchain.toString}"
      removeFileIfExists $ toolchain.updateHashFile elanRoot
      removeDirAllIfExists $ toolchain.toolchainDir elanRoot

def main : IO Unit := do
  let home ← getHomeDir
  let elanRoot ← getElanRoot
  let toolchains ← getUsedToolchains home
  removeToolchainsExcept elanRoot toolchains
