import Lake
open Lake DSL

package «elan-cleanup» {
  -- add package configuration options here
}

@[default_target]
lean_exe «elan-cleanup» {
  root := `Main
}
