import Lake
open Lake DSL

package «baseball-verify» where


lean_lib BaseballVerify where
  globs := #[.andSubmodules `BaseballVerify]

lean_exe DiffTest where
  root := `DiffTest.Main
