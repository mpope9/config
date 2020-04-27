import gleam/dynamic.{bool, Dynamic}
import gleam/config
import gleam/expect
import gleam/atom.{Atom}
import gleam/list

// TODO: Replace with Gleam code when implemented.
pub external fn putenv(key: List(Int), value: List(Int)) -> Bool = "os" "putenv"
pub external fn unsetenv(key: List(Int)) -> Bool = "os" "unsetenv"
pub external fn gleam_str_to_erl_str(value) -> List(Int) = "erlang" "binary_to_list"

pub fn new_config_test() {
  config.start_link()

  config.new()
  |> expect.equal(_, Ok(True))

  config.get("berry.black.has_some")
  |> dynamic.bool
  |> expect.equal(_, Ok(True))

  config.put("berry.black.has_some", False)

  config.get("berry.black.has_some")
  |> dynamic.bool
  |> expect.equal(_, Ok(False))

  config.stop()
}

pub fn read_env_var_test() {
  let env_var = gleam_str_to_erl_str("GLEAM_CONFIG")
  let env_cfg = gleam_str_to_erl_str("config/env_config.toml")

  putenv(env_var, env_cfg)

  config.new()
  |> expect.equal(_, Ok(True))

  unsetenv(env_var)
}

pub fn read_invalid_config_test() {
  let env_var = gleam_str_to_erl_str("GLEAM_CONFIG")
  let invalid_env_cfg = gleam_str_to_erl_str("config/invalid.toml")

  putenv(env_var, invalid_env_cfg)

  config.new()
  |> expect.equal(_, Error("Error in file: config/invalid.toml at line 3"))

  unsetenv(env_var)
}
