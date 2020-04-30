import gleam/dynamic.{bool, Dynamic}
import gleam/config
import gleam/expect
import gleam/atom.{Atom, create_from_string}
import gleam/list
import gleam/map.{Map, insert, new}
import gleam/result.{Option}

// TODO: Replace with Gleam code when implemented.
pub external fn putenv(key: List(Int), value: List(Int)) -> Bool = "os" "putenv"
pub external fn unsetenv(key: List(Int)) -> Bool = "os" "unsetenv"
pub external fn gleam_str_to_erl_str(value) -> List(Int) = "erlang" "binary_to_list"

pub fn new_config_test() {
  config.start_link()

  config.new()
  |> expect.equal(_, Ok(True))

  config.get("berry.black.has_some")
  |> expect.equal(_, Ok(True))

  config.put("berry.black.has_some", False)

  config.get("berry.black.has_some")
  |> expect.equal(_, Ok(False))

  config.get_default("bad.key", False)
  |> expect.equal(_, Ok(False))

  config.get("psum")
  |> expect.equal(_, Ok("lorem ipsum dolor sit amet"))

  config.get("test_int.value")
  |> expect.equal(_, Ok(1))

  config.get("test_float.value")
  |> expect.equal(_, Ok(2.2))

  config.stop()
}

pub fn read_env_var_test() {
  let env_var = gleam_str_to_erl_str("GLEAM_CONFIG")
  let env_cfg = gleam_str_to_erl_str("test/config/env_config.toml")

  putenv(env_var, env_cfg)

  config.new()
  |> expect.equal(_, Ok(True))

  unsetenv(env_var)
}

pub fn read_invalid_config_test() {
  let env_var = gleam_str_to_erl_str("GLEAM_CONFIG")
  let invalid_env_cfg = gleam_str_to_erl_str("test/config/invalid.toml")

  putenv(env_var, invalid_env_cfg)

  config.new()
  |> expect.equal(_, Error("Error in file: test/config/invalid.toml at line 3"))

  unsetenv(env_var)
}

pub fn get_all_config_test() {
  let env_var = gleam_str_to_erl_str("GLEAM_CONFIG")
  let env_cfg = gleam_str_to_erl_str("test/config/get_all.toml")

  putenv(env_var, env_cfg)

  config.new()

  let expected_config =
    map.new() 
    |> map.insert(_, "test1", "test1")
    |> map.insert(_, "test2", "test2")

  config.get_config()
  |> expect.equal(_, expected_config)
}

pub fn put_batch_test() {
  let env_var = gleam_str_to_erl_str("GLEAM_CONFIG")
  let env_cfg = gleam_str_to_erl_str("test/config/get_all.toml")

  putenv(env_var, env_cfg)

  config.start_link()

  config.new()

  let new_config =
    map.new()
    |> map.insert(_, "test2", "asdf")
    |> map.insert(_, "test3", "test3")

  config.put_batch(new_config)

  let expected_config =
    map.new()
    |> map.insert(_, "test1", "test1")
    |> map.insert(_, "test2", "asdf")
    |> map.insert(_, "test3", "test3")

  config.get_config()
  |> expect.equal(_, expected_config)
}
