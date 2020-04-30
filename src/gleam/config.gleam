import gleam/dynamic.{Dynamic}
import gleam/atom.{Atom}
import gleam/map.{Map, insert, new}
import gleam/result.{Option}

///
/// Configuration server for Gleam.
/// v0.2.0
/// Relys on erlang's persistent_terms, so updating config values during
/// runtime carries a heavy penalty (forces a GC on each process).
///
/// Parses toml files and stores them.  An example:
///
/// ```toml
/// [test1.test2]
/// test3 = true
/// ```
///
/// This config's key is transalted to the Gleam string:
/// ```gleam
/// "test1.test2.test3"
/// ```
/// and returns an atom value when accessed.
//
/// This is powered by a gen_server, but configs are parsed and stored
/// independently.  The gen_server is for safety and to optimize puts.
///
/// ```gleam
/// import gleam/config
///
/// config.new()                    // Parses the config and stores it.
/// config.start()                  // Starts the config server.
/// config.get("test1.test2.test3") // Access.
///   // Ok(True)
/// ```
///
/// The default config value lives at `config/config.toml` for a project.
/// To use a seperate config file, you can set the environment variable
/// `GLEAM_CONFIG`.
///
/// Updates to the config can carry heavy runtime penalties.  To minimize this,
/// this should be done by batching configuration updates.  The function
/// put_batch is prefered.
///

// Extern types from the config_server
// TODO: Move config_server to Gleam gen_server
external fn new_config() -> Result(Bool, String) 
  = "config_server" "new"
external fn config_server_start_link() -> tuple() 
  = "config_server" "start_link"
external fn config_server_get_config() -> Map(String, v)
  = "config_server" "get_config"
external fn config_server_put_batch(new_config: Map(String, v)) -> Bool
  = "config_server" "put_batch"
external fn config_server_stop() -> Atom
  = "config_server" "stop"

///
/// Starts the config gen_server backend.
///
pub fn start_link() -> Bool {
  config_server_start_link()
  True
}

///
/// Stops the config server backend.
pub fn stop() -> Atom {
  config_server_stop()
}

///
/// Parses the toml config and stores it in the persistent_term backend.
///
pub fn new() -> Result(Bool, String) {
  new_config()
}

///
/// Gets the specified key.
///
pub fn get(key: String) -> Option(v) {
  let config = config_server_get_config()
  map.get(config, key)
}

///
/// Gets the specified key.  If it doesn't exist, returns the default value.
///
pub fn get_default(key: String, value) -> Option(v) {
  let config = config_server_get_config()

  case map.has_key(config, key) {
    True -> map.get(config, key)
    False -> Ok(value)
  }
}

pub fn get_config() {
  config_server_get_config()
}

///
/// Stores a new configuration value.  Use with caution.
///
pub fn put(key: String, value) -> Bool {
  let input =
    map.new()
    |> map.insert(_, key, value)

  let config = config_server_get_config()
  let new_config = map.merge(config, input)

  config_server_put_batch(new_config)
  True
}

///
/// Stores a new batch of configuration.  Any values existing in the current
/// configuration are overwritten.  It is prefered this is used over `put/2`
///
pub fn put_batch(input: Map(String, v)) -> Bool {
  let config = config_server_get_config()

  let new_config = map.merge(config, input)

  config_server_put_batch(new_config)
  True
}
