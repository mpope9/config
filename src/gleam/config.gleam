import gleam/dynamic.{Dynamic}
import gleam/atom.{Atom}

///
/// Configuration server for Gleam.
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
/// ```
///
/// The default config value lives at `config/config.toml` for a project.
/// To use a seperate config file, you can set the environment variable
/// `GLEAM_CONFIG`.
///
/// Things needed to be implemented.
/// 1. Addition to the Gleam supervision tree.
/// 2. Gleam gen_server implementation.
/// 3. Cleanup the Erlang toml library.
/// 4. Replace toml library with Gleam implementation (file read, etc.)
///

// Extern types from the config_server
// TODO: Move config_server to Gleam gen_server
pub external fn new_config() -> Result(Bool, String) 
  = "config_server" "new"
pub external fn start_link_config_server() -> tuple() 
  = "config_server" "start_link"
pub external fn get_config_server(key: String) -> Dynamic 
  = "config_server" "get"
pub external fn get_default_config_server(key: String, default) -> Dynamic 
  = "config_server" "get"
pub external fn put_config_server(key: String, value) -> Dynamic
  = "config_server" "put"
pub external fn stop_config_server() -> Atom
  = "config_server" "stop"

///
/// Starts the config gen_server backend.
///
pub fn start_link() -> Bool {
  start_link_config_server()
  True
}

///
/// Stops the config server backend.
pub fn stop() -> Atom {
  stop_config_server()
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
pub fn get(key: String) -> Dynamic {
  get_config_server(key)
}

///
/// Gets the specified key.  If it doesn't exist, returns the default value.
///
pub fn get_default(key: String, value) {
  get_default_config_server(key, value)
}

///
/// Stores a new configuration value.  Use with caution.
///
pub fn put(key: String, value) {
  put_config_server(key, value)
}
