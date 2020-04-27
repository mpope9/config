# config

A Gleam configuration library.

Relys on erlang's [persistent_terms](https://erlang.org/doc/man/persistent_term.html), so updating config values during runtime carries a heavy penalty (forces a GC on each process).

## Installation

```erlang
{deps, [
    {config, {git, "https://github.com/mpope9/config"}}
]}.
```

## Example Usage
This is powered by a gen_server, but configs are parsed and stored independently.  The gen_server is for safety and to optimize puts.

```gleam
import gleam/config

config.new()                    // Parses the config and stores it.
config.start()                  // Starts the config server.
config.get("test1.test2.test3") // Access.
```

## Keys
Parses toml files and stores them.  An example:

```toml
[test1.test2]
test3 = true
```

This config's key is transalted to the Gleam string:
```gleam
"test1.test2.test3"
```
and returns an atom value when accessed.

## Set Configuration File
The default config value lives at `config/config.toml` for a project.
To use a seperate config file, you can set the environment variable `GLEAM_CONFIG`.

### TODO
1. Ability to add to the Gleam supervision tree.
2. Gleam gen_server implementation.
3. Cleanup the Erlang toml library.
4. Replace toml library with Gleam implementation (file read, etc.)

## Quick start

```sh
# Build the project
rebar3 compile

# Run the eunit tests
rebar3 eunit

# Run the Erlang REPL
rebar3 shell
```

