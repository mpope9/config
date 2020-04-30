# config

![test](https://github.com/mpope9/config/workflows/test/badge.svg)

#### TOC
[Installation](##installation)
[Example Usage](##example-usage)

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

Configuration:
```toml
[test1.test2]
test3 = true
```

Usage:
```gleam
import gleam/config
import gleam/dynamic

config.new()                    // Parses the config and stores it.
config.start()                  // Starts the config server.
config.get("test1.test2.test3") // Access.
|> expect.equal(_, Ok(True))

config.get_default("not.existing", False)
|> expect.equal(_, Ok(False))
```

## Updating Values
Updates to the config can have a performance penalty, due to persistent_terms being optimized for reads over writes.  So it is preffered to update configs in batches.  This is done through passing a map to the config module.  Values in the map will override existing configuration values.

```gleam
import gleam/map

let new_config = 
   map.new
   |> map.insert(_, "key1", "value1")
   |> map.insert(_, "Ricky Booby", "Cal Naughton Jr.")

config.put_batch(new_config)
```

To update a single value, the following api is provided, although its use is discouraged if multiple updates need to be made.  It will also override an existing value.
````gleam
config.put("key1", "value1")
```

## Get Entire Config
Getting the whole configuration is supported.
```gleam
config.get_config()
```

## Keys
Parses toml files and stores them as strings.  At this time, only string key are supported.
An example:

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
3. Replace toml library with Gleam implementation (file read, etc.)

## Quick start

```sh
# Build the project
rebar3 compile

# Run the eunit tests
rebar3 eunit

# Run the Erlang REPL
rebar3 shell
```

