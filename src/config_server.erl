%% ----------------------------------
%% @version 0.1.0
%% @doc
%% Base config server to interface with persistent_term storage.
%% Parses a toml file defaulting to `config/config.toml`, or the
%% environment variable GLEAM_CONFIG.
%%
%% Heavily inspired by Elixir's elixir_config.
%%
%% Note, updating configs during runtime carries a heavy penalty.
%% See persistent_term erlang documentation for more details.
%% This server is useful for safe runtime puts, because
%% persistent_term isn't optimized for writes.
%%
%% TODO: Move to Gleam based gen_server when that is implimented.
%% @end
%% ----------------------------------

-module(config_server).
-compile({no_auto_import}).
-export([new/0, put/2, get/1, get/2]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, stop/0]).
-behaviour(gen_server).

-define(DEFAULT_CONFIG, "config/config.toml").
-define(ENV_FILE, "GLEAM_CONFIG").

%% ----------------------------------
%% Client api
%% ----------------------------------

%% Parse the config.  On failure, return a
%% Gleam binary error message.
new() ->
   ConfigFile = os:getenv(?ENV_FILE, ?DEFAULT_CONFIG),
   case toml:read_file(ConfigFile) of
      
      {ok, Config} ->
         maps:fold(fun process_map/3, <<>>, Config),
         {ok, true};

      {error, {parse, LineNumber}} ->
         ErrorMessage = 
            "Error in file: " ++ 
            ConfigFile ++ 
            " at line " ++ 
            integer_to_list(LineNumber),
         ErrorBinary = list_to_binary(ErrorMessage),
         {error, ErrorBinary}
   end.

get(Key) ->
   PersistentKey = {?MODULE, Key},
   persistent_term:get(PersistentKey).

get(Key, Default) ->
   PersistentKey = {?MODULE, Key},
   persistent_term:get(PersistentKey, Default).

put(Key, Value) ->
   gen_server:call(?MODULE, {put, Key, Value}).

%% ----------------------------------
%% gen_server api
%% ----------------------------------

start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, ?MODULE, []).

init(Opt) ->
   {ok, Opt}.

handle_call({put, Key, Value}, _From, State) ->
   PersistentKey = {?MODULE, Key},
   persistent_term:put(PersistentKey, Value),
   {reply, Value, State}.

handle_cast(Cast, State) ->
   {stop, {bad_cast, Cast}, State}.

stop() ->
   gen_server:stop(?MODULE).

%% ----------------------------------
%% Internal Functions
%% ----------------------------------
process_map(CurrentKey, Value, PartialKey) ->

   FullKey = <<PartialKey/binary, CurrentKey/binary>>,
   case is_map(Value) of

      true ->
         Dot = <<".">>,
         ExtendedKey = <<FullKey/binary, Dot/binary>>,
         maps:fold(fun process_map/3, ExtendedKey, Value),
         PartialKey;
         
      _ ->
         Key = {?MODULE, FullKey},
         persistent_term:put(Key, Value),
         PartialKey
   end.

% EOF
