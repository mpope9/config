%% ----------------------------------
%% @version 0.2.0
%% @doc
%% Base config server to interface with persistent_term storage.
%% Parses a toml file defaulting to `config/config.toml`, or the
%% environment variable GLEAM_CONFIG.
%%
%% Heavily inspired by Elixir's elixir_config.
%%
%% Note, updating configs during runtime carries a heavy penalty.
%% See persistent_term erlang documentation for more details.
%% This server is useful for safe(er) runtime puts, because
%% persistent_term isn't optimized for writes.
%%
%% TODO: Move to Gleam based gen_server when that is implimented.
%% @end
%% ----------------------------------

-module(config_server).
-compile({no_auto_import}).
-export([new/0, get_config/0, put_batch/1]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, stop/0]).
-behaviour(gen_server).

-define(DEFAULT_CONFIG, "config/config.toml").
-define(ENV_FILE, "GLEAM_CONFIG").

-define(CONFIG_KEY, {?MODULE, config_map}).

%% ----------------------------------
%% Client api
%% ----------------------------------

%% ----------------------------------
%% Parse the config.  On failure, return a
%% Gleam binary error message.
%% ----------------------------------
-spec new() -> {error, binary()} | {ok, true}.

new() ->
   ConfigFile = os:getenv(?ENV_FILE, ?DEFAULT_CONFIG),
   case tomerl:read_file(ConfigFile) of
      
      {ok, Config} ->
         {_, ConfigMap} = maps:fold(fun process_map/3, {<<>>, #{}}, Config),
         io:format("~p~n", [ConfigMap]),
         persistent_term:put(?CONFIG_KEY, ConfigMap),
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

%% ----------------------------------
%% Gets the entire config.
%% ----------------------------------
-spec get_config() -> map().

get_config() ->
   persistent_term:get(?CONFIG_KEY).

%% ----------------------------------
%% Replaces the current config with the passed map.
%% ----------------------------------
-spec put_batch(map()) -> term().

put_batch(NewConfig) ->
   gen_server:call(?MODULE, {put, NewConfig}).

%% ----------------------------------
%% gen_server api
%% ----------------------------------

-spec start_link() -> {ok, pid()}.

start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, ?MODULE, []).

-spec init(term()) -> {ok, term()}.

init(Opt) ->
   {ok, Opt}.

handle_call({put, NewConfig}, _From, State) ->
   persistent_term:put(?CONFIG_KEY, NewConfig),
   {reply, true, State}.

handle_cast(Cast, State) ->
   {stop, {bad_cast, Cast}, State}.

stop() ->
   gen_server:stop(?MODULE).

%% ----------------------------------
%% Internal Functions
%% ----------------------------------
-spec process_map(binary(), term(), binary()) -> binary().

process_map(CurrentKey, Value, {PartialKey, AccMap}) ->

   FullKey = <<PartialKey/binary, CurrentKey/binary>>,
   case is_map(Value) of

      true ->
         Dot = <<".">>,
         ExtendedKey = <<FullKey/binary, Dot/binary>>,
         {_, NewAccMap} = maps:fold(fun process_map/3, {ExtendedKey, AccMap}, Value),
         {PartialKey, NewAccMap};
         
      _ ->
         NewAccMap = maps:put(FullKey, Value, AccMap),
         {PartialKey, NewAccMap}
   end.

% EOF
