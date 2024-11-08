%%%===================================================================
%%% @copyright Arweave (c) 2024
%%% @author Arweave team
%%% @author Mathieu Kerjouan
%%% @doc
%%% @end
%%%===================================================================
-module(ipapi_connection).
-behavior(gen_statem).
-export([start_link/0, get_connection/0]).
-export([callback_mode/0, init/1, terminate/3]).
-export([connect/3, connected/3]).
-record(?MODULE, { pid }).
-define(TIMEOUT, 5000).

%%--------------------------------------------------------------------
%% @doc start ipapi_connection module.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Returns ipapi http connection.
%% @end
%%--------------------------------------------------------------------
get_connection() ->
    gen_statem:call(?MODULE, {get, connection}, ?TIMEOUT).

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
callback_mode() -> state_functions.    

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
init(_Args) ->
    ApiUrl = "api.ipapi.is",
    Port = 443,
    CaCerts = certifi:cacerts(),
    TlsOpts = [ {verify, verify_peer}
	      , {depth, 99}
	      , {cacerts, CaCerts}
	      ],
    Opts = #{ tls_opts => TlsOpts },
    case gun:open(ApiUrl, Port, Opts) of
	{ok, Pid} ->
	    State = #?MODULE{pid = Pid},
	    {ok, connect, State};
	Elsewise ->
	    {stop, Elsewise}
    end.

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
terminate(_, _, #?MODULE{ pid = Pid } = _State) ->
    gun:shutdown(Pid).

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
connect(info, {gun_up, Pid, http}, #?MODULE{ pid = Pid } = State) ->
    {next_state, connected, State};
connect({call, _}, _, _) ->
    postpone;
connect(cast, _, _) ->
    postpone.

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
connected({call, From}, {get, connection}, #?MODULE{ pid = Pid } = State) ->
    {keep_state, State, [{reply, From, Pid}]};
connected(info, {gun_down, Pid, http, closed, _}, #?MODULE{ pid = Pid } = State) ->
    {next_state, connect, State};
connected(_, _, State) ->
    {keep_state, State}.
