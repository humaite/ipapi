%%%===================================================================
%%% @copyright Arweave (c) 2024
%%% @author Arweave team
%%% @author Mathieu Kerjouan
%%% @doc
%%% @end
%%%===================================================================
-module(ipapi_app).
-behaviour(application).
-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    case os:getenv("IPAPI_API_KEY") of
	false -> {error, "IPAPI_API_KEY not defined"};
	ApiKey ->
	    application:set_env(ipapi, api_key, ApiKey),
	    ipapi_sup:start_link()
    end.

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
stop(_State) ->
    ok.
