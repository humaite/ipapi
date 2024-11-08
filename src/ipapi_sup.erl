%%%===================================================================
%%% @copyright Arweave (c) 2024
%%% @author Arweave team
%%% @author Mathieu Kerjouan
%%% @doc
%%% @end
%%%===================================================================
-module(ipapi_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

%%--------------------------------------------------------------------
%% @hidden
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @hidden
%%---------------------------------------------------------------------
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [#{ id => ipapi_connection
		   , start => {ipapi_connection, start_link, []}
		   , type => worker 
		   }
		 ],
    {ok, {SupFlags, ChildSpecs}}.

