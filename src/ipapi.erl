%%%===================================================================
%%%
%%%===================================================================
-module(ipapi).
-export([simple/1, bulk/1]).
-export([is_ip/1, ntoa/1]).
-define(API_URL, "api.ipapi.is").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%-------------------------------------------------------------------
-spec is_ip(list() | binary()) -> boolean().

is_ip(IP) ->
    case ntoa(IP) of
	{ok, _} -> true;
	_ -> false
    end.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec ntoa(RawIP) -> Return when
      RawIP :: binary() | string(),
      Return :: {ok, binary()} | {error, term()}.

ntoa(IP)
  when is_binary(IP) ->
    ntoa(binary_to_list(IP));
ntoa(IP) 
  when is_list(IP) ->
    case inet:parse_address(IP) of
	{ok, Address} ->
	    List = inet:ntoa(Address),
	    {ok, list_to_binary(List)};
	Elsewise ->
	    Elsewise
    end.

%%--------------------------------------------------------------------
%% @doc
%%
%% ```
%% curl 'https://api.ipapi.is?q=32.5.140.2&key=private_key'
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec simple(IP) -> Return when
      IP :: string() | binary(),
      Return :: {ok, map()}.

simple(IP) ->
    case ntoa(IP) of
	{ok, Address} ->
	    Query = [{"q", Address}],
	    Path = "/",
	    request(#{ method => get
		     , path => Path
		     , query => Query
		     });
	Elsewise ->
	    Elsewise
    end.

%%--------------------------------------------------------------------
%% @doc
%%
%% Equivalent to:
%%
%% ```
%% curl --header "Content-Type: application/json" \
%%      --request POST \
%%      --data '{"ips": ["162.158.0.0", "20.41.193.225"]}' \
%%      'https://api.ipapi.is?key=private_key
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec bulk(IPs) -> Return when
      IPs :: [IP],
      IP :: string() | binary(),
      Return :: {ok, map()}.

bulk(IPs) ->
    Foldl = lists:foldl(
	      fun (I,Acc) ->
		      case ntoa(I) of
			  {ok, Address} ->
			      [Address|Acc];
			  _Elsewise ->			      
			      Acc
		      end
	      end,
	      [], IPs),
    bulk2(Foldl).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
bulk2([]) ->
    {error, no_valid_addresses};
bulk2(IPs) 
  when length(IPs) > 100 ->
    {error, too_many_ips};
bulk2(IPs) ->
    Path = "/",
    Headers = [{"content-type", "application/json"}],
    Body = jsone:encode(#{ ips => IPs }),
    request(#{ method => post 
	     , path => Path
	     , headers => Headers
	     , body => Body
	     }).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
request(Payload) ->
    request_apikey(Payload).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
request_apikey(Payload) ->
    Query = maps:get(query, Payload, []),
    case application:get_env(ipapi, api_key) of
	undefined ->
	    {error, api_key};
	{ok, ApiKey} ->
	    NewQuery = [{"key", ApiKey}|Query],
	    NewPayload = Payload#{ query => NewQuery },
	    request_query(NewPayload)
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
request_query(Payload) ->
    Query = maps:get(query, Payload, []),
    case uri_string:compose_query(Query) of
	String when is_list(String) ->
	    NewPayload = Payload#{ query => String },
	    request_method(NewPayload);
	String when is_binary(String) ->
	    List = binary_to_list(String),
	    NewPayload = Payload#{ query => List },
	    request_method(NewPayload);
	Elsewise -> 
	    Elsewise
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
request_method(#{ method := get } = Payload) ->
    http_get(Payload);
request_method(#{ method := post } = Payload) ->
    http_post(Payload).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
http_get(Payload) ->
    Connection = ipapi_connection:get_connection(),
    Path = maps:get(path, Payload, "/"),
    Query = maps:get(query, Payload, ""),
    Headers = maps:get(headers, Payload, []),
    FullPath = uri_string:normalize(#{ path => Path
				     , query => Query
				     }),
    Ref = gun:get(Connection, FullPath, Headers, #{}),
    await(Connection, Ref).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
http_post(Payload) ->
    Connection = ipapi_connection:get_connection(),
    Path = maps:get(path, Payload, "/"),
    Query = maps:get(query, Payload, ""),
    Headers = maps:get(headers, Payload, []),
    Body = maps:get(body, Payload, <<>>),
    FullPath = uri_string:normalize(#{ path => Path
				     , query => Query 
				     }),
    Ref = gun:post(Connection, FullPath, Headers, Body, #{}),
    await(Connection, Ref).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
await(Connection, Ref) ->
    case gun:await(Connection, Ref) of
	{response, nofin, 200, _Headers} ->
	    await_body(Connection, Ref);
	Elsewise ->
	    Elsewise
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
await_body(Connection, Ref) ->
    case gun:await_body(Connection, Ref) of
	{ok, Body} ->
	    {ok, jsone:decode(Body)};
	Elsewise ->
	    Elsewise
    end.


    
