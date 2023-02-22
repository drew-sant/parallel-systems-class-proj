%%%-------------------------------------------------------------------
%% @doc hello public API
%% @end
%%%-------------------------------------------------------------------

-module(hello_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	Dispatch = cowboy_router:compile([
        {'_',[
			{"/", toppage_h, [] },
			{"/gfriends", get_friends, []},
			{"/pfriends",set_friends, []},
			{"/afriends",add_friends, []}
		]}
    ]),

%%    {ok, _} = cowboy:start_clear(my_http_listener,
%%        [{port, 80}],
%%        #{env => #{dispatch => Dispatch}}

        PrivDir = code:priv_dir(hello),
	{ok, _} = cowboy:start_tls(https_listener, [
						    {port, 443},
						    {certfile, PrivDir ++ "/ssl/fullchain.pem"},
						    {keyfile, PrivDir ++ "/ssl/privkey.pem"}
						   ],
	#{env => #{dispatch => Dispatch}}
    ),
    hello_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
