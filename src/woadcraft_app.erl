%%%-------------------------------------------------------------------
%% @doc woadcraft public API
%% @end
%%%-------------------------------------------------------------------

-module(woadcraft_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gloom:start_listener(woadcraft, 8080, login_state),
    woadcraft_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
