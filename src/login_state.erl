-module(login_state).
-behavior(gen_statem).
-export([start_link/2, init/1, callback_mode/0, terminate/3]).
-export([unauthorized/1, unauthorized/3]).
-export([send/2]).

start_link(Pid, Args) ->
    gen_statem:start_link(?MODULE, [Pid, Args], []).


%% state machine API
init([Pid, _Args]) ->
    {ok, unauthorized, Pid}.

callback_mode() -> [state_functions, state_enter].

terminate(shutdown, _, _) -> ok.

%% states
unauthorized(Pid) ->
    {keep_state, Pid}.

unauthorized(enter, _State, Pid) ->
    gloom:send(Pid, 
               "Welcome to a land of Woadcraft\n"
               "\n"
               "Please enter your username, or NEW if you are new\n"),
    {keep_state, Pid};
unauthorized(cast, <<"NEW">>, Pid) ->
    {repeat_state, Pid, [{change_callback_module, new_user_state}]};
unauthorized(cast, Data, Pid) ->
    gloom:send(
        Pid,
        io_lib:format(
            "Sorry, I don't understand the command: '~p'\n",
            [string:trim(Data)]
        )
    ),
    {keep_state, Pid}.

%% gloom behavior
send(Pid, Data) ->
    gen_statem:cast(Pid, Data).

