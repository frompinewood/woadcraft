-module(login_state).
-behavior(gen_statem).
-export([start_link/2, init/1, callback_mode/0, terminate/3]).
-export([
    unauthorized/1, unauthorized/3,
    authorized/1, authorized/3
]).
-export([send/2]).

start_link(Pid, Args) ->
    gen_statem:start_link(?MODULE, [Pid, Args], []).

%% state machine API
init([Pid, _Args]) ->
    {ok, unauthorized, Pid}.

callback_mode() -> [state_functions, state_enter].

terminate(shutdown, _, _) ->
    ok;
terminate(_Reason, _State, _Data) ->
    ok.

%% states
unauthorized(Pid) ->
    {keep_state, Pid}.

unauthorized(enter, _State, Pid) ->
    Prompt = [
        {username,
            "Welcome to a land of Woadcraft\n"
            "\n"
            "Please enter your username, or NEW if you are new\nEnter username: "}
    ],
    gloom:prompt(Pid, username, Prompt),
    {keep_state, Pid};
unauthorized(cast, {username, #{username := <<"NEW">>}}, Pid) ->
    Prompt = [
        {username, "Enter username: "},
        {password, "Enter password: "}
    ],
    gloom:prompt(Pid, new_user, Prompt),
    {keep_state, Pid};
unauthorized(cast, {new_user, Creds}, Pid) ->
    #{username := User, password := Pass} = Creds,
    case gloom_user:create(User, Pass) of
        {ok, Id} ->
            {next_state, authorized, {Pid, Id}};
        {error, _} ->
            gloom:send(Pid, "Registration failed."),
            {repeat_state, Pid}
    end;
unauthorized(cast, {username, #{username := User}}, Pid) ->
    Prompt = [{password, "Enter password: "}],
    gloom:prompt(Pid, password, Prompt),
    {keep_state, {Pid, User}};
unauthorized(cast, {password, #{password := Pass}}, {Pid, User}) ->
    case gloom_user:verify(User, Pass) of
        {ok, Id} ->
            {next_state, authorized, {Pid, Id}};
        {error, _} ->
            gloom:send(Pid, "Login failed."),
            {repeat_state, Pid}
    end.

authorized(State) ->
    {keep_state, State}.
authorized(enter, _, {Pid, _} = State) ->
    gloom:send(Pid, "Authenticated!"),
    {keep_state, State}.

%% gloom behavior
send(Pid, Data) ->
    gen_statem:cast(Pid, Data).
