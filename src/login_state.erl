-module(login_state).
-behavior(gen_statem).
-export([start_link/2, init/1, callback_mode/0, terminate/3]).
-export([
    login/1, login/3,
    create/1, create/3
]).
-export([send/2]).

start_link(Pid, _) ->
    gen_statem:start_link(?MODULE, [Pid], []).

callback_mode() -> [state_functions, state_enter].

init([Pid]) ->
    gloom:send(
        Pid,
        "##################################\n"
        "|Welcome to the land of Woadcraft|\n"
        "|(C) 2024 Micaiah Parker         |\n"
        "##################################\n"
        "\n"
    ),
    {ok, login, Pid}.

login(Pid) ->
    {keep_state, Pid}.

login(enter, _, Pid) ->
    PromptText =
        "Enter your login username\n"
        "or NEW to create a new login\n"
        "Username: ",
    gloom:prompt(Pid, username, [{username, PromptText}]),
    {keep_state, Pid};

login(cast, {username, #{username := <<"NEW">>}}, Pid) ->
    {next_state, create, Pid};

login(cast, {username, #{username := User}}, Pid) ->
    gloom:prompt(Pid, password, [{password, "Enter password: "}]),
    {keep_state, {Pid, User}};

login(cast, {password, #{password := Pass}}, {Pid, User}) ->
    case gloom_user:verify(User, Pass) of
        {ok, Id} -> 
            gloom:send(Pid, "Authenticated."),
            {repeat_state, {Pid, Id}, [{push_callback_module, new_character_state}]};
        {error, bad_password} -> 
            gloom:send("Login failed."),
            {repeat_state, {Pid, User}};
        {error, _} -> {repeat_state, Pid}
    end.

create(Pid) -> {keep_state, Pid}.

create(enter, _, Pid) ->
    Prompts = [
        {username, "Enter username: "},
        {password, "Enter password: "},
        {confirm,  "Confirm password: "}
    ],
    gloom:prompt(Pid, create, Prompts),
    {keep_state, Pid};

create(cast, {create, #{username := User, password := Pass, confirm := Pass}}, Pid) ->
    case gloom_user:create(User, Pass) of
        {ok, Id} -> 
            gloom:send(Pid, "New account created!"),
            {repeat_state, {Pid, Id}, [{push_callback_module, new_character_state}]};
        {error, _} -> 
            gloom:send(Pid, "Unable to create new account "++User),
            {repeat_state, Pid}
    end;
create(cast, {create, #{pass := Pass, confirm := Pass}}, Pid) ->
    gloom:send(Pid, "Password mismatch."),
    {repeat_state, Pid}.

terminate(_, _, _) -> ok.

%% gloom behavior
send(Pid, Data) ->
    gen_statem:cast(Pid, Data).
