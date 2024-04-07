-module(login_state).
-behavior(gen_statem).
-export([start_link/2, init/1, callback_mode/0, terminate/3]).
-export([
    login/1, login/3,
    create/1, create/3,
    find/1, find/3
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
        {ok, Id} -> {next_state, find, {Pid, Id}};
        {error, bad_password} -> {keep_state, {Pid, User}};
        {error, _} -> {keep_state, Pid}
    end.

create(Pid) -> {keep_state, Pid}.

create(enter, _, Pid) ->
    Prompts = [
        {username, "Enter username: "},
        {password, "Enter password: "}
    ],
    gloom:prompt(Pid, create, Prompts),
    {keep_state, Pid};

create(cast, {create, #{username := User, password := Pass}}, Pid) ->
    case gloom_user:create(User, Pass) of
        {ok, Id} -> {next_state, find, {Pid, Id}};
        {error, _} -> {keep_state, Pid}
    end.

find({Pid, Id}) -> {keep_state, {Pid, Id}}.

find(enter, _, {Pid, Id}) ->
    gloom:send(Pid, "This game world does not yet exist and your location could not be found."),
    gloom:send(Pid, close),
    {keep_state, {Pid, Id}}.

terminate(_, _, _) -> ok.

%% gloom behavior
send(Pid, Data) ->
    gen_statem:cast(Pid, Data).
