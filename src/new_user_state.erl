-module(new_user_state).
-export([init/1, callback_mode/0]).
-export([unauthorized/1, unauthorized/3]).

callback_mode() -> [state_functions, state_enter].

init(Args) -> erlang:error(undefined, Args).

unauthorized(Pid) ->
    {keep_state, Pid}.

unauthorized(cast, Data, #{pid := Pid, user := undefined} = State) ->
    gloom:send(Pid, "Hi, ~s, enter your NEW password: ", [Data]),
    {keep_state, State#{user => Data}};

unauthorized(cast, Data, #{pid := Pid, pass := undefined} = State) ->
    gloom:send(Pid, "Please confirm: "),
    {keep_state, State#{pass => Data}};

unauthorized(cast, Data, #{pid := Pid, user := User, pass := Data} = _State) ->
    case gloom_user:create(User, Data) of
        {ok, Id} -> 
            gloom:send(Pid, "Registration complete!\n"),
            {authorized, {Pid, Id}, [{change_callback_module, login_state}]};
        {error, _} ->
            gloom:send(Pid, "Registration failed.\n"),
            {repeat_state, Pid}
    end;

unauthorized(cast, Data, #{pid := Pid, pass := Pass}) when Data =/= Pass ->
    gloom:send(Pid, "Confirmation failed.\n"),
    {repeat_state, Pid};

unauthorized(enter, _, Pid) ->
    gloom:send(Pid, "Enter NEW username: "),
    {keep_state, #{pid => Pid, user => undefined, pass => undefined}}.
