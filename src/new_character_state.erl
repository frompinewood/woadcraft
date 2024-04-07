-module(new_character_state).
-export([init/1, callback_mode/0, terminate/3]).
-export([create/1, create/3]).
-export([send/2]).

init(_) -> erlang:error(not_implemented).

callback_mode() -> [state_functions, state_enter].

terminate(_, _, _) -> ok.

create(State) -> {repeat_state, State}.
create(enter, _, {Pid, Id}) ->
    Prompts = [
               {race, 
                "Enter race: ",
                woadcraft:races()},
               {class,
                "Enter class: ",
                woadcraft:classes()},
               {name, "Enter name: "}
              ],
    gloom:prompt(Pid, create, Prompts),
    {keep_state, {Pid, Id}};

create(cast, {create, #{race := Race, class := Class, name := Name}}, {Pid, Id}) ->
    gloom:send(Pid, io_lib:format("Created ~s ~s ~s!", [Race, Class, Name])),
    {keep_state, {Pid, Id}}.

send(Pid, Data) ->
    gen_statem:cast(Pid, Data).
