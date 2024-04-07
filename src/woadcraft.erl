-module(woadcraft).
-export([get_characters/1, classes/0, races/0]).

get_characters(Id) -> [].

classes() ->
    ["Knight", "Thief", "Magi", "Tinkerer", "Scout", "Elementist"].
races() ->
    ["Human", "Beastman", "Goblin", "Hobbit", "Elf"].
