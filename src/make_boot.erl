-module(make_boot).
-vc(' $Id ').
-author('tapoueh@users.sourceforge.net').

-export([make_boot/1]).

make_boot([App]) ->
    A = atom_to_list(App),
    case catch systools:make_script(A, [exref, no_module_tests]) of
        ok ->
            halt();
        {'EXIT', Error} ->
            io:format("Error = ~p~n", [Error]),
            halt(1);
        E ->
            io:format("~p~n", [E]),
            halt(1)
    end.

