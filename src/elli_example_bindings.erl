-module(elli_example_bindings).
-export([handle/3]).

handle(_Req, Bindings, _Args) ->
    Class = proplists:get_value(class, Bindings),
    Op = proplists:get_value(op, Bindings),
    handle_op(Class, Op, proplists:get_value(Class, Bindings)).

handle_op(number, Op, N) -> 
    {ok, [], [integer_to_list(number_op(Op, N))]};
handle_op(list, Op, L) -> 
    {ok, [], list_op(Op, L)}.

number_op(double, N) -> N + N;
number_op(square, N) -> N * N. 

list_op(reverse, L) -> lists:reverse(L);
list_op(sort, L)    -> lists:sort(L). 
