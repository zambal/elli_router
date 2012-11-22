-module(elli_example_bindings).
-export([handle/3]).

handle(Req, Bindings, _Args) ->
    case elli_request:path(Req) of
        [<<"cars">>|_] ->
            cars(proplists:get_value(year, Bindings),
                 proplists:get_value(model, Bindings),
                 proplists:get_value(color, Bindings));
        _ ->
            Class = proplists:get_value(class, Bindings),
            Op = proplists:get_value(op, Bindings),
            handle_op(Class, Op, proplists:get_value(Class, Bindings))
    end.

cars(Year, Model, Color) ->
    Prefix = if Year < 2000  -> <<"An old ">>;
                Year < 2009  -> <<"A slightly old ">>;
                Year < 2012  -> <<"A new ">>;
                Year >= 2012 -> <<"A brand new ">>
             end,
    {ok, [], [Prefix, Color, <<" ">>, Model]}.


handle_op(number, Op, N) -> 
    {ok, [], [integer_to_list(number_op(Op, N))]};
handle_op(list, Op, L) -> 
    {ok, [], list_op(Op, L)}.

number_op(double, N) -> N + N;
number_op(square, N) -> N * N. 

list_op(reverse, L) -> lists:reverse(L);
list_op(sort, L)    -> lists:sort(L). 
