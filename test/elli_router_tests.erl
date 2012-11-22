-module(elli_router_tests).
-include_lib("eunit/include/eunit.hrl").


elli_router_test_() ->
    {setup,
     fun setup/0, fun teardown/1,
     [
      ?_test(number_op()),
      ?_test(list_op()),
      ?_test(cars()),
      ?_test(forbidden())
     ]}.

%%
%% TESTS
%%


number_op() ->
    URL = "http://localhost:3003/number/double/21",
    {ok, Response} = httpc:request(URL),
    ?assertEqual("42", body(Response)).

list_op() ->
    URL = "http://localhost:3003/list/reverse/a,b,c,d",
    {ok, Response} = httpc:request(URL),
    ?assertEqual("dcba", body(Response)).

cars() ->
    URL = "http://localhost:3003/cars/1968/corvette/black",
    {ok, Response} = httpc:request(URL),
    ?assertEqual("An old black corvette", body(Response)).

forbidden() ->
    URL = "http://localhost:3003/hello",
    {ok, Response} = httpc:request(URL),
    ?assertEqual("Forbidden", body(Response)).
    

%%
%% HELPERS
%%

body({_, _, Body}) ->
    Body.


setup() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    inets:start(),

    {ok, P} = elli:start_link([{callback, elli_router},
                               {callback_args, {elli_example_router, []}},
                               {port, 3003}]),
    unlink(P),
    [P].

teardown(Pids) ->
    [elli:stop(P) || P <- Pids].
