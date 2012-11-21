-module(elli_example_router).

-export([preprocess/2,
         handlers/1,
         postprocess/3,
         handle_event/3]).


preprocess(Req, _Config) ->
    case elli_request:path(Req) of
        [P|_] when P =:= <<"number">>;
                   P =:= <<"list">> -> Req;
        _ -> {nopost, {403, [], "Forbidden"}}
    end.
            
handlers(_Config) ->
    [{[{class, atom}, {op, atom}, {number, integer}], elli_example_bindings, []},
     {[{class, atom}, {op, atom}, {list, list}], elli_example_bindings, []}].

postprocess(_Req, Response, _Config) -> Response.

handle_event(_Event, _Args, _Config) -> ok.
