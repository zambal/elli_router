%% @doc: HTTP routing handler.
%%
%% This module offers routing of requests.
%% 
%% It includes both pattern match and regular expression based URL
%% dispatching. 
%%
%% Optionally, pre- and post-process steps can also be added. 
%% Preprocess functions can shortcircuit the request (with the
%% 'nopost' return even excluding post-processing) in order to make
%% validation or authentication possible. 
%%
%% Usage:
%%
%%  elli:start_link([..., {callback, elli_router},
%%                        {callback_args, {elli_router_example, []}}]).
%%
%% The configured router callback module may implement the following
%% functions:
%%
%%
%% - preprocess(Req, Args) -> NewReq | {NewReq, Response} | {nopost, Response}.
%%
%% - handlers(Args) -> [{[<<"hello">>, '*'], elli_example_middleware, []},
%%                      {<<"^/(send|receive)file/.*$">>, elli_example_callback, []}].
%%
%% postprocess(Req, Response, Args) -> NewResponse.
%%
%% handle_event(Event, EventArgs, Args) -> ok.
%%
%% All events are send to the router callback's handle_event function
%% and events send after URL dispatching are also routed to the
%% dispatched handler until the request is completed. However, the
%% elli_startup event is forwarded to all handlers, in order to make
%% initialisation possible.
%%
%% elli_router is implemented as a regular Elli handler, so it can be
%% freely mixed with other Elli middleware or callback handlers.

-module(elli_router).
-behaviour(elli_handler).
-export([handle/2, handle_event/3]).

-export([match_pat/2]).

%%
%% ELLI CALLBACKS
%%

handle(Req, Callback) ->
    case preprocess(Req, Callback) of
        {nopost, Response} -> Response;
        {Req0, Response} -> postprocess(Req0, Response, Callback);
        Req0 -> postprocess(Req0, dispatch(Req, handlers(Callback)), Callback)
    end.

handle_event(elli_startup, Args, Callback) ->
    event_handler(elli_startup, Args, Callback),
    [begin
         code:ensure_loaded(M),
         event_handler(elli_startup, Args, {M, ModArgs})
     end || {M, ModArgs} <- mods(Callback)],
    ok;
handle_event(Event, Args, Callback) ->
    event_handler(Event, Args, Callback),
    dispatch_event(Event, Args).


%%
%% DISPATCH LOGIC
%%

preprocess(Req, {M, Args}) ->
    case erlang:function_exported(M, preprocess, 2) of
        true  -> M:preprocess(Req, Args);
        false -> Req
    end.

handlers({M, Args}) ->
    case erlang:function_exported(M, handlers, 1) of
        true  -> case M:handlers(Args) of
                     {M0, Args0} -> [{'...', M0, Args0}];
                     Handlers -> Handlers
                 end;
        false -> []
    end.

postprocess(Req, Resp, {M, Args}) ->
    case erlang:function_exported(M, postprocess, 2) of
        true  -> M:postprocess(Req, Resp, Args);
        false -> Resp
    end.

dispatch(Req, [{Route, M, Args}|Rs]) ->
    case match(Req, Route) of
        {true, Bindings} ->
            set_dispatch_handler({M, Args}),
            case erlang:function_exported(M, handle, 3) of
                true  -> M:handle(Req, Bindings, Args);
                false -> M:handle(Req, Args)
            end;
        false -> dispatch(Req, Rs)
    end;
dispatch(_Req, []) -> {404, [], <<"Not Found">>}.

event_handler(Event, EventArgs, {M, Args}) ->
    case erlang:function_exported(M, handle_event, 3) of
        true  -> M:handle_event(Event, EventArgs, Args);
        false -> ok
    end.

dispatch_event(Event, EventArgs) ->
    case get_dispatch_handler(Event) of
        {M, Args} ->
            event_handler(Event, EventArgs, {M, Args});
        undefined -> ok
    end.


%%
%% INTERNAL HELPERS
%%

mods(Callback) -> 
    [{M, Args} || {_Route, M, Args} <- handlers(Callback)].   

get_dispatch_handler(Event)
  when Event == request_complete;
       Event == chunk_complete -> erase(elli_dispatch_handler);
get_dispatch_handler(_Event)   -> get(elli_dispatch_handler). 

set_dispatch_handler(Mod) -> put(elli_handler, Mod).

match(Req, Route) ->
    case is_list(Route) of
        true  -> match_pat(elli_request:path(Req), Route);
        false -> match_regex(elli_request:raw_path(Req), Route)
    end.

match_pat(Path, Route) -> match_pat(Path, Route, []).

match_pat([PT|PTs], [RT|RTs], Bind) when PT == RT ->
    match_pat(PTs, RTs, Bind);
match_pat([PT|PTs] = Path, [RT|RTs], Bind) when is_atom(RT) ->
    case RT of
        '...' -> {true, lists:reverse(Bind) ++ Path};
        '_'   -> match_pat(PTs, RTs, Bind);
        '*'   -> match_pat(PTs, RTs, [PT|Bind]);
        Key   -> match_pat(PTs, RTs, [{Key, PT}|Bind])
    end;
match_pat([PT|PTs], [{Key, Type}|RTs], Bind) ->
    try V = case Type of
                integer   -> binary_to_integer(PT);
                float     -> list_to_float(binary_to_list(PT));
                atom      -> binary_to_existing_atom(PT, utf8);
                string    -> binary_to_list(PT);
                binary    -> PT;
                list      -> csv_to_list(PT);
                range     -> binary_to_range(PT);
                num_range -> {From, To} = binary_to_range(PT),
                             {binary_to_integer(From), binary_to_integer(To)}
            end,
          match_pat(PTs, RTs, [{Key, V}|Bind])
    catch
        error:badarg -> false
    end;
match_pat([], [], Bind) -> {true, lists:reverse(Bind)};
match_pat(_, _, _Bind)  -> false.

match_regex(Path, Route) ->
    case re:run(Path, Route, [{capture, none}]) of
        match   -> {true, []};
        nomatch -> false
    end.

binary_to_range(Bin) ->
    case binary:split(Bin, <<"-">>) of
        [From, To] -> {From, To};
        _ -> erlang:error(badarg)
    end.

binary_to_integer(Bin) ->
    list_to_integer(binary_to_list(Bin)).

csv_to_list(Bin) ->
    case binary:split(Bin, <<",">>, [global]) of
        []   -> erlang:error(badarg);
        List -> List 
    end.
