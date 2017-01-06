%%%-------------------------------------------------------------------
%%% @author Blaise Fringel
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jan 2017 10:01 AM
%%%-------------------------------------------------------------------
-module(sc_event).
-author("Blaise Fringel").

-behaviour(gen_event).

%% API
-export([start_link/0,
  lookup/1,
  create/2,
  replace/2,
  delete/1,
  add_handler/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() -> {ok, pid()} | {error, {already_started, pid()}}).
start_link() ->
  gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @end
%%--------------------------------------------------------------------
-spec(add_handler() -> ok | {'EXIT', Reason :: term()} | term()).
add_handler(Handler, Args) ->
  gen_event:add_handler(?SERVER, Handler, Args).

%%--------------------------------------------------------------------
%% @doc
%% Create an event
%%
%% @end
%%--------------------------------------------------------------------
create(Key, Value) ->
  gen_event:notify(?SERVER, {create, {Key, Value}}).

%%--------------------------------------------------------------------
%% @doc
%% Lookup an event
%%
%% @end
%%--------------------------------------------------------------------
lookup(Key) ->
  gen_event:notify(?SERVER, {lookup, Key}).

%%--------------------------------------------------------------------
%% @doc
%% Delete an event
%%
%% @end
%%--------------------------------------------------------------------
delete(Key) ->
  gen_event:notify(?SERVER, {delete, Key}).

%%--------------------------------------------------------------------
%% @doc
%% Replace an event
%%
%% @end
%%--------------------------------------------------------------------
replace(Key, Value) ->
  gen_event:notify(?SERVER, {replace, {Key, Value}}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================