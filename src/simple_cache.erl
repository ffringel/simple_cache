%%%-------------------------------------------------------------------
%%% @author Blaise Fringel
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2016 1:10 PM
%%%-------------------------------------------------------------------
-module(simple_cache).
-author("Blaise Fringel").

%% API
-export([insert/2,
  lookup/1,
  delete/1]).

%%%===================================================================
%%% API
%%%===================================================================

insert(Key, Value) ->
  case sc_store:lookup(Key) of
    {ok, Pid}  ->
      sc_event:replace(Key),
      sc_element:replace(Pid, Value);
    {error, _Reason} ->
      sc_event:create(Key),
      sc_element:create(Value),
      sc_store:insert(Key, Value)
end.

lookup(Key) ->
  try
    {ok, Pid} = sc_store:lookup(Key),
    {ok, Value} = sc_element:fetch(Pid),
    {ok, Value}
  catch
      _Class:_Exception -> {error, not_found}
  end.

delete(Key) ->
  case sc_store:lookup(Key) of
    {ok, Pid} ->
      sc_event:delete(Key),
      sc_element:delete(Pid);
    {error, _Reason} ->
      ok
  end.