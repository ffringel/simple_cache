%%%-------------------------------------------------------------------
%%% @author fabu
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Dec 2016 11:28 AM
%%%-------------------------------------------------------------------
{application, simple_cache, [
  {description, "Simple Caching system"},
  {vsn, "0.1.0"},
  {registered, [sc_app,
      sc_sup]},
  {registered, [sc_sup]},
  {applications, [
    kernel,
    stdlib
  ]},
  {mod, {sc_app, []}},
  {env, []}
]}.