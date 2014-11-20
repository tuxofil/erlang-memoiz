%%% @doc
%%% Memoization Library.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 20 Nov 2014
%%% @copyright 2014, Aleksey Morarash <aleksey.morarash@gmail.com>

-module(memoiz).

%% API exports
-export([do/2, do/3]).

-include("memoiz.hrl").

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

%% @doc Apply the functional object with the given args or immediately
%% return result term if the return value is already stored.
-spec do(FunObject :: fun(), Args :: list()) ->
                FunctionResult :: any().
do(FunObject, Args) ->
    Key = get_hash({FunObject, Args}),
    case memoiz_srv:lookup(Key) of
        {ok, Value} ->
            Value;
        undefined ->
            Value = erlang:apply(FunObject, Args),
            ok = memoiz_srv:store(Key, Value),
            Value
    end.

%% @doc Apply the function with the given args or immediately
%% return result term if the return value is already stored.
-spec do(Mod :: atom(), Fun :: atom(), Args :: list()) ->
                FunctionResult :: any().
do(Mod, Fun, Args) ->
    Key = get_hash({{Mod, Fun}, Args}),
    case memoiz_srv:lookup(Key) of
        {ok, Value} ->
            Value;
        undefined ->
            Value = erlang:apply(Mod, Fun, Args),
            ok = memoiz_srv:store(Key, Value),
            Value
    end.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc Make a hash for the Erlang term.
-spec get_hash(Term :: any()) -> Hash :: any().
get_hash(Term) ->
    erlang:phash2(Term, 16#ffffffff).

%% ----------------------------------------------------------------------
%% EUnit tests
%% ----------------------------------------------------------------------

-ifdef(TEST).

-define(
   _assertLong(FunObj, Args),
   ?_assert(timer_tc(FunObj, Args) >= 1000)).

-define(
   _assertFast(FunObj, Args),
   ?_assert(timer_tc(FunObj, Args) < 1000)).

main_test_() ->
    {setup,
     _Setup =
         fun() ->
                 ok = application:load(?MODULE),
                 ok = application:set_env(?MODULE, max_size, 3),
                 ok = application:start(?MODULE)
         end,
     _CleanUp =
         fun(_) ->
                 ok = application:stop(?MODULE)
         end,
     {inorder,
      [%% long, because of first run
       ?_assertLong(fun timer:sleep/1, [2]),
       ?_assertFast(fun timer:sleep/1, [2]),
       %% long, because of first run
       ?_assertLong(fun timer:sleep/1, [3]),
       ?_assertFast(fun timer:sleep/1, [3]),
       %% long, because of first run
       ?_assertLong(fun timer:sleep/1, [4]),
       ?_assertFast(fun timer:sleep/1, [4]),
       ?_assertFast(fun timer:sleep/1, [2]),
       %% long, because of first run
       ?_assertLong(fun timer:sleep/1, [5]),
       ?_assertFast(fun timer:sleep/1, [5]),
       %% long, because the value was removed from the storage
       ?_assertLong(fun timer:sleep/1, [3])
      ]}}.

timer_tc(FunObj, Args) ->
    {Micros, _Value} = timer:tc(fun do/2, [FunObj, Args]),
    Micros.

-endif.
