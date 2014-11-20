%%% @doc
%%% Application main supervisor.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 20 Nov 2014
%%% @copyright 2014, Aleksey Morarash <aleksey.morarash@gmail.com>

-module(memoiz_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("memoiz.hrl").

%% ----------------------------------------------------------------------
%% API functions
%% ----------------------------------------------------------------------

%% @doc Starts supervisor as part of a supervision tree.
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, undefined).

%% @hidden
%% @doc Calls initialization procedures and return child workers spec.
-spec init(Args :: any()) ->
                  {ok,
                   {{RestartStrategy :: supervisor:strategy(),
                     MaxR :: non_neg_integer(),
                     MaxT :: non_neg_integer()},
                    [ChildSpec :: supervisor:child_spec()]}}.
init(_Args) ->
    {ok, {
       {one_for_one, 5, 1},
       [
        %% main activity
        {memoiz_srv, {memoiz_srv, start_link, []},
         permanent, 100, supervisor, [memoiz_srv]}
       ]}}.
