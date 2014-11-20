%%% @doc
%%% Handles a TCP connection.

%%% @author Aleksey Morarash <aleksey.morarash@gmail.com>
%%% @since 20 Nov 2014
%%% @copyright 2014, Aleksey Morarash <aleksey.morarash@gmail.com>

-module(memoiz_srv).

-behaviour(gen_server).

%% API exports
-export(
   [start_link/0,
    store/2,
    lookup/1
   ]).

%% gen_server callback exports
-export(
   [init/1, handle_call/3, handle_info/2, handle_cast/2,
    terminate/2, code_change/3]).

-include("memoiz.hrl").

%% --------------------------------------------------------------------
%% Data type definitions
%% --------------------------------------------------------------------

-type key() :: any().

-type value() :: any().

-type max_size() ::
        pos_integer() | infinity.

-type max_mem() ::
        pos_integer() | infinity.

-define(STORE(Key, Value), {store, Key, Value}).

-define(TOUCH(Key), {touch, Key}).

%% ETS table name: access times for the records
-define(ATIME1, memoiz_srv_atime1).
-define(ATIME2, memoiz_srv_atime2).

%% --------------------------------------------------------------------
%% API functions
%% --------------------------------------------------------------------

%% @doc Start the process as part of the supervision tree.
-spec start_link() -> {ok, pid()} | ignore | {error, Reason :: any()}.
start_link() ->
    gen_server:start_link(
      {local, ?MODULE}, ?MODULE, _Args = undefined, _Options = []).

%% @doc Store new item in the ETS table.
-spec store(Key :: key(), Value :: value()) -> ok.
store(Key, Value) ->
    ok = gen_server:cast(?MODULE, ?STORE(Key, Value)).

%% @doc Lookup a value through the table.
-spec lookup(Key :: key()) ->
                    {ok, Value :: value()} |
                    undefined.
lookup(Key) ->
    case ets:lookup(?MODULE, Key) of
        [{Key, Value}] ->
            ok = gen_server:cast(?MODULE, ?TOUCH(Key)),
            {ok, Value};
        [] ->
            undefined
    end.

%% --------------------------------------------------------------------
%% gen_server callback functions
%% --------------------------------------------------------------------

-record(
   state,
   {max_size = infinity :: max_size(),
    max_mem = infinity :: max_mem(),
    cur_size = 0 :: non_neg_integer(),
    cur_mem = 0 :: non_neg_integer()
   }).

%% @hidden
-spec init(Args :: any()) ->
                  {ok, InitialState :: #state{}}.
init(_Args) ->
    %% Function key to function value lookup table
    ?MODULE = ets:new(?MODULE, [named_table]),
    %% Function key to time lookup table
    ?ATIME1 = ets:new(?ATIME1, [named_table]),
    %% Time to function key lookup table
    ?ATIME2 = ets:new(?ATIME2, [named_table, ordered_set]),
    MaxSize =
        case application:get_env(max_size) of
            {ok, MaxSize0} when is_integer(MaxSize0), MaxSize0 > 0 ->
                MaxSize0;
            undefined ->
                infinity
        end,
    MaxMem =
        case application:get_env(max_mem) of
            {ok, MaxMem0} when is_integer(MaxMem0), MaxMem0 > 0 ->
                MaxMem0;
            undefined ->
                infinity
        end,
    {ok, #state{max_size = MaxSize,
                max_mem = MaxMem}}.

%% @hidden
-spec handle_info(Request :: any(), State :: #state{}) ->
                         {noreply, State :: #state{}}.
handle_info(_Request, State) ->
    {noreply, State}.

%% @hidden
-spec handle_cast(Request :: any(), State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_cast(?STORE(Key, Value), State) ->
    ok = vacuum(
          State#state.max_size,
          State#state.max_mem,
          State#state.cur_size,
          State#state.cur_mem),
    true = ets:insert(?MODULE, {Key, Value}),
    NewTime = next_time(),
    true = ets:insert(?ATIME1, {Key, NewTime}),
    true = ets:insert(?ATIME2, {NewTime, Key}),
    {NewSize, NewMem} = stats(),
    {noreply, State#state{cur_size = NewSize,
                          cur_mem = NewMem}};
handle_cast(?TOUCH(Key), State) ->
    ok = touch(Key),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden
-spec handle_call(Request :: any(), From :: any(), State :: #state{}) ->
                         {noreply, NewState :: #state{}}.
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @hidden
-spec terminate(Reason :: any(), State :: #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @hidden
-spec code_change(OldVersion :: any(), State :: #state{}, Extra :: any()) ->
                         {ok, NewState :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------

%% @doc Return main table size and heap size.
-spec stats() -> {EtsSize :: non_neg_integer(),
                  EtsHeapSize :: non_neg_integer()}.
stats() ->
    {ets:info(?MODULE, size),
     (ets:info(?MODULE, memory) +
          ets:info(?ATIME1, memory) +
          ets:info(?ATIME2, memory)) * erlang:system_info(wordsize)}.

%% @doc Remove the less recent accessed record from the table.
-spec vacuum(MaxSize :: max_size(),
             MaxMem :: max_mem(),
             CurSize :: non_neg_integer(),
             CurMem :: non_neg_integer()) -> ok.
vacuum(_MaxSize, _MaxMem, 0 = _CurSize, _CurMem) ->
    %% table is already empty, nothing to delete
    ok;
vacuum(MaxSize, MaxMem, CurSize, _CurMem)
  when MaxSize /= infinity, CurSize >= MaxSize ->
    ok = remove_less_recent(),
    {NewSize, NewMem} = stats(),
    vacuum(MaxSize, MaxMem, NewSize, NewMem);
vacuum(MaxSize, MaxMem, _CurSize, CurMem)
  when MaxMem /= infinity, CurMem >= MaxMem ->
    ok = remove_less_recent(),
    {NewSize, NewMem} = stats(),
    vacuum(MaxSize, MaxMem, NewSize, NewMem);
vacuum(_MaxSize, _MaxMem, _CurSize, _CurMem) ->
    ok.

%% @doc Remove the less recent record from the table.
-spec remove_less_recent() -> ok.
remove_less_recent() ->
    case ets:first(?ATIME2) of
        '$end_of_table' ->
            %% table is already empty, nothing to do
            ok;
        Time ->
            [{_, Key}] = ets:lookup(?ATIME2, Time),
            true = ets:delete(?MODULE, Key),
            true = ets:delete(?ATIME1, Key),
            true = ets:delete(?ATIME2, Time),
            ok
    end.

%% @doc Update item access time.
-spec touch(Key :: key()) -> ok.
touch(Key) ->
    case ets:lookup(?ATIME1, Key) of
        [{_, OldTime}] ->
            NewTime = next_time(),
            true = ets:delete(?ATIME2, OldTime),
            true = ets:insert(?ATIME2, {NewTime, Key}),
            true = ets:insert(?ATIME1, {Key, NewTime}),
            ok;
        [] ->
            ok
    end.

%% @doc Return next value for autoincrementing counter.
-spec next_time() -> non_neg_integer().
next_time() ->
    Key = counter,
    Value =
        case get(Key) of
            undefined ->
                0;
            Int when is_integer(Int) ->
                Int
        end,
    put(Key, Value + 1),
    Value.
