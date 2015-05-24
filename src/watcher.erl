-module(watcher).

-behaviour(gen_server).

-export([run/1, run/2]).
-export([stop/1]).

-export([start_link/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
  pid :: pid(),
  name :: term(),
  item_list :: [atom()],
  interval :: non_neg_integer()
}).

-spec run([atom()]) -> {ok, pid()} | term().
run(ItemList) ->
  run(ItemList, #{}).

-spec run([atom()], map()) -> {ok, pid()} | term().
run(ItemList, Opts) ->
  case check_opts(maps:to_list(Opts)) of
    ok ->
      case supervisor:start_child(watcher_sup, [self(), ItemList, Opts]) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
          Error
      end;
    Error ->
      Error
  end.

%% TODO
stop(_) ->
  ok.


check_opts(_) ->
  ok.

start_link(Pid, ItemList, Opts) ->
  gen_server:start_link(?MODULE, [Pid, ItemList, Opts], []).

init([Pid, ItemList, Opts]) ->
  Name = maps:get(name, Opts, Pid),
  Interval = maps:get(interval, Opts, 1000),
  {ok, #state{pid = Pid, name = Name, item_list = ItemList, interval = Interval}, Interval}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, #state{pid = Pid, name = Name, item_list = ItemList, interval = Interval} = State) ->
  case erlang:process_info(Pid, ItemList) of
    undefined ->
      {stop, normal, State};
    InfoTupleList ->
      ok = print(Name, InfoTupleList),
      {noreply, State, Interval}
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

print(Name, InfoTupleList) ->
  Label = io_lib:format("~p:~n", [Name]),
  Lines = [ format(InfoTuple) ++ "\n" || InfoTuple <- InfoTupleList ],
  io:put_chars([Label|Lines]).

format({message_queue_len, L}) ->
  io_lib:format("  message_queue_len: ~p", [L]);
%% TODO
format(_) ->
  "".
