-module(collector).
-behaviour(gen_server).

-export([start_link/0, add_metric/1, get_metrics/0]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_metrics() -> gen_server:call(?MODULE, get_metrics).

add_metric(Metric) -> 
  gen_server:cast(?MODULE, {metric, Metric}).

timestamp() ->
    erlang:system_time(millisecond).

init(_) ->
    application:ensure_all_started(jsx),

    ets:new(metrics, [named_table, public, {keypos, 1}]),
    {ok, listen_tcp()}.

handle_cast({metric, Metric}, State) ->
    io:format("Storing metric: ~p~n", [Metric]),
    ets:insert(metrics, {timestamp(), Metric}),
    {noreply, State}.

handle_call(get_metrics, _From, State) ->
    Records = ets:tab2list(metrics),
    {reply, Records, State}.

listen_tcp() ->
    {ok, Listen} = gen_tcp:listen(9000, [binary, {packet, 0}, {active, false}]),
    spawn(fun() -> accept_connection(Listen) end).

accept_connection(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> handle_client(Socket) end),
    accept_connection(Listen).

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, <<"get_metrics">>} ->
            Metrics = gen_server:call(?MODULE, get_metrics),
            Json = case Metrics of
                [] ->
                    <<"[{}]">>;
                _ ->
                    jsx:encode([#{
                        <<"cpu_usage">> => Cpu,
                        <<"mem_usage">> => Mem,
                        <<"disk_read">> => DiskRead,
                        <<"disk_write">> => DiskWrite
                    } || {_Ts, {Cpu, Mem, DiskRead, DiskWrite}} <- Metrics])
            end,
            gen_tcp:send(Socket, Json),
            gen_tcp:close(Socket);

        {ok, Data} ->
            io:format("Received data: ~p~n", [Data]),
            try
                JsonMap = jsx:decode(Data, [return_maps]),
                io:format("Parsed JSON: ~p~n", [JsonMap]),

                CpuUsage = get_number(<<"cpu_usage">>, JsonMap),
                MemUsage = get_number(<<"mem_usage">>, JsonMap),
                DiskRead = get_number(<<"disk_read">>, JsonMap),
                DiskWrite = get_number(<<"disk_write">>, JsonMap),

                io:format("Extracted values - CPU: ~p, MEM: ~p, DiskRead: ~p, DiskWrite: ~p~n",
                         [CpuUsage, MemUsage, DiskRead, DiskWrite]),

                Metric = {CpuUsage, MemUsage, DiskRead, DiskWrite},
                add_metric(Metric),

                handle_client(Socket)
            catch
                E:R:S ->
                    io:format("Error processing: ~p:~p~n~p~n", [E, R, S]),
                    handle_client(Socket)
            end;

        {error, closed} ->
            io:format("Connection closed~n");
        
        {error, Reason} ->
            io:format("TCP recv error: ~p~n", Reason)

    end.
        

get_number(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, null} -> 0.0;
        {ok, Value} when is_number(Value) -> Value;
        _ -> 0.0
    end.
