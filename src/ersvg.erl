%%%-------------------------------------------------------------------
%% @doc ersvg public API
%% @end
%%%-------------------------------------------------------------------

-module(ersvg).

-export([version/0,svg_to_png/1,svg_to_png/2]).

version() ->
    Bin=exec(<<"priv/bin/resvg --version">>),
    binary:part(Bin,0,byte_size(Bin)-1).

svg_to_png(Data) ->
  svg_to_png(Data,#{}).

svg_to_png(Binary,#{}) when is_binary(Binary)->
    exec(<<"echo \"",Binary/binary,"\" | priv/bin/resvg - -c">>);
svg_to_png(List,Options) when is_list(List)->
    svg_to_png(list_to_binary(List),Options).
    

exec(Command) ->
    io:format("ersvg:~p~n",[Command]),
    get_data(open_port({spawn, Command}, [stream, in, eof, hide, exit_status])).

get_data(Port) ->
  case get_data(Port, []) of
    {0,Success} -> list_to_binary(Success);
    {N,Error} -> erlang:error({N,Error})
  end.
get_data(Port, Sofar) ->
    receive
    {Port, {data, Bytes}} ->
        get_data(Port, [Sofar|Bytes]);
    {Port, eof} ->
        Port ! {self(), close},
        receive
        {Port, closed} ->
            true
        end,
        receive
        {'EXIT',  Port,  _} ->
            ok
        after 1 ->              % force context switch
            ok
        end,
        ExitCode =
            receive
            {Port, {exit_status, Code}} ->
                Code
        end,
        {ExitCode, lists:flatten(Sofar)}
    end.
