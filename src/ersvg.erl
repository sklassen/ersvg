%%%-------------------------------------------------------------------
%% @doc ersvg public API
%% @end
%%%-------------------------------------------------------------------

-module(ersvg).

-include_lib("kernel/include/logger.hrl").

-export([version/0,svg_to_png/1,svg_to_png/2]).

version() ->
    Bin=exec(<<"priv/bin/resvg --version">>),
    binary:part(Bin,0,byte_size(Bin)-1).

svg_to_png(Data) ->
  svg_to_png(Data,#{}).

svg_to_png(Binary,#{}) when is_binary(Binary)->
    exec(<<"echo \"",Binary/binary,"\" | priv/bin/resvg - -c">>);
    %exec(<<"priv/bin/resvg - -c --resources-dir .">>,Binary);
    %exec(<<"/usr/bin/tee /tmp/pipe.log">>,Binary);
svg_to_png(List,Options) when is_list(List)->
    svg_to_png(list_to_binary(List),Options).
    
exec(Command) ->
  exec(Command,<<>>).

exec(Command,StdIn) ->
    ?LOG_DEBUG("ersvg:~p~n",[Command]),
    sanitize(Command),
    Port = erlang:open_port({spawn, Command}, [binary, eof, use_stdio, exit_status, hide, stream]),
    %EOT = <<$\>>,
    %?LOG_DEBUG("EOT ~p~n",[<<EOT/binary>>]),
    %erlang:port_command(Port, StdIn),
    %erlang:port_command(Port, [$\n]),
    %erlang:port_command(Port, <<4>>),
    %erlang:port_command(Port, EOT),
    get_data(Port).

sanitize(Binary)->
  case binary:match(Binary,[<<"`">>]) of
    nomatch -> ok;
    _ -> erlang:error(illegal)
  end.

get_data(Port) ->
  case get_data(Port, <<>>) of
    {0,Success} -> Success;
    {N,Error} -> erlang:error({N,Error})
  end.

get_data(Port, Buffer) ->
    receive
    {Port, {data, Bytes}} ->
        get_data(Port, <<Buffer/binary,Bytes/binary>>);
    {Port, eof} ->
        Port ! {self(), close},
        receive
        {Port, closed} ->
            true
        end,
        receive
        {'EXIT',  Port,  _} ->
            ok
        after 1 -> 
            ok
        end,
        ExitCode = receive {Port, {exit_status, Code}} -> Code end,
        {ExitCode, Buffer}
     %Unknown -> erlang:error({-1,Unknown}) 
    end.
