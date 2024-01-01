%%%-------------------------------------------------------------------
%% @doc ersvg public API
%% @end
%%%-------------------------------------------------------------------

-module(ersvg).

-include_lib("kernel/include/logger.hrl").

-export([version/0,svg_to_png/1,svg_to_png/2]).
-export_type([options/0]).

-type options() :: #{}.

version() ->
    Bin=exec(<<"priv/bin/resvg --version">>),
    binary:part(Bin,0,byte_size(Bin)-1).

svg_to_png(Data) ->
  svg_to_png(Data,#{}).

-spec svg_to_png(iodata(), options()) -> binary().
svg_to_png(Binary,#{}) when is_binary(Binary)->
    exec(<<"priv/bin/resvg - -c --resources-dir .">>,Binary);
svg_to_png(List,Options) when is_list(List)->
    svg_to_png(list_to_binary(List),Options).

-spec exec(unicode:unicode_binary()) -> binary().
exec(Command) ->
  exec(Command,<<>>).

-spec exec(unicode:unicode_binary(), unicode:unicode_binary()) -> binary().
exec(BaseCommand,StdIn) ->
    QuotedStdIn = quote(StdIn),
    Command = <<"echo ", QuotedStdIn/binary, " | ", BaseCommand/binary>>,
    ?LOG_DEBUG("ersvg:~p~n",[Command]),
    Port = erlang:open_port({spawn, Command}, [binary, eof, use_stdio, exit_status, hide, stream]),
    get_data(Port).

-spec quote(unicode:unicode_binary()) -> unicode:unicode_binary().
quote(Binary) when is_binary(Binary) ->
    sanitize(Binary),
    Sanitized = binary:replace(Binary, <<"'">>, <<"'\"'\"'">>, [global]),
    <<"'", Sanitized/binary, "'">>;
quote(Arg1) ->
    erlang:error(badarg, [Arg1]).

-spec sanitize(unicode:unicode_binary()) -> ok.
sanitize(Binary)->
  case binary:match(Binary,[<<"\0">>]) of
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
