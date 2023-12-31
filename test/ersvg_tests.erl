-module(ersvg_tests). 
-import(ersvg,[version/0,svg_to_png/1]).
-include_lib("eunit/include/eunit.hrl").

version_test() ->
	?assertEqual(true,is_binary(version())).

svg_to_png_0_test() ->
  Data = <<"<svg width='100' height='100' xmlns='http://www.w3.org/2000/svg'><rect x='10' y='20' width='80' height='50' fill='black'/></svg>">>,
	?assertEqual(true,is_binary(svg_to_png(Data))).
