%% coding: latin-1
-module(meth_tests).

-include_lib("eunit/include/eunit.hrl").

pow_test_() ->
    [
     {"底が0",                    ?_assertEqual(0,   meth:pow(0,  4))},
     {"指数が0",                  ?_assertEqual(1,   meth:pow(9,  0))},
     {"底が自然数で指数が偶数",   ?_assertEqual(25,  meth:pow(5,  2))},
     {"底が自然数で指数が奇数",   ?_assertEqual(27,  meth:pow(3,  3))},
     {"底が負の整数で指数が偶数", ?_assertEqual(49,  meth:pow(-7, 2))},
     {"底が負の整数で指数が奇数", ?_assertEqual(-64, meth:pow(-4, 3))},

     {"指数が適当に大きな数(2#11001)",
      ?_assertEqual(-847288609443, meth:pow(-3, 25))},

     {"底, 指数の小数部が0の場合に整数が変える",
      ?_assertEqual(9, meth:pow(3.0, 2.0))},

     {"底が整数で指数が負の整数", ?_assertEqual(0.5, meth:pow(2, -1))},
     {"底が浮動小数点数で指数が整数", ?_assertEqual(2.0, meth:pow(0.5, -1))},
     {"底が数で指数が浮動小数点数",
      ?_assertEqual(0.7071067811865476, meth:pow(0.5, 0.5))}
    ].
