%% coding: latin-1
-module(meth_tests).

-include_lib("eunit/include/eunit.hrl").

is_non_neg_integer_test_() ->
    [
     {"正の整数", ?_assert(meth:is_non_neg_integer(1))},
     {"0",        ?_assert(meth:is_non_neg_integer(0))},
     {"負の整数", ?_assertNot(meth:is_non_neg_integer(-1))}
    ].

float_to_integer_if_possible_test_() ->
    [
     {"結果がinteger()になる",
      [
       ?_assertEqual(1, meth:float_to_integer_if_possible(1.0)),
       ?_assertEqual(2, meth:float_to_integer_if_possible(2.0)),
       ?_assertEqual(3, meth:float_to_integer_if_possible(3.0)),
       ?_assertEqual(4, meth:float_to_integer_if_possible(4.0))
      ]},

     {"結果がfloat()のまま",
      [
       ?_assertEqual(2.9, meth:float_to_integer_if_possible(2.9)),
       ?_assertEqual(2.8, meth:float_to_integer_if_possible(2.8)),
       ?_assertEqual(2.7, meth:float_to_integer_if_possible(2.7)),
       ?_assertEqual(2.6, meth:float_to_integer_if_possible(2.6))
      ]}
    ].

pow_test_() ->
    [
     {"integer()を返す", ?_assertEqual(9, meth:pow(3, 2))},

     {"底, 指数の小数部が0の場合に整数が変える",
      ?_assertEqual(9, meth:pow(3.0, 2.0))},
     {"底の小数部が0で指数の小数部が0でない",
      ?_assertEqual(math:pow(3.0, 2.5), meth:pow(3.0, 2.5))},
     {"指数の小数部が0で底の小数部が0でない",
      ?_assertEqual(math:pow(2.5, 3.0), meth:pow(2.5, 3.0))},

     {"底が整数で指数が負の整数", ?_assertEqual(0.5, meth:pow(2, -1))},
     {"底が浮動小数点数で指数が整数", ?_assertEqual(2.0, meth:pow(0.5, -1))},
     {"底が数で指数が浮動小数点数",
      ?_assertEqual(math:pow(0.5, 0.5), meth:pow(0.5, 0.5))}
    ].

pow_int_test_() ->
    [
     {"底が0",                    ?_assertEqual(0,   meth:pow_int(0,  4))},
     {"指数が0",                  ?_assertEqual(1,   meth:pow_int(9,  0))},
     {"底が自然数で指数が偶数",   ?_assertEqual(25,  meth:pow_int(5,  2))},
     {"底が自然数で指数が奇数",   ?_assertEqual(27,  meth:pow_int(3,  3))},
     {"底が負の整数で指数が偶数", ?_assertEqual(49,  meth:pow_int(-7, 2))},
     {"底が負の整数で指数が奇数", ?_assertEqual(-64, meth:pow_int(-4, 3))},

     {"指数が適当に大きな数(2#11001)",
      ?_assertEqual(-847288609443, meth:pow_int(-3, 25))}
    ].

gcd_test_() ->
    [
     {"第1引数が0の場合",        ?_assertEqual(2, meth:gcd(0, 2))},
     {"第2引数が0の場合",        ?_assertEqual(2, meth:gcd(2, 0))},
     {"第1引数の方が小さい場合", ?_assertEqual(3, meth:gcd(6, 9))},
     {"第2引数の方が小さい場合", ?_assertEqual(3, meth:gcd(9, 6))}
    ].
