-module(meth).

-export([
         is_non_neg_integer/1,

         float_to_integer_if_possible/1,

         pow/2,
         pow_int/2,

         gcd/2
        ]).

%% @doc 自然数かどうかを返す.
-spec is_non_neg_integer(non_neg_integer()) -> true
    ;                   (neg_integer())     -> false.
is_non_neg_integer(NonNeg) when NonNeg >= 0 ->
    true;
is_non_neg_integer(_) ->
    false.

%% @doc 浮動小数点数の小数部が0ならば整数を返す.
-spec float_to_integer_if_possible(float()) -> number().
float_to_integer_if_possible(Float) when trunc(Float) == Float ->
    trunc(Float);
float_to_integer_if_possible(Float) ->
    Float.

%% @doc 冪乗.
%%
%% `math:pow/2' と違い, 計算結果が整数値の場合は, `integer()' を返す.
%% FIXME: `pow(0.5, -1)' のようなときに `integer()' を返す.
-spec pow(Base::integer(), Exponent::non_neg_integer()) -> integer()
    ;    (Base::integer(), Exponent::neg_integer())     -> float()
    ;    (Base::float(),   Exponent::number())          -> number().
pow(Base, Exponent) when is_float(Base), trunc(Base) == Base ->
    pow(trunc(Base), Exponent);
pow(Base, Exponent) when is_float(Exponent), trunc(Exponent) == Exponent ->
    pow(Base, trunc(Exponent));
pow(Base, Exponent) when is_integer(Base), is_integer(Exponent), Exponent >= 0 ->
    pow_int(Base, Exponent);
pow(Base, Exponent) ->
    math:pow(Base, Exponent).

%% @doc 結果が整数になるもののみを計算できる冪乗.
%%
%% 条件としては, Baseが `integer()' , Exponentが `non_neg_integer()' のみを扱う.
-spec pow_int(Base::integer(), Exponent::non_neg_integer()) -> integer().
pow_int(Base, Exponent) ->
    pow_int(Base, Exponent, 1).

pow_int(_, 0, C) ->
    C;
pow_int(Base, Exponent, C) when Exponent > 0 ->
    pow_int(Base * Base, Exponent div 2, case Exponent rem 2 of 0 -> C; 1 -> Base * C end).

%% @doc 最大公約数.
-spec gcd(A::integer(), B::integer()) -> GCD::integer().
gcd(A, 0) ->
    abs(A);
gcd(A, B) ->
    gcd(B, A rem B).
