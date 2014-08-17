-module(meth).

-export([
         decimal_part/1,

         float_to_integer_if_possible/1,

         pow/2,
         pow_int/2
        ]).

%% @doc 小数部を返す.
-spec decimal_part(float()) -> float().
decimal_part(Float) ->
    Float - trunc(Float).

%% @doc 浮動小数点数の小数部が0ならば整数を返す.
-spec float_to_integer_if_possible(float()) -> number().
float_to_integer_if_possible(Float) ->
    case decimal_part(Float) == 0 of
        true ->
            trunc(Float);
        false ->
            Float
    end.

%% @doc 冪乗.
%%
%% `math:pow/2' と違い, 計算結果が整数値の場合は, `integer()' を返す.
%% FIXME: `pow(0.5, -1)' のようなときに `integer()' を返す.
%% FIXME: `pow(5.0, 2.0)' のようなときに `integer()' を返す.
-spec pow(Base::integer(), Exponent::non_neg_integer()) -> integer()
    ;    (Base::integer(), Exponent::neg_integer())     -> float()
    ;    (Base::float(),   Exponent::number())          -> number().
pow(Base0, Exponent0) ->
    Base = float_to_integer_if_possible(Base0),
    Exponent = float_to_integer_if_possible(Exponent0),

    case is_integer(Base) andalso is_integer(Exponent) andalso Exponent >= 0 of
        true ->
            pow_int(Base, Exponent);
        false ->
            math:pow(Base, Exponent)
    end.

%% @doc 結果が整数になるもののみを計算できる冪乗.
%%
%% FIXME: `pow(0.5, -1)' のようなときに `integer()' にする.
-spec pow_int(Base::integer(), Exponent::non_neg_integer()) -> integer().
pow_int(Base, Exponent) ->
    pow_int(Base, Exponent, 1).

pow_int(_, 0, C) ->
    C;
pow_int(Base, Exponent, C) ->
    pow_int(Base * Base, Exponent div 2, case Exponent rem 2 of  0 -> C; 1 -> Base * C end).
