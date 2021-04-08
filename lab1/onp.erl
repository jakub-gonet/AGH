-module(onp).
-export([onp/1]).
% 1 + 2 * 3 - 4 / 5 + 6 = 12.2
% => 1 2 3 * + 4 5 / - 6 +

% 1 + 2 + 3 + 4 + 5 + 6 * 7 = 57
% => 1 2 3 4 5 6 7 * + + + + +

% ( (4 + 7) / 3 ) * (2 - 19) = -62.(3)
% => 4 7 + 3 / 2 19 - *

% 17 * (31 + 4) / ( (26 - 15) * 2 - 22 ) - 1 % => (26 - 15) * 2 - 22 = 0
% => 17 31 4 + * 26 15 - 2 * 22 - / 1 -
onp(S) -> 
  TokenizedStrings = string:lexemes(S, " "), % string:tokens/2 => deprecated
  % ParsedExpression = lists:map(fun parseNumber/1, TokenizedStrings), % => nie potrzebujemy parseNumbers
  ParsedExpression = parseNumbers(TokenizedStrings),
  evaluate(ParsedExpression).

parseNumbers([]) -> [];
parseNumbers([Token | T]) ->
  [parseNumber(Token) | parseNumbers(T)].

parseNumber(Token) ->
  parseNumberWith([fun string:to_float/1, fun string:to_integer/1], Token).

parseNumberWith([], Val) -> Val;
parseNumberWith([F | T], Val) ->
  case F(Val) of
    {error, _} -> parseNumberWith(T, Val);
    {ReturnVal, _} -> ReturnVal
  end.

evaluate(Expression) -> evaluate(Expression, []).
evaluate([], [Value]) -> Value;
evaluate(["+" | Expr], [B, A | Stack]) -> evaluate(Expr, [A + B | Stack]);
evaluate(["-" | Expr], [B, A | Stack]) -> evaluate(Expr, [A - B | Stack]);
evaluate(["/" | Expr], [B, A | Stack]) -> evaluate(Expr, [A / B | Stack]);
evaluate(["*" | Expr], [B, A | Stack]) -> evaluate(Expr, [A * B | Stack]);
evaluate(["pow" | Expr], [B, A | Stack]) -> evaluate(Expr, [math:pow(A, B) | Stack]);
evaluate(["sqrt" | Expr], [A | Stack]) -> evaluate(Expr, [math:sqrt(A) | Stack]);
evaluate(["sin" | Expr], [A | Stack]) -> evaluate(Expr, [math:sin(A) | Stack]);
evaluate(["cos" | Expr], [A | Stack]) -> evaluate(Expr, [math:cos(A) | Stack]);
evaluate(["tan" | Expr], [A | Stack]) -> evaluate(Expr, [math:tan(A) | Stack]);
evaluate([X | Expr], Stack) when is_number(X) -> evaluate(Expr, [X | Stack]). % czy powinnismy uzywac tutaj is_number?




