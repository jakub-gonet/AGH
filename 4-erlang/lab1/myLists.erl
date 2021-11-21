-module(myLists).
-export([contains/2,
          duplicateElements/1,
          sumFloats/1,
          sumFloatsTailRec/1
        ]).

contains([], _) -> false;
contains([H | T], H) -> true;
contains([_ | T], Val) -> contains(T, Val).

duplicateElements([]) -> [];
duplicateElements([H | T]) -> [H, H | duplicateElements(T)].

sumFloats([]) -> 0;
sumFloats([H | T]) when is_float(H) -> H + sumFloats(T);
sumFloats([_ | T]) -> sumFloats(T).

sumFloatsTailRec(L) -> sumFloatsTailRecInt(L, 0).

sumFloatsTailRecInt([], Acc) -> Acc;
sumFloatsTailRecInt([H | T], Acc) when is_float(H) ->            sumFloatsTailRecInt(T, Acc + H);
sumFloatsTailRecInt([_ | T], Acc) -> 
  sumFloatsTailRecInt(T, Acc).
