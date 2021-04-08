-module(erlmath).
-export([power/2]).

power(_, 0) -> 1;
power(A, B) -> A*power(A, B-1).
