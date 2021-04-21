-define(assertWithDelta(V, Assertion),
    ?assertWithDelta(
        V,
        Assertion,
        1.0e-7
    )
).

-define(assertWithDelta(V, Assertion, Delta), ?assertMatch(X when abs(X - V) < Delta, Assertion)).
