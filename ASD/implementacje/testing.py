def test(function, args_tuple):
    for args, expected in args_tuple:
        try:
            assert function(*args) == expected
            print(".", end="")
        except AssertionError:
            print(
                f"{function.__name__}({', '.join((str(x) for x in args))}), expected {expected}, got {function(*args)}"
            )
    print("")
