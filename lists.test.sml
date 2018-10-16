use "unittest.sml";

use "lists.sml";

val _ = (

    test("length", [
        assert_eq(length [], 0),
        assert_eq(length([1]), 1),
        assert_eq(length([1,1]), 2),
        assert_eq(length(["a", "b"]), 2)
    ]);

    test("concatenate", [
        assert_eq(concatenate([1,2,3,4], []), [1,2,3,4]),
        assert_eq(concatenate([], [1,2,3,4]), [1,2,3,4]),
        assert_eq(concatenate([1,2,3,4], [1,2,3,4]), [1,2,3,4,1,2,3,4])
    ]);

    test("reals", [
        assert(Real.==(1.1, 1.1)),
        assert(not(Real.==(1.2, 1.1)))
    ]);

    OS.Process.exit(OS.Process.success)
);
