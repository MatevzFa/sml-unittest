
exception InternalAssert;

datatype assert_result = EMPTY | OK | FAIL | FAILIDX of int | FAILEXCEPT of exn * int;


fun assert (e: bool) =
    if e then OK
    else FAIL;

fun assert_eq (e1: ''a, e2: ''a) =
    assert (e1 = e2);

fun assert_equal (f: 'a -> ''b, args: 'a, e: ''b) =
    (assert (f (args) = e))
    handle throw_e => FAILEXCEPT (throw_e, ~1);

fun raises (f: 'a -> 'b, args: 'a, e: exn) =
    (f args; FAIL)
    handle thrown_e =>
        assert_eq (exnName thrown_e, exnName e)

fun assert_all_i (nil, _)          = EMPTY
|   assert_all_i (OK :: nil, _)    = OK
|   assert_all_i (FAIL :: nil, i)  = FAILIDX i
|   assert_all_i (OK :: tail, i)   = assert_all_i (tail, i + 1)
|   assert_all_i (FAIL :: tail, i) = FAILIDX i
|   assert_all_i (FAILEXCEPT (e, _) :: _, i) = FAILEXCEPT (e, i)
|   assert_all_i (_, _)            = raise InternalAssert;

fun test (name: string, asserts: assert_result list) =
    case assert_all_i (asserts, 0) of
        OK        => (print ("OK\t" ^ name ^ "\n"); ())
    |   EMPTY     => (print ("EMPTY\t" ^ name ^ "\n"); ())
    |   FAIL      => (print ("FAIL\t" ^ name ^ "\n"); ())
    |   FAILIDX i => (print ("FAIL\t" ^ name ^ " (at assert #" ^ Int.toString(i) ^ ")\n"); ())
    |   FAILEXCEPT (ex, i) => (print("FAIL \t" ^ name ^ " (exception: " ^ exnName ex ^ " at #" ^ Int.toString(i) ^ ")\n"); ());
