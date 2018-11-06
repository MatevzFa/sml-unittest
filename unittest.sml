
exception InternalAssert;

datatype assert_result = OK | FAIL | FAILIDX of int;


fun assert (e: bool) =
    if e then OK
    else FAIL;


fun assert_eq (e1: ''a, e2: ''a) =
    assert (e1 = e2);


fun raises (f: 'a -> 'b, args: 'a, e: exn) =
    (f args; FAIL)
    handle thrown_e =>
        assert_eq (exnName thrown_e, exnName e)


fun assert_all_i (nil, _)          = raise InternalAssert
|   assert_all_i (OK :: nil, i)    = OK
|   assert_all_i (FAIL :: nil, i)  = FAILIDX i
|   assert_all_i (OK :: tail, i)   = assert_all_i (tail, i + 1)
|   assert_all_i (FAIL :: tail, i) = FAILIDX i
|   assert_all_i (_, _)            = raise InternalAssert;


fun test (name: string, asserts: assert_result list) =
    case assert_all_i (asserts, 0) of
        OK        => (print ("OK\t" ^ name ^ "\n"); ())
    |   FAIL      => (print ("FAIL\t" ^ name ^ "\n"); ())
    |   FAILIDX i => (print ("FAIL\t" ^ name ^ " (at assert #" ^ Int.toString(i) ^ ")\n"); ());
