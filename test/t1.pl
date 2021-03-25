
:- use_module(sbcl(predidx)).

test :-
  run_test,
  halt.

run_test :-
  Tname = 'has_some',
  tbl_create(Tname, [max_rows(10000),realloc_rows(100),structure(['int32','uint64'])], Tid), !,
  format("created table '~a' with id = ~p~n", [Tname, Tid]),

  tbl_set(Tid, 12, [11, 987654321]),
  tbl_set(Tid, 10, [1, 1234567890]),
  tbl_set(Tid, 11, [10, 30949376]),

  findall((Idx, El), (tbl_has(Tid,Idx), tbl_get(Tid,Idx,El)),
    [(10, [1, 1234567890]),  (11, [10, 30949376]),  (12, [11, 987654321])]),

  tbl_unset(Tid, 11),

  findall((Idx, El), (tbl_has(Tid,Idx), tbl_get(Tid,Idx,El)),
    [(10, [1, 1234567890]),  (12, [11, 987654321])]),

  format("success.~n",[]).
run_test.
