
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
  tbl_set(Tid, 13, [1, 987654321]),

  findall((Idx, El), (tbl_find(Tid,0,1,Idx), tbl_get(Tid,Idx,El)),
    [(10, [1, 1234567890]),  (13, [1, 987654321])]),

  tbl_unset(Tid, 11),

  findall((Idx, El), (tbl_find(Tid,0,1,Idx), tbl_get(Tid,Idx,El)),
    [(10, [1, 1234567890]),  (13, [1, 987654321])]),

  findall((Idx, El), (tbl_find(Tid,1,30949376,Idx), tbl_get(Tid,Idx,El)),
    []),
  findall((Idx, El), (tbl_find(Tid,1,987654321,Idx), tbl_get(Tid,Idx,El)),
    [(12, [11, 987654321]),  (13, [1, 987654321])]),

  format("success.~n",[]).
run_test.
