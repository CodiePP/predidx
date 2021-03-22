
:- use_module(sbcl(predidx)).

test :-
  Tname = 'has_some',
  tbl_create(Tname, [max_rows(10000),realloc_rows(100),structure(['int32','uint64'])], Tid), !,
  format("created table '~a' with id = ~p~n", [Tname, Tid]),

  tbl_set(0, 12, [11, 987654321]),
  tbl_set(0, 10, [1, 1234567890]),
  tbl_set(0, 11, [10, 30949376]),
  tbl_set(0, 13, [1, 987654321]),

  findall((Idx, El), (tbl_find(0,0,1,Idx), tbl_get(0,Idx,El)),
    [(10, [1, 1234567890]),  (13, [1, 987654321])]),

  tbl_unset(0, 11),

  findall((Idx, El), (tbl_find(0,0,1,Idx), tbl_get(0,Idx,El)),
    [(10, [1, 1234567890]),  (13, [1, 987654321])]),

  findall((Idx, El), (tbl_find(0,1,30949376,Idx), tbl_get(0,Idx,El)),
    []),
  findall((Idx, El), (tbl_find(0,1,987654321,Idx), tbl_get(0,Idx,El)),
    [(12, [11, 987654321]),  (13, [1, 987654321])]),

  format("success.~n",[]).

