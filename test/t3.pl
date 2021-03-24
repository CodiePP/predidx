
:- use_module(sbcl(predidx)).

one_of_10(X) :-
  member(X, [0,1,2,3,4,5,6,7,8,9]).
one_of_100(X) :-
  one_of_10(A),
  one_of_10(B),
  X is B * 10 + A.

enter_data(Xid) :-
  one_of_100(H),
  one_of_10(T),
  idx_add32(Xid, H, 4, T),
  fail.
enter_data(_).

test :-
  idx_create32(4, Xid),
  enter_data(Xid),
  idx_find32(Xid, 3, Count, Results),
  format("found ~p items: ~p~n", [Count,Results]),
  idx_destroy32(Xid).

