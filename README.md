[![building the module for SWI Prolog](https://github.com/CodiePP/predidx/actions/workflows/make-swi.yml/badge.svg)](https://github.com/CodiePP/predidx/actions/workflows/make-swi.yml)

# Indexed Predicates

## Installation

```sh
aclocal --force && autoheader --force && autoconf --force
./configure
make swi
```

then, I copy the results to a module directory that SWI Prolog finds:
```sh
mkdir -v -p ~/lib/sbcl
cp -v predidx-* ~/lib/sbcl/predidx
cp -v src/predidx.qlf ~/lib/sbcl/
```

Update the swipl config file:

- SWI-Prolog version < 8.1.15<br>
(add the following to ~/.swiplrc)
```sh
echo ":- assertz(file_search_path(sbcl,'${HOME}/lib/sbcl'))." >> ${HOME}/.swiplrc
```
- SWI-Prolog version >= 8.1.15<br>
run the following from the terminal
```sh
echo ":- assertz(file_search_path(sbcl,'${HOME}/lib/sbcl'))." >> ${HOME}/.config/swi-prolog/init.pl
```

## Description

Prolog predicates in tables indexed by their row number. Behind the scenes
this is the same as an array of a structure type in `C`.

```prolog
tbl_create('table_name', [max_rows(1_000_000), realloc_rows(10_000)], Tid).
```

so, indexing from 0 to 999_999 is OK

```prolog
tbl_has(Tid, 42).
tbl_has(Tid, X). % enumerate
```

```prolog
tbl_get(Tid, 42, X).
```

```prolog
tbl_set(Tid, 42, 'something').
```

```prolog
tbl_unset(Tid, 42).
tbl_has(Tid, 42).  % => false
```

## example usage

```prolog
:- use_module(sbcl(predidx)).

t1 :-
  Tname = 'likes',
  tbl_create(Tname, [max_rows(10000),realloc_rows(100),structure(['int32'])], Tid),
  format("created table '~a' with id = ~p~n", [Tname, Tid]).
```

