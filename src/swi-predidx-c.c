
#include <stdio.h>
#include <string.h>

#include "predidx-c.h"


#include <SWI-Prolog.h>

/* declarations */
foreign_t swi_tbl_create(term_t tblname, term_t maxrows, term_t allocrows,
                         term_t strx, term_t ret_tbl_id);
foreign_t swi_tbl_has_idx(term_t tbl_id, term_t idx);
foreign_t swi_tbl_next_idx(term_t tbl_id, term_t idx, control_t handle);
foreign_t swi_tbl_get_idx_value(term_t tbl_id, term_t idx, term_t values);
foreign_t swi_tbl_set_idx_value(term_t tbl_id, term_t idx, term_t values);
foreign_t swi_tbl_invalidate_idx(term_t tbl_id, term_t idx);
foreign_t swi_tbl_info(term_t tbl_id, term_t desc);
foreign_t swi_tbl_find_idx(term_t in_tblid, term_t in_ncol, term_t in_value, term_t out_idx, control_t handle);
foreign_t swi_idx_create32(term_t in_dlen, term_t out_xid);
foreign_t swi_idx_destroy32(term_t in_xid);
foreign_t swi_idx_add32(term_t in_xid, term_t in_key, term_t in_dlen, term_t in_value);
foreign_t swi_idx_find32(term_t in_xid, term_t in_key, term_t in_dlen, term_t out_count, term_t out_results);
foreign_t swi_idx_create64(term_t in_dlen, term_t out_xid);
foreign_t swi_idx_destroy64(term_t in_xid);
foreign_t swi_idx_add64(term_t in_xid, term_t in_key, term_t in_dlen, term_t in_value);
foreign_t swi_idx_find64(term_t in_xid, term_t in_key, term_t in_dlen, term_t out_count, term_t out_results);


install_t install()
{
        PL_register_foreign("pl_tbl_info", 2, swi_tbl_info, 0);
        PL_register_foreign("pl_tbl_create", 5, swi_tbl_create, 0);
        PL_register_foreign("pl_tbl_has_idx", 2, swi_tbl_has_idx, 0);
        PL_register_foreign("pl_tbl_next_idx", 2, swi_tbl_next_idx, PL_FA_NONDETERMINISTIC);
        PL_register_foreign("pl_tbl_get_idx_value", 3, swi_tbl_get_idx_value, 0);
        PL_register_foreign("pl_tbl_set_idx_value", 3, swi_tbl_set_idx_value, 0);
        PL_register_foreign("pl_tbl_invalidate_idx", 2, swi_tbl_invalidate_idx, 0);
        PL_register_foreign("pl_tbl_find_idx", 4, swi_tbl_find_idx, PL_FA_NONDETERMINISTIC);
        PL_register_foreign("pl_idx_create32", 2, swi_idx_create32, 0);
        PL_register_foreign("pl_idx_destroy32", 1, swi_idx_destroy32, 0);
        PL_register_foreign("pl_idx_add32", 4, swi_idx_add32, 0);
        PL_register_foreign("pl_idx_find32", 5, swi_idx_find32, 0);
        PL_register_foreign("pl_idx_create64", 2, swi_idx_create64, 0);
        PL_register_foreign("pl_idx_destroy64", 1, swi_idx_destroy64, 0);
        PL_register_foreign("pl_idx_add64", 4, swi_idx_add64, 0);
        PL_register_foreign("pl_idx_find64", 5, swi_idx_find64, 0);
}

/* global memory */
static int tbl_count = 0;
#define max_tbl_count 32
static struct tabledef tbl_defs[max_tbl_count];

/* utilities */
foreign_t validate_tbl_id (int tblid)
{
  if (tblid < 0 || tblid + 1 >= max_tbl_count)
  {
    PL_fail;
  }
  PL_succeed;
}

foreign_t access_tbl_id (term_t in_tblid, int *tblid)
{
  if (!PL_is_integer(in_tblid)) { PL_fail; }
  if (!PL_get_integer(in_tblid, tblid)) { PL_fail; }
  return validate_tbl_id(*tblid);
}

foreign_t swi_tbl_info(term_t in_tblid, term_t out_values)
{
  int tblid;
  if (!access_tbl_id(in_tblid, &tblid)) { PL_fail; }
  struct tabledef *tbl = &tbl_defs[tblid];
  if (!PL_is_variable(out_values)) { PL_fail; }

  term_t ele = PL_new_term_ref();
  term_t lst = PL_copy_term_ref(out_values);

  functor_t fct;

  fct = PL_new_functor(PL_new_atom("name"), 1);
  if (!PL_unify_list(lst, ele, lst)) { PL_fail; }
  if (!PL_unify_term(ele, PL_FUNCTOR, fct,
                          PL_STRING,  tbl->name)) { PL_fail; }

  fct = PL_new_functor(PL_new_atom("arity"), 1);
  if (!PL_unify_list(lst, ele, lst)) { PL_fail; }
  if (!PL_unify_term(ele, PL_FUNCTOR, fct,
                          PL_LONG,    tbl->arity)) { PL_fail; }

  fct = PL_new_functor(PL_new_atom("max_rows"), 1);
  if (!PL_unify_list(lst, ele, lst)) { PL_fail; }
  if (!PL_unify_term(ele, PL_FUNCTOR, fct,
                          PL_LONG,    tbl->max_rows)) { PL_fail; }

  fct = PL_new_functor(PL_new_atom("alloc_rows"), 1);
  if (!PL_unify_list(lst, ele, lst)) { PL_fail; }
  if (!PL_unify_term(ele, PL_FUNCTOR, fct,
                          PL_LONG,    tbl->alloc_rows)) { PL_fail; }

  fct = PL_new_functor(PL_new_atom("row_size"), 1);
  if (!PL_unify_list(lst, ele, lst)) { PL_fail; }
  if (!PL_unify_term(ele, PL_FUNCTOR, fct,
                          PL_LONG,    tbl->row_size)) { PL_fail; }

  fct = PL_new_functor(PL_new_atom("nrows"), 1);
  if (!PL_unify_list(lst, ele, lst)) { PL_fail; }
  if (!PL_unify_term(ele, PL_FUNCTOR, fct,
                          PL_LONG,    tbl->nrows)) { PL_fail; }

  fct = PL_new_functor(PL_new_atom("highestrow"), 1);
  if (!PL_unify_list(lst, ele, lst)) { PL_fail; }
  if (!PL_unify_term(ele, PL_FUNCTOR, fct,
                          PL_LONG,    tbl->highestrow)) { PL_fail; }

  fct = PL_new_functor(PL_new_atom("nalloc"), 1);
  if (!PL_unify_list(lst, ele, lst)) { PL_fail; }
  if (!PL_unify_term(ele, PL_FUNCTOR, fct,
                          PL_LONG,    tbl->nalloc)) { PL_fail; }

  return PL_unify_nil(lst);
}

foreign_t swi_tbl_create(term_t tblname, term_t maxrows, term_t allocrows,
                         term_t strx, term_t ret_id)
{
  if (PL_term_type(ret_id) != PL_VARIABLE)
  {
    PL_fail;
  }

  int tbl_id = tbl_count;
  validate_tbl_id(tbl_id);

  /* analyse structure */
  int colcount = 0;
  int ssize = 0;
  term_t hd = PL_new_term_ref();
  term_t tl = PL_copy_term_ref(strx);
  while (PL_get_list(tl, hd, tl))
  {
    if (colcount >= MAX_COLUMNS) { printf("max columns reached\n"); PL_fail; }
    char *coltype;
    if (!PL_get_atom_chars(hd, &coltype)) { printf("invalid type\n"); PL_fail; }
    int colid = colname2type(coltype);
    if (colid < 0) { printf("colid invalid\n"); PL_fail; }
    tbl_defs[tbl_id].col_type[colcount] = colid;
    int colsz = coltype2size(colid);
    if (colsz < 0) { printf("colsz invalid\n"); PL_fail; }
    tbl_defs[tbl_id].col_offset[colcount] = ssize;
    ssize += colsz;
    tbl_defs[tbl_id].col_size[colcount] = colsz;
    colcount += 1;
  }

  tbl_defs[tbl_id].nrows = 0;
  tbl_defs[tbl_id].nalloc = 0;
  tbl_defs[tbl_id].data_point = NULL;
  tbl_defs[tbl_id].arity = colcount;
  tbl_defs[tbl_id].row_size = ssize;

  char *tname;
  if (!PL_get_atom_chars(tblname, &tname))
  {
    printf("cannot access table name\n"); PL_fail;
  }
  tbl_defs[tbl_id].name = strdup(tname);
  // TODO: might remember the atom id instead of its name
  // TODO: check that this will be released

  if (!PL_get_long(maxrows, &tbl_defs[tbl_id].max_rows))
  {
    printf("cannot read integer 'maxrows'\n"); PL_fail;
  }
  if (!PL_get_long(allocrows, &tbl_defs[tbl_id].alloc_rows))
  {
    printf("cannot read integer 'allocrows'\n"); PL_fail;
  }

  tbl_count += 1;  // commit this id

  /* return id of tabledef */
  return PL_unify_int64(ret_id, tbl_id);
}


foreign_t swi_tbl_get_idx_value(term_t in_tblid, term_t in_idx, term_t out_values)
{
  int tblid;
  if (!access_tbl_id(in_tblid, &tblid)) { PL_fail; }
  struct tabledef *tbl = &tbl_defs[tblid];
  if (PL_is_variable(in_idx)) { PL_fail; }
  if (!PL_is_variable(out_values)) { PL_fail; }

  long rowidx;
  if (!PL_get_long(in_idx, &rowidx) || rowidx < 0) { PL_fail; }
  if (rowidx > tbl->highestrow) { PL_fail; }

  if (tbl->isvalid[rowidx] != '1') { PL_fail; }

  char *dp = tbl->data_point + (rowidx * tbl->row_size);
  term_t ele = PL_new_term_ref();
  term_t lst = PL_copy_term_ref(out_values);
  int ncol = 0;
  while (ncol < tbl->arity)
  {
    if (!PL_unify_list(lst, ele, lst)) { PL_fail; }
    switch (tbl->col_type[ncol])
    {
      case COL_CHAR:
        {
          char c = 0;
          memcpy(&c,
                 dp+tbl->col_offset[ncol],
                 tbl->col_size[ncol]);
          long t = c;
          if (!PL_unify_integer(ele, t)) { PL_fail; }
          break;
        }
      case COL_UCHAR:
        {
          unsigned char c = 0;
          memcpy(&c,
                 dp+tbl->col_offset[ncol],
                 tbl->col_size[ncol]);
          long t = c;
          if (!PL_unify_integer(ele, t)) { PL_fail; }
          break;
        }
      case COL_INT32:
        {
          int32_t i = 0;
          memcpy(&i,
                 dp+tbl->col_offset[ncol],
                 tbl->col_size[ncol]);
          long t = i;
          if (!PL_unify_integer(ele, t)) { PL_fail; }
          break;
        }
      case COL_UINT32:
        {
          uint32_t i = 0;
          memcpy(&i,
                 dp+tbl->col_offset[ncol],
                 tbl->col_size[ncol]);
          long t = i;
          if (!PL_unify_integer(ele, t)) { PL_fail; }
          break;
        }
      case COL_INT64:
        {
          int64_t i = 0;
          memcpy(&i,
                 dp+tbl->col_offset[ncol],
                 tbl->col_size[ncol]);
          if (!PL_unify_int64(ele, i)) { PL_fail; }
          break;
        }
      case COL_UINT64:
        {
          uint64_t i = 0;
          memcpy(&i,
                 dp+tbl->col_offset[ncol],
                 tbl->col_size[ncol]);
          if (!PL_unify_uint64(ele, i)) { PL_fail; }
          break;
        }
      case COL_ATOM:
        {
          atom_t t;
          memcpy(&t,
                 dp+tbl->col_offset[ncol],
                 tbl->col_size[ncol]);
          if (!PL_unify_atom(ele, t)) { PL_fail; }
          break;
        }
      case COL_DOUBLE:
        {
          double t = 0;
          memcpy(&t,
                 dp+tbl->col_offset[ncol],
                 tbl->col_size[ncol]);
          if (!PL_unify_float(ele, t)) { PL_fail; }
          break;
        }
    } // switch
    ncol++;
  }

  return PL_unify_nil(lst);
}

foreign_t swi_tbl_has_idx(term_t in_tblid, term_t in_idx)
{
  int tblid;
  if (!access_tbl_id(in_tblid, &tblid)) { PL_fail; }
  if (PL_is_variable(in_idx)) { PL_fail; }

  long rowidx;
  if (!PL_get_long(in_idx, &rowidx)) { PL_fail; }
  
  // needs to be in the valid range and the data point needs to be valid
  if (rowidx < 0 || (rowidx + 1) * tbl_defs[tblid].row_size > tbl_defs[tblid].nalloc)
  {
    PL_fail;
  }
  if (tbl_defs[tblid].isvalid[rowidx] != '1')
  {
    //printf("@%ld -> '%c'\n", rowidx, tbl_defs[tblid].isvalid[rowidx]);
    PL_fail;
  }
  PL_succeed;
}

foreign_t swi_tbl_next_idx(term_t in_tblid, term_t out_idx, control_t handle)
{
  int tblid;
  if (!access_tbl_id(in_tblid, &tblid)) { PL_fail; }
  if (!PL_is_variable(out_idx)) { PL_fail; }
  struct tabledef *tbl = &tbl_defs[tblid];

  long *lastidx;
  switch (PL_foreign_control(handle)) {
    case PL_FIRST_CALL:
      // none
      if (tbl->nrows <= 0)
      {
        PL_fail;
      }
      // search for first valid row
      long idx = 0;
      for (; idx <= tbl->highestrow; idx++) {
        if (tbl->isvalid[idx] == '1')
        {
          break;
        }
      }
      if (tbl->nrows == 1)
      {
        if (!PL_unify_integer(out_idx, idx)) { PL_fail; }
        PL_succeed;
      }
      lastidx = malloc(sizeof(long));
      *lastidx = idx;
      if (PL_unify_integer(out_idx, *lastidx))
      {
        PL_retry_address(lastidx);
      }
      else
      {
        free(lastidx);
        PL_fail;
      }
    case PL_REDO:
      lastidx = PL_foreign_context_address(handle);
      // search for next valid row
      *lastidx += 1;
      while (*lastidx <= tbl->highestrow)
      {
        if (tbl->isvalid[*lastidx] == '1')
        {
          break;
        }
        *lastidx += 1;
      }
      if (tbl->isvalid[*lastidx] != '1')
      {
        free(lastidx);
        PL_fail;
      }
      else
      {
        if (*lastidx == tbl->highestrow)
        {
          if (PL_unify_integer(out_idx, *lastidx));
          free(lastidx);
          PL_succeed;
        }
        else
        {
          if (PL_unify_integer(out_idx, *lastidx));
          PL_retry_address(lastidx);
        }
      }
    case PL_PRUNED:
      lastidx = PL_foreign_context_address(handle);
      free(lastidx);
      PL_succeed;
  }
  PL_fail;
}

foreign_t swi_tbl_find_idx(term_t in_tblid, term_t in_ncol, term_t in_value, term_t out_idx, control_t handle)
{
  int tblid;
  if (!access_tbl_id(in_tblid, &tblid)) { PL_fail; }
  if (!PL_is_variable(out_idx)) { PL_fail; }
  struct tabledef *tbl = &tbl_defs[tblid];
  long ncol;
  if (!PL_get_long(in_ncol, &ncol) || ncol < 0) { PL_fail; }
  if (ncol >= tbl->arity) { PL_fail; }
  char criterium[12];
  size_t matchlen = tbl->col_size[ncol];
  size_t fldoffset = tbl->col_offset[ncol];
  switch (tbl->col_type[ncol]) {
    case COL_UCHAR:
    case COL_CHAR:
    {
      long t;
      if (!PL_get_long(in_value, &t)) { PL_fail; }
      char c = t & 0xff;
      memcpy(&criterium, &c, matchlen);
      break;
    }
    case COL_UINT32:
    case COL_INT32:
    {
      long t;
      if (!PL_get_long(in_value, &t)) { PL_fail; }
      int32_t v = t & 0xffffffff;
      memcpy(&criterium, &v, matchlen);
      break;
    }
    case COL_UINT64:
    case COL_INT64:
    {
      int64_t t;
      if (!PL_get_int64(in_value, &t)) { PL_fail; }
      memcpy(&criterium, &t, matchlen);
      break;
    }
    case COL_DOUBLE:
    {
      double t;
      if (!PL_get_float(in_value, &t)) { PL_fail; }
      memcpy(&criterium, &t, matchlen);
      break;
    }
    case COL_ATOM:
    {
      atom_t t;
      if (!PL_get_atom(in_value, &t)) { PL_fail; }
      memcpy(&criterium, &t, matchlen);
      break;
    }
  }

  long *lastidx;
  char *dp;
  switch (PL_foreign_control(handle)) {
    case PL_FIRST_CALL:
      // none
      if (tbl->nrows <= 0)
      {
        PL_fail;
      }
      // search for first valid row matching criterium
      long idx = 0;
      dp = tbl->data_point + fldoffset + (idx * tbl->row_size);
      for (; idx <= tbl->highestrow; idx++) {
        if (tbl->isvalid[idx] == '1' &&
            dp[0] == criterium[0] &&
            strncmp(dp, criterium, matchlen) == 0)
        {
          break;
        }
        dp += tbl->row_size;
      }
      if (tbl->nrows == 1)
      {
        if (!PL_unify_integer(out_idx, idx)) { PL_fail; }
        PL_succeed;
      }
      lastidx = malloc(sizeof(long));
      *lastidx = idx;
      if (PL_unify_integer(out_idx, *lastidx))
      {
        PL_retry_address(lastidx);
      }
      else
      {
        free(lastidx);
        PL_fail;
      }
    case PL_REDO:
      lastidx = PL_foreign_context_address(handle);
      // search for next valid row
      *lastidx += 1;
      char found = 0;
      dp = tbl->data_point + fldoffset + (*lastidx * tbl->row_size);
      while (*lastidx <= tbl->highestrow)
      {
        if (tbl->isvalid[*lastidx] == '1' &&
            dp[0] == criterium[0] &&
            strncmp(dp, criterium, matchlen) == 0)
        {
          found = 1;
          break;
        }
        *lastidx += 1;
        dp += tbl->row_size;
      }
      if (found == 0)
      {
        free(lastidx);
        PL_fail;
      }
      else
      {
        if (*lastidx == tbl->highestrow)
        {
          if (PL_unify_integer(out_idx, *lastidx));
          free(lastidx);
          PL_succeed;
        }
        else
        {
          if (PL_unify_integer(out_idx, *lastidx));
          PL_retry_address(lastidx);
        }
      }
    case PL_PRUNED:
      lastidx = PL_foreign_context_address(handle);
      free(lastidx);
      PL_succeed;
  }
  PL_fail;
}

foreign_t swi_tbl_set_idx_value(term_t in_tblid, term_t in_idx, term_t in_values)
{
  int tblid;
  if (!access_tbl_id(in_tblid, &tblid)) { PL_fail; }
  if (PL_is_variable(in_values)) { PL_fail; }
  if (PL_is_variable(in_idx)) { PL_fail; }
  long rowidx;
  if (!PL_get_long(in_idx, &rowidx) || rowidx < 0) { PL_fail; }

  struct tabledef *tbl = &tbl_defs[tblid];
  // check for space
  size_t allocsize = tbl->row_size * (rowidx + 1);
  if (tbl->nalloc < allocsize)
  {
    allocsize = (tbl->alloc_rows + rowidx) * 1;
    void *ptr = realloc(tbl->isvalid, allocsize);
    if (ptr)
    {
      tbl->isvalid = ptr;
      // invalidate all new data
      int r0 = tbl->nalloc / tbl->row_size;
      for (; r0 < tbl->alloc_rows + rowidx; r0++) {
        tbl->isvalid[r0] = '0';
      }
    }
    else
    {
      printf("error on realloc of 'isvalid'\n");
      PL_fail;
    }

    allocsize = (tbl->alloc_rows + rowidx) * tbl->row_size;
    ptr = realloc(tbl->data_point, allocsize);
    if (ptr)
    {
      //printf("reallocated from size %ld to %ld\n", tbl->nalloc, allocsize);
      tbl->nalloc = allocsize;
      tbl->data_point = ptr;
    } else {
      printf("failure on table allocation\n");
      PL_fail;
    }
  }

  // copy values
  char *dp = tbl->data_point + (rowidx * tbl->row_size);
  int colcount = 0;
  term_t hd = PL_new_term_ref();
  term_t tl = PL_copy_term_ref(in_values);
  while (PL_get_list(tl, hd, tl))
  {
    if (colcount >= MAX_COLUMNS) { printf("max columns reached\n"); PL_fail; }
    //printf("%d copy value of type %d at offset %ld with len %ld\n", colcount, tbl->col_type[colcount], tbl->col_offset[colcount], tbl->col_size[colcount]);
    switch (tbl->col_type[colcount]) {
      case COL_UCHAR:
      case COL_CHAR:
      {
        long t;
        if (!PL_get_long(hd, &t)) { PL_fail; }
        char c = t & 0xff;
        memcpy(dp+tbl->col_offset[colcount],
               &c,
               tbl->col_size[colcount]);
        break;
      }
      case COL_UINT32:
      case COL_INT32:
      {
        long t;
        if (!PL_get_long(hd, &t)) { PL_fail; }
        int32_t v = t & 0xffffffff;
        memcpy(dp+tbl->col_offset[colcount],
               &v,
               tbl->col_size[colcount]);
        break;
      }
      case COL_UINT64:
      case COL_INT64:
      {
        int64_t t;
        if (!PL_get_int64(hd, &t)) { PL_fail; }
        memcpy(dp+tbl->col_offset[colcount],
               &t,
               tbl->col_size[colcount]);
        break;
      }
      case COL_ATOM:
      {
        atom_t t;
        if (!PL_get_atom(hd, &t)) { PL_fail; }
        PL_register_atom(t); // prevent gc of atom
        memcpy(dp+tbl->col_offset[colcount],
               &t,
               tbl->col_size[colcount]);
        break;
      }
      case COL_DOUBLE:
      {
        double t;
        if (!PL_get_float(hd, &t)) { PL_fail; }
        memcpy(dp+tbl->col_offset[colcount],
               &t,
               tbl->col_size[colcount]);
        break;
      }
    }
    colcount += 1;
  }

  if (colcount != tbl->arity)
  {
    PL_fail;
  }
  tbl->isvalid[rowidx] = '1';
  tbl->nrows += 1;
  if (tbl->highestrow < rowidx)
  {
    tbl->highestrow = rowidx;
  }
  PL_succeed;
}

foreign_t swi_tbl_invalidate_idx(term_t in_tblid, term_t in_idx)
{
  int tblid;
  if (!access_tbl_id(in_tblid, &tblid)) { PL_fail; }
  struct tabledef *tbl = &tbl_defs[tblid];
  if (PL_is_variable(in_idx)) { PL_fail; }
  long rowidx;
  if (!PL_get_long(in_idx, &rowidx) || rowidx < 0) { PL_fail; }
  if (rowidx > tbl->highestrow) { PL_fail; }

  tbl->isvalid[rowidx] = '0';

  // TODO: if type = COL_ATOM; then PL_unregister_atom(t)

  PL_succeed;
}

/* index functions */

typedef void* (*fp_create_idx)(int);
typedef int (*fp_destroy_idx)(void*);
typedef int (*fp_add_data)(void*, const char*, int, const char*);
typedef int (*fp_find_data)(void*, const char*, int, char*);
typedef int (*fp_put_value32)(const uint32_t, char*);
typedef int (*fp_put_value64)(const uint64_t, char*);

typedef struct idx_funs {
  fp_create_idx create_idx;
  fp_destroy_idx destroy_idx;
  fp_add_data add_data;
  fp_find_data find_data;
} idx_funs;

struct idx_funs idx_funs32 = { 
  .create_idx = &create_index32,
  .destroy_idx = &destroy_index32,
  .add_data = &add_data32,
  .find_data = &find_data32,
};
struct idx_funs idx_funs64 = { 
  .create_idx = &create_index64,
  .destroy_idx = &destroy_index64,
  .add_data = &add_data64,
  .find_data = &find_data64,
};

foreign_t swi_idx_create(struct idx_funs *f, term_t in_dlen, term_t out_xid)
{
  long dlen;
  if (!PL_get_long(in_dlen, &dlen) || dlen < 0) { PL_fail; }
  void* xid = f->create_idx(dlen);
  return PL_unify_pointer(out_xid, xid);
}
foreign_t swi_idx_create32(term_t in_dlen, term_t out_xid) {
  return swi_idx_create(&idx_funs32, in_dlen, out_xid);
}
foreign_t swi_idx_create64(term_t in_dlen, term_t out_xid) {
  return swi_idx_create(&idx_funs64, in_dlen, out_xid);
}

foreign_t swi_idx_destroy(struct idx_funs *f, term_t in_xid)
{
  void* xid = NULL;
  if (!PL_get_pointer(in_xid, &xid)) { PL_fail; }
  if (xid) {
    f->destroy_idx(xid);
  }
  PL_succeed;
}
foreign_t swi_idx_destroy32(term_t in_xid) {
  return swi_idx_destroy(&idx_funs32, in_xid);
}
foreign_t swi_idx_destroy64(term_t in_xid) {
  return swi_idx_destroy(&idx_funs64, in_xid);
}

int term_to_32(term_t in_value, char out_value[4])
{
  T32 v;
  switch (PL_term_type(in_value)) {
    case PL_INTEGER:
      {
        long t;
        if (!PL_get_long(in_value, &t)) { PL_fail; }
        v = (T32)t & 0xffffffff;
      }
      break;
    default:
      PL_fail;
  }
  put_value32(v, out_value);
  PL_succeed;
}

int term_to_64(term_t in_value, char out_value[8])
{
  T64 v;
  switch (PL_term_type(in_value)) {
    case PL_ATOM:
      {
        atom_t t;
        if (!PL_get_atom(in_value, &t)) { PL_fail; }
        v = (T64)t;
      }
      break;
    case PL_INTEGER:
      {
        long t;
        if (!PL_get_long(in_value, &t)) { PL_fail; }
        v = (T64)t;
      }
      break;
    case PL_FLOAT:
      {
        double t;
        if (!PL_get_float(in_value, &t)) { PL_fail; }
        v = (T64)t;
      }
      break;
    default:
      PL_fail;
  }
  put_value64(v, out_value);
  PL_succeed;
}

foreign_t swi_idx_add(int is64, struct idx_funs *f, term_t in_xid, term_t in_key, term_t in_dlen, term_t in_value)
{
  void* xid = NULL;
  if (!PL_get_pointer(in_xid, &xid) || xid == NULL) { PL_fail; }
  long dlen;
  if (!PL_get_long(in_dlen, &dlen) || dlen < 0) { PL_fail; }
  if (!(dlen == 4 || dlen == 8)) { PL_fail; }
  char k[9];
  if (1 == is64) {
    if (!term_to_64(in_key, k)) { PL_fail; }
  } else {
    if (!term_to_32(in_key, k)) { PL_fail; }
    k[4]=0; k[5]=0; k[6]=0; k[7]=0;
  }
  char v[9]; // > 8
  if (dlen == 4) {
    if (!term_to_32(in_value, v)) { PL_fail; }
  }
  else if (dlen == 8) {
    if (!term_to_64(in_value, v)) { PL_fail; }
  }
  else
  {
    PL_fail;
  }
  f->add_data(xid, k, dlen, v);
  PL_succeed;
}
foreign_t swi_idx_add32(term_t in_xid, term_t in_key, term_t in_dlen, term_t in_value) {
  return swi_idx_add(0, &idx_funs32, in_xid, in_key, in_dlen, in_value);
}
foreign_t swi_idx_add64(term_t in_xid, term_t in_key, term_t in_dlen, term_t in_value) {
  return swi_idx_add(1, &idx_funs64, in_xid, in_key, in_dlen, in_value);
}

foreign_t swi_idx_find(int (*get_term)(term_t, char*), struct idx_funs *f, term_t in_xid, term_t in_key, term_t in_dlen, term_t out_count, term_t out_results)
{
  void* xid = NULL;
  if (!PL_get_pointer(in_xid, &xid) || xid == NULL) { PL_fail; }
  long dlen;
  if (!PL_get_long(in_dlen, &dlen) || dlen < 0) { PL_fail; }
  if (!(dlen == 4 || dlen == 8)) { PL_fail; }
  char k[9]; k[4]='\0'; k[8]='\0';
  get_term(in_key, k);
  const int sz = 1024;
  char buf[sz];
  long count = f->find_data(xid, k, sz, buf);
  if (!PL_unify_integer(out_count, count)) { PL_fail; }
  term_t ele = PL_new_term_ref();
  term_t lst = PL_copy_term_ref(out_results);
  int idx = 0, c = 0;
  while (c < count && idx + dlen <= sz)
  {
    if (!PL_unify_list(lst, ele, lst)) { PL_fail; }
    if (dlen == 4) {
      T32 t = mk_value32(buf+idx);
      if (!PL_unify_integer(ele, t)) { PL_fail; }
    } else {
      T64 t = mk_value64(buf+idx);
      if (!PL_unify_integer(ele, t)) { PL_fail; }
    }
    idx += dlen;
    c++;
  }
  return PL_unify_nil(lst);
}

foreign_t swi_idx_find32(term_t in_xid, term_t in_key, term_t in_dlen, term_t out_count, term_t out_results) {
  return swi_idx_find(&term_to_32, &idx_funs32, in_xid, in_key, in_dlen, out_count, out_results);
}
foreign_t swi_idx_find64(term_t in_xid, term_t in_key, term_t in_dlen, term_t out_count, term_t out_results) {
  return swi_idx_find(&term_to_64, &idx_funs64, in_xid, in_key, in_dlen, out_count, out_results);
}
