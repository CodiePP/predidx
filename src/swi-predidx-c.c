
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


install_t install()
{
        PL_register_foreign("pl_tbl_create", 5, swi_tbl_create, 0);
        PL_register_foreign("pl_tbl_has_idx", 2, swi_tbl_has_idx, 0);
        PL_register_foreign("pl_tbl_next_idx", 2, swi_tbl_next_idx, PL_FA_NONDETERMINISTIC);
        PL_register_foreign("pl_tbl_get_idx_value", 3, swi_tbl_get_idx_value, 0);
        PL_register_foreign("pl_tbl_set_idx_value", 3, swi_tbl_set_idx_value, 0);
        PL_register_foreign("pl_tbl_invalidate_idx", 2, swi_tbl_invalidate_idx, 0);
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
    PL_fail;
  }
  PL_succeed;
}

foreign_t swi_tbl_next_idx(term_t in_tblid, term_t out_idx, control_t handle)
{
  int tblid;
  if (!access_tbl_id(in_tblid, &tblid)) { PL_fail; }
  if (!PL_is_variable(out_idx)) { PL_fail; }

  long *lastidx;
  switch (PL_foreign_control(handle)) {
    case PL_FIRST_CALL:
      // none
      if (tbl_defs[tblid].nrows <= 0)
      {
        PL_fail;
      }
      // TODO: search for first valid row
      long idx = 0;
      for (; idx <= tbl_defs[tblid].highestrow; idx++) {
        if (tbl_defs[tblid].isvalid[idx] == '1')
        {
          break;
        }
      }
      if (tbl_defs[tblid].nrows == 1)
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
      // TODO: search for next valid row
      *lastidx += 1;
      while (*lastidx <= tbl_defs[tblid].highestrow)
      {
        if (tbl_defs[tblid].isvalid[*lastidx] == '1')
        {
          break;
        }
        *lastidx += 1;
      }
      if (tbl_defs[tblid].isvalid[*lastidx] != '1')
      {
        free(lastidx);
        PL_fail;
      }
      else
      {
        if (*lastidx == tbl_defs[tblid].highestrow)
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
      int r0 = tbl->alloc_rows;
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
        if (!PL_get_long(hd, &t))
        {
          PL_fail;
        }
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
        if (!PL_get_long(hd, &t))
        {
          PL_fail;
        }
        memcpy(dp+tbl->col_offset[colcount],
               &t,
               tbl->col_size[colcount]);
        break;
      }
      case COL_UINT64:
      case COL_INT64:
      {
        int64_t t;
        if (!PL_get_int64(hd, &t))
        {
          PL_fail;
        }
        memcpy(dp+tbl->col_offset[colcount],
               &t,
               tbl->col_size[colcount]);
        break;
      }
      case COL_ATOM:
      {
        atom_t t;
        if (!PL_get_atom(hd, &t))
        {
          PL_fail;
        }
        PL_register_atom(t); // prevent gc of atom
        memcpy(dp+tbl->col_offset[colcount],
               &t,
               tbl->col_size[colcount]);
        break;
      }
      case COL_DOUBLE:
      {
        double t;
        if (!PL_get_float(hd, &t))
        {
          PL_fail;
        }
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

