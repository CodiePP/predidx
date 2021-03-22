
#include <stddef.h>
#include <stdint.h>

#define COL_UCHAR    0
#define COL_CHAR     1
#define COL_UINT32   2
#define COL_INT32    3
#define COL_UINT64   4
#define COL_INT64    5
#define COL_DOUBLE   6
#define COL_ATOM     7

#define MAX_COL_TYPE COL_ATOM

#define MAX_COLUMNS 32
struct tabledef {
  char *name;
  int arity;
  long max_rows;
  long alloc_rows;
  size_t row_size;
  int col_type[MAX_COLUMNS];
  size_t col_offset[MAX_COLUMNS];
  size_t col_size[MAX_COLUMNS];
  long nrows;
  long highestrow;
  long nalloc;
  char *isvalid;
  char *data_point;
};

extern int colname2type(const char *colname);
extern int coltype2size(int coltype);

