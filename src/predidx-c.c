
#include <string.h>
#include <stdio.h>

#include "predidx-c.h"

int colname2type(const char *cname)
{
  if (cname == NULL) { return -1; }
  switch (cname[0]) {
    case 'u':
        if (strcmp(cname,"uchar") == 0) { return COL_UCHAR; }
        if (strcmp(cname,"uint32") == 0) { return COL_UINT32; }
        if (strcmp(cname,"uint64") == 0) { return COL_UINT64; }
        break;
    case 'c':
        if (strcmp(cname,"char") == 0) { return COL_CHAR; }
        break;
    case 'i':
        if (strcmp(cname,"int32") == 0) { return COL_INT32; }
        if (strcmp(cname,"int64") == 0) { return COL_INT64; }
        break;
    case 'a':
        if (strcmp(cname,"atom") == 0) { return COL_ATOM; }
        break;
    case 'd':
        if (strcmp(cname,"double") == 0) { return COL_DOUBLE; }
        break;
    default:
      printf("unknown column type: %s\n", cname);
      break;
  }
  return -1;
}

static size_t _coltype2size[MAX_COL_TYPE + 1] =
  { sizeof(unsigned char),
    sizeof(char),
    sizeof(uint32_t),
    sizeof(int32_t),
    sizeof(uint64_t),
    sizeof(int64_t),
    sizeof(double),
    sizeof(void*)
  };
  
int coltype2size(int coltype)
{
  if (coltype <= MAX_COL_TYPE) {
    return _coltype2size[coltype];
  }
  return -1;
}

