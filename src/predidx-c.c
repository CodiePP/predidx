
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

void put_value32(const T32 v, char out_value[4])
{
  out_value[3] = (char)v&0xff;
  out_value[2] = (char)(v>>8)&0xff;
  out_value[1] = (char)(v>>16)&0xff;
  out_value[0] = (char)(v>>24)&0xff;
}

void put_value64(const T64 v, char out_value[8])
{
  out_value[7] = (char)v&0xff;
  out_value[6] = (char)(v>>8)&0xff;
  out_value[5] = (char)(v>>16)&0xff;
  out_value[4] = (char)(v>>24)&0xff;
  out_value[3] = (char)(v>>32)&0xff;
  out_value[2] = (char)(v>>40)&0xff;
  out_value[1] = (char)(v>>48)&0xff;
  out_value[0] = (char)(v>>56)&0xff;
}

T32 mk_value32(const char *k)
{
    T32 v=k[3];
    T32 key = v&0x000000ff;
    v = k[2];
    key += (v<<8)&0x0000ff00;
    v = k[1];
    key += (v<<16)&0x00ff0000;
    v = k[0];
    key += (v<<24)&0xff000000;
    return key;
}

T64 mk_value64(const char *k)
{
    T64 v=k[7];
    T64 key = v&0x00000000000000ff;
    v = k[6];
    key += (v<<8)&0x000000000000ff00;
    v = k[5];
    key += (v<<16)&0x0000000000ff0000;
    v = k[4];
    key += (v<<24)&0x00000000ff000000;
    v = k[3];
    key += (v<<32)&0x000000ff00000000;
    v = k[2];
    key += (v<<40)&0x0000ff0000000000;
    v = k[1];
    key += (v<<48)&0x00ff000000000000;
    v = k[0];
    key += (v<<56)&0xff00000000000000;
    return key;
}

