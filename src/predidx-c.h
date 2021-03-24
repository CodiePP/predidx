
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
#if defined(_MSC_VER)

#define EXPORT __declspec(dllexport)
#define IMPORT __declspec(dllimport)

#elif defined(__GNUC__)

#define EXPORT __attribute__((visibility("default")))
#define IMPORT __attribute__((visibility("default")))

#else

#define EXPORT
#define IMPORT
#pragma warning undefined import/export attributes

#endif

#else  // in "C"

#define EXPORT
#define IMPORT

#endif

#ifdef __cplusplus
extern "C" {
#endif

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

/* indexing */

typedef void* index32_p; 
typedef void* index64_p; 
typedef char data32_t[4];
typedef char data64_t[8];

extern EXPORT int32_t mk_value32(const char *k);
extern EXPORT int64_t mk_value64(const char *k);

// create an index where key length is 32/64 bits and data length is dlen (bytes)
extern EXPORT index32_p create_index32(int dlen);
extern EXPORT index64_p create_index64(int dlen);
extern EXPORT int destroy_index32(index32_p);
extern EXPORT int destroy_index64(index64_p);

// adds data (in d, len bytes) for key
extern EXPORT int add_data32(index32_p, const data32_t k, int len, const char *d);
extern EXPORT int add_data64(index64_p, const data64_t k, int len, const char *d);

// finds all data entries for a key, returns data in buffer with max. len (bytes)
// returns the number of matching items, maybe more than (maxlen / dlen)
extern EXPORT int find_data32(index32_p, const data32_t k, int maxlen, char *d);
extern EXPORT int find_data64(index64_p, const data64_t k, int maxlen, char *d);

#ifdef __cplusplus
}
#endif
