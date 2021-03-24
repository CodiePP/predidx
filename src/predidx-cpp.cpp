#include <string.h>
#include <stdio.h>
#include <assert.h>

#include <algorithm>
#include <unordered_map>

#include "predidx-c.h"


void get_value(const char *d, T32 &v);
void get_value(const char *d, T64 &v);
void put_value(const T32 v, char *d);
void put_value(const T64 v, char *d);

#define DataIndex32_4 DataIndex<T32,T32>
#define DataIndex32_8 DataIndex<T32,T64>
#define DataIndex64_4 DataIndex<T64,T32>
#define DataIndex64_8 DataIndex<T64,T64>

/* C++ */
template <typename KeyT, typename ValT>
class DataIndex {
  public:
    DataIndex() = default;
    ~DataIndex() = default;
    int add(KeyT k, int len, const char *d) {
      ValT t; get_value(d, t);
      _map.emplace(k, t);
      return sizeof(ValT);
    };
    int find(KeyT k, int maxlen, char *d) const {
      auto range = _map.equal_range(k);
      int count = 0, idx = 0;
      for_each(range.first, range.second,
          [&](auto const &itm) {
              count++;
              if (idx <= maxlen - _dlen) {
                  put_value(itm.second, d+idx);
                  idx += _dlen;
              }
          });
      return count;
    }
    int _dlen {sizeof(ValT)};
  private:
    std::unordered_multimap<KeyT,ValT> _map;

    DataIndex(DataIndex const &) = delete;
    DataIndex& operator=(DataIndex const &) = delete;
};

template class DataIndex32_4;
template class DataIndex32_8;
template class DataIndex64_4;
template class DataIndex64_8;

void get_value(const char *d, T32 &v) {
    v = mk_value32(d);
}
void get_value(const char *d, T64 &v) {
    v = mk_value64(d);
}

void put_value(const T32 v, char *d) {
  put_value32(v,d);
}
void put_value(T64 const v, char *d) {
  put_value64(v,d);
}

/* exported C functions */

// create an index where key length is 32/64 bits and data length is dlen (bytes)
extern "C" EXPORT
index32_p create_index32(int dlen)
{
  if (dlen == 4) {
    return new DataIndex32_4();
  } if (dlen == 8) {
    return new DataIndex32_8();
  }
  return NULL;
}

extern "C" EXPORT
index64_p create_index64(int dlen)
{
  if (dlen == 4) {
    return new DataIndex64_4();
  } if (dlen == 8) {
    return new DataIndex64_8();
  }
  return NULL;
}

extern "C" EXPORT
int destroy_index32(index32_p pidx)
{
  if (pidx) {
    DataIndex32_4 *p = (DataIndex32_4*)pidx;
    int dlen = p->_dlen;
    if (dlen == 4) {
      delete (DataIndex32_4*)pidx;
    } else {
      delete (DataIndex32_8*)pidx;
    }
  }
  return 0;
}

extern "C" EXPORT
int destroy_index64(index64_p pidx)
{
  if (pidx) {
    DataIndex64_4 *p = (DataIndex64_4*)pidx;
    int dlen = p->_dlen;
    if (dlen == 4) {
      delete (DataIndex64_4*)pidx;
    } else {
      delete (DataIndex64_8*)pidx;
    }
  }
  return 0;
}


// adds data (in d, len bytes) for key
extern "C" EXPORT
int add_data32(index32_p pidx, const data32_t k, int len, const char *d)
{
  if (pidx) {
    DataIndex32_4 *p4 = (DataIndex32_4*)pidx;
    DataIndex32_8 *p8 = (DataIndex32_8*)pidx;
    auto key = mk_value32(k);
    if (len == 4) {
      p4->add(key, len, d);
      return 4;
    } else if (len == 8) {
      p8->add(key, len, d);
      return 8;
    }
  }
  return 0;
}

extern "C" EXPORT
int add_data64(index64_p pidx, const data64_t k, int len, const char *d)
{
  if (pidx) {
    DataIndex64_4 *p4 = (DataIndex64_4*)pidx;
    DataIndex64_8 *p8 = (DataIndex64_8*)pidx;
    auto key = mk_value64(k);
    if (len == 4) {
      p4->add(key, len, d);
      return 4;
    } else if (len == 8) {
      p8->add(key, len, d);
      return 8;
    }
  }
  return 0;
}


// finds all data entries for a key, returns data in buffer with max. len (bytes)
// returns the number of matching items, maybe more than (maxlen / dlen)
extern "C" EXPORT
int find_data32(index32_p pidx, const data32_t k, int maxlen, char *d)
{
  if (pidx) {
    DataIndex32_4 *p4 = (DataIndex32_4*)pidx;
    DataIndex32_8 *p8 = (DataIndex32_8*)pidx;
    auto key = mk_value32(k);
    int dlen = p4->_dlen;
    if (dlen == 4) {
      return p4->find(key, maxlen, d);
    } else if (dlen == 8) {
      return p8->find(key, maxlen, d);
    }
  }
  return 0;
}

extern "C" EXPORT
int find_data64(index64_p pidx, const data64_t k, int maxlen, char *d)
{
  if (pidx) {
    DataIndex64_4 *p4 = (DataIndex64_4*)pidx;
    DataIndex64_8 *p8 = (DataIndex64_8*)pidx;
    auto key = mk_value64(k);
    int dlen = p4->_dlen;
    if (dlen == 4) {
      return p4->find(key, maxlen, d);
    } else if (dlen == 8) {
      return p8->find(key, maxlen, d);
    }
  }
  return 0;
}

int cpp_test32()
{
  DataIndex32_8 t1;
  char s1[5] = "key1";
  auto k1 = mk_value32(s1);
  char s2[5] = "key2";
  auto k2 = mk_value32(s2);
  assert(k2 - k1 == 1);
  int ret1 = add_data32(&t1, s1, 8, "abcdefgh");
  assert(ret1 == 8);
  int ret2 = add_data32(&t1, s2, 8, "abcdefgh");
  int ret3 = add_data32(&t1, s2, 8, "12345678");

  char buf[100];
  buf[16] = '\0';
  int ret4 = find_data32(&t1, s2, 100, buf);
  assert(ret4 == 2);
  printf("ret = %s\n", buf);

  return 0;
}

int cpp_test64()
{
  DataIndex64_4 t1;
  char s1[9] = "key10000";
  T64 k1 = mk_value64(s1);
  char q1[9]; put_value(k1, q1);
  assert((q1[0]==s1[0]) && (q1[1]==s1[1]) && (q1[2]==s1[2]) && (q1[3]==s1[3]) &&
         (q1[4]==s1[4]) && (q1[5]==s1[5]) && (q1[6]==s1[6]) && (q1[7]==s1[7]));
  char s2[9] = "key20000";
  auto k2 = mk_value64(s2);
  printf("key1 = %lx key2 = %lx\n", k1, k2);
  int ret1 = t1.add(k1, 4, "abcd");
  assert(ret1 == 4);
  int ret2 = t1.add(k2, 4, "dcba");
  assert(ret2 == 4);
  int ret3 = add_data64(&t1, s1, 4, "dcba");
  assert(ret3 == 4);

  char buf[5];
  buf[4] = '\0';
  int ret4 = t1.find(k1, 4, buf);
  assert(ret4 == 2);
  printf("ret = %s\n", buf);

  return 0;
}

