/*
 * build objects: $ make swi
 * build test program: 
 * $ g++ -g -o test++ -Isrc test/test.cpp obj-Linux/predidx-c.o obj-Linux/predidx-cpp.o
 *
 * run test program: $ ./test++
 */

#include <stdio.h>
#include <assert.h>

#include "predidx-c.h"

extern int cpp_test32();
extern int cpp_test64();

int c_test32()
{
  void* t1 = create_index32(8);
  char k1[5] = "key1";
  char k2[5] = "key2";
  T32 d1 = 1539788031;
  T32 d2 = 1539798132;
  put_value32(d1,k1);
  put_value32(d2,k2);
  T32 c1 = mk_value32(k1);
  printf("c1: %d == d1: %d\n", c1, d1);
  T32 c2 = mk_value32(k2);
  assert(c1 == d1);
  assert(c2 == d2);

  char v1[9] = "abcdefgh";
  char v2[9] = "12345678";
  int ret1 = add_data32(t1, k1, 8, v1);
  assert(ret1 == 8);
  int ret2 = add_data32(t1, k2, 8, v1);
  int ret3 = add_data32(t1, k2, 8, v2);

  char buf[100];
  buf[16] = '\0';
  int ret4 = find_data32(t1, k2, 100, buf);
  assert(ret4 == 2);
  printf("ret = %s\n", buf);

  destroy_index32(t1);

  return 0;
}

int c_test64()
{
  void* t1 = create_index64(4);
  char k1[9];
  char k2[9];
  char v1[5] = "abcd";
  char v2[5] = "1234";
  T64 d1 = 1539788031;
  T64 d2 = 1539798132;
  put_value64(d1,k1);
  put_value64(d2,k2);
  T64 c1 = mk_value64(k1);
  printf("c1: %ld == d1: %ld\n", c1, d1);
  T64 c2 = mk_value64(k2);
  assert(c1 == d1);
  assert(c2 == d2);
  int ret1 = add_data64(t1, k1, 4, v1);
  assert(ret1 == 4);
  int ret2 = add_data64(t1, k2, 4, v1);
  int ret3 = add_data64(t1, k2, 4, v2);

  char buf[100];
  buf[8] = '\0';
  int ret4 = find_data64(t1, k2, 100, buf);
  assert(ret4 == 2);
  printf("ret = %s\n", buf);

  destroy_index64(t1);

  return 0;
}

int main(int argc, char **argv)
{
  cpp_test32();
  cpp_test64();
  c_test32();
  c_test64();
  return 0;
}
