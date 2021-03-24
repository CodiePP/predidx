/*
 * build objects: $ make swi
 * build test program: 
 * $ g++ -o test++ -Isrc test/test.cpp obj-Linux/predidx-c.o obj-Linux/predidx-cpp.o
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


int main(int argc, char **argv)
{
  cpp_test32();
  cpp_test64();
  c_test32();
  return 0;
}
