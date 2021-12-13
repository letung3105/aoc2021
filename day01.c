#include <stdio.h>
#include <stdlib.h>

const int ARRAY_MAX_SZ = 10000; // 1e4

int count_increments(int numbers[], int numbers_sz) {
  int count = 0;
  for (int i = 0; i < numbers_sz - 1; i++)
    if (numbers[i] < numbers[i + 1])
      count++;
  return count;
}

int rolling_sum_three(int numbers[], int numbers_sz) {
  numbers_sz -= 2;
  for (int i = 0; i < numbers_sz; i++)
    numbers[i] += numbers[i + 1] + numbers[i + 2];
  return numbers_sz;
}

int main() {
  int numbers[ARRAY_MAX_SZ];
  int numbers_sz = 0;
  int n, ret;
  while (ret = scanf("%d\n", &n), ret == 1 || ret == 2 || ret == 3)
    numbers[numbers_sz++] = n;

  int sol1 = count_increments(numbers, numbers_sz);
  printf("%d\n", sol1);

  numbers_sz = rolling_sum_three(numbers, numbers_sz);
  int sol2 = count_increments(numbers, numbers_sz);
  printf("%d\n", sol2);

  return 0;
}
