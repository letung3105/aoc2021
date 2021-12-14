#include <stdint.h>
#include <stdio.h>
#include <string.h>

typedef struct PositionV1 {
  int x, y;
} PositionV1;

typedef struct PositionV2 {
  int x, y, aim;
} PositionV2;

int main() {
  PositionV1 pos1 = {0, 0};
  PositionV2 pos2 = {0, 0, 0};

  char buf[8];
  int n;
  int ret;
  while (ret = scanf("%s %d\n", &buf, &n), ret == 1 || ret == 2 || ret == 3) {
    if (strcmp(buf, "forward") == 0) {
      pos1.x += n;
      pos2.x += n;
      pos2.y += n * pos2.aim;
    } else if (strcmp(buf, "down") == 0) {
      pos1.y += n;
      pos2.aim += n;
    } else if (strcmp(buf, "up") == 0) {
      pos1.y -= n;
      pos2.aim -= n;
    }
  }

  printf("%d\n", pos1.x * pos1.y);
  printf("%d\n", pos2.x * pos2.y);
}
