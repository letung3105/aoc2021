#include<stdio.h>
#include<stdint.h>
#include<stdbool.h>

void addOne(int grid[10][10]) {
  for (int i = 0; i < 10; i++) {
    for (int j = 0; j < 10; j++) {
      grid[i][j]++;
    }
  }
}

void spreadAt(int grid[10][10], int i, int j) {
  for (int di = -1; di <= 1; di++) {
    for (int dj = -1; dj <= 1; dj++) {
      int ii = i + di;
      int jj = j + dj;
      if (di == 0 && dj == 0
          || ii < 0 || ii > 9
          || jj < 0 || jj > 9
          || grid[ii][jj] == 0) {
        continue;
      }
      grid[ii][jj] += 1;
    }
  }
}

int flash(int grid[10][10]) {
  int flashes = 0;
  for (int i = 0; i < 10; i++) {
    for (int j = 0; j < 10; j++) {
      if (grid[i][j] > 9) {
        grid[i][j] = 0;
        spreadAt(grid, i, j);
        flashes++;
      }
    }
  }
  return flashes;
}

int step(int grid[10][10]) {
  addOne(grid);
  int n;
  int flashes = 0;
  while (n = flash(grid)) flashes += n;
  return flashes;
}

void printGrid(int grid[10][10]) {
  for (int i = 0; i < 10; i++) {
    for (int j = 0; j < 10; j++) {
      printf("%d", grid[i][j]);
    }
    printf("\n");
  }
  printf("===\n");
}

int main() {
  int grid[10][10] = {0};
  for (int i = 0; i < 10; i++) {
    for (int j = 0; j < 10; j++) {
      scanf("%1d", &grid[i][j]);
    }
  }

  int flashes = 0;
  int steps = 0;
  while (++steps) {
    int n = step(grid);
    if (steps <= 100) flashes += n;
    if (n == 100) break;
  }
  printf("%d\n%d\n", flashes, steps);
}

