#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#define STACK_SIZE 1000
#define MAX_NODES 100
#define MAX_NAME_LENGTH 6 // "start" is the longest name in the input

// get the idx of the cave given its name
int caveIdx(char name[MAX_NAME_LENGTH],
            char caves_names[MAX_NODES][MAX_NAME_LENGTH], int caves_count) {
  for (int i = 0; i < caves_count; i++) {
    if (strcmp(name, caves_names[i]) == 0)
      return i;
  }
  return -1;
}

// check if the cave is a small cave
bool isSmall(char name[MAX_NAME_LENGTH]) {
  if (strcmp(name, "start") == 0 || strcmp(name, "end") == 0)
    return false;
  for (int i = 0; i < strlen(name); i++) {
    if (!islower(name[i]))
      return false;
  }
  return true;
}

int explore1(int cave_idx, int visits_count[MAX_NODES],
             int adjacent_count[MAX_NODES],
             int adjacent_caves[MAX_NODES][MAX_NODES],
             char caves_names[MAX_NODES][MAX_NAME_LENGTH]) {
  if (strcmp(caves_names[cave_idx], "end") == 0)
    // count one when found a path to "end"
    return 1;

  int num_paths = 0;
  for (int i = 0; i < adjacent_count[cave_idx]; i++) {
    int adj_cave_idx = adjacent_caves[cave_idx][i];
    if (strcmp(caves_names[adj_cave_idx], "start") == 0)
      // do not revisit "start"
      continue;
    if (isSmall(caves_names[adj_cave_idx]) && visits_count[adj_cave_idx] > 0)
      // small caves can only be visited once
      continue;
    // recursively count the number of paths to "end"
    visits_count[adj_cave_idx]++;
    num_paths += explore1(adj_cave_idx, visits_count, adjacent_count,
                          adjacent_caves, caves_names);
    visits_count[adj_cave_idx]--;
  }
  return num_paths;
}

int explore2(int cave_idx, int visits_count[MAX_NODES],
             int adjacent_count[MAX_NODES],
             int adjacent_caves[MAX_NODES][MAX_NODES],
             char caves_names[MAX_NODES][MAX_NAME_LENGTH], int caves_count) {
  if (strcmp(caves_names[cave_idx], "end") == 0)
    // count one when found a path to "end"
    return 1;

  int num_paths = 0;
  for (int i = 0; i < adjacent_count[cave_idx]; i++) {
    int adj_cave_idx = adjacent_caves[cave_idx][i];
    if (strcmp(caves_names[adj_cave_idx], "start") == 0)
      // do not revisit "start"
      continue;
    if (isSmall(caves_names[adj_cave_idx]) && visits_count[adj_cave_idx] > 0) {
      // found a small cave that has been visited
      bool canVisit = true;
      for (int j = 0; j < caves_count; j++) {
        if (isSmall(caves_names[j]) && visits_count[j] > 1) {
          // only one small cave can be visited twice
          canVisit = false;
          break;
        }
      }
      if (!canVisit)
        continue;
    }
    // recursively count the number of paths to "end"
    visits_count[adj_cave_idx]++;
    num_paths += explore2(adj_cave_idx, visits_count, adjacent_count,
                          adjacent_caves, caves_names, caves_count);
    visits_count[adj_cave_idx]--;
  }
  return num_paths;
}

int part01(int adjacent_count[MAX_NODES],
           int adjacent_caves[MAX_NODES][MAX_NODES],
           char caves_names[MAX_NODES][MAX_NAME_LENGTH], int caves_count) {
  int start_idx = caveIdx("start", caves_names, caves_count);
  int visits_count[MAX_NODES] = {0};
  return explore1(start_idx, visits_count, adjacent_count, adjacent_caves,
                  caves_names);
}

int part02(int adjacent_count[MAX_NODES],
           int adjacent_caves[MAX_NODES][MAX_NODES],
           char caves_names[MAX_NODES][MAX_NAME_LENGTH], int caves_count) {
  int start_idx = caveIdx("start", caves_names, caves_count);
  int visits_count[MAX_NODES] = {0};
  return explore2(start_idx, visits_count, adjacent_count, adjacent_caves,
                  caves_names, caves_count);
}

int main() {
  int caves_count = 0;
  int adjacent_count[MAX_NODES] = {0};
  int adjacent_caves[MAX_NODES][MAX_NODES];
  char caves_names[MAX_NODES][MAX_NAME_LENGTH];

  int ret;
  char buf1[MAX_NAME_LENGTH];
  char buf2[MAX_NAME_LENGTH];

  // read the caves names and assign a number to the name so we can store
  // the graph as an adjacency list using a 2d array.
  while (ret = scanf("%[a-zA-Z]-%[a-zA-Z]\n", &buf1, &buf2),
         ret == 1 || ret == 2 || ret == 3) {
    int idx_head = caveIdx(buf1, caves_names, caves_count);
    if (idx_head == -1) {
      idx_head = caves_count;
      strcpy(caves_names[caves_count++], buf1);
    }

    int idx_tail = caveIdx(buf2, caves_names, caves_count);
    if (idx_tail == -1) {
      idx_tail = caves_count;
      strcpy(caves_names[caves_count++], buf2);
    }

    adjacent_caves[idx_head][adjacent_count[idx_head]++] = idx_tail;
    adjacent_caves[idx_tail][adjacent_count[idx_tail]++] = idx_head;
  }

  int paths1 = part01(adjacent_count, adjacent_caves, caves_names, caves_count);
  int paths2 = part02(adjacent_count, adjacent_caves, caves_names, caves_count);

  printf("%d\n", paths1);
  printf("%d\n", paths2);
}
