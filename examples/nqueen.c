// How to run:
//
//   $ ./xcc examples/nqueen.c && ./a.out

#include "../lib/crt0.c"
#include "util.c"

void print_board(int board[][8]) {
  int i, j;
  for (i = 0; i < 8; i++) {
    for (j = 0; j < 8; j++)
      if (board[i][j])
	puts("Q ");
      else
	puts(". ");
    puts("\n");
  }
  puts("\n\n");
}

int conflict(int board[][8], int row, int col) {
  int i;
  for (i = 0; i < row; i++) {
    if (board[i][col])
      return 1;
    int j = row - i;
    if (0 < col - j + 1 && board[i][col - j])
      return 1;
    if (col + j < 8 && board[i][col + j])
      return 1;
  }
  return 0;
}

void solve(int board[][8], int row) {
  if (row > 8 - 1) {
    print_board(board);
    return;
  }
  int i;
  for (i = 0; i < 8; i++) {
    if (conflict(board, row, i)) {
    } else {
      board[row][i] = 1;
      solve(board, row + 1);
      board[row][i] = 0;
    }
  }
}

int main() {
  int board[8][8];
  int i, j;
  for (i = 0; i < 8; ++i)
    for (j = 0; j < 8; ++j)
      board[i][j] = 0;
  solve(board, 0);
  return 0;
}
