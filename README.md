# Virus Spread

m x n grid

0 empty

1 healthy unmasked

2 healthy masked

3 sick

4-directionally adjacent

Every minute, unmasked person becomes sick.

Every two minutes, masked person becomes sick.

Return minimum number of minutes to infect all occupants

If impossible, return -1.

m == grid.length

n == grid[i].length

1 <= m, n <= 10

grid[i][j] is 0, 1, 2 or 3