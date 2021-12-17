#include <cmath>
#include <cstring>

#include "common.h"

#define TARGET 277678
#define GRID_SIZE 1024

unsigned int part_one(unsigned int input)
{
	if (input == 1) { return 0; }

	// find smallest odd square > input
	unsigned int odd_square = 1;
	while (odd_square*odd_square < input) { odd_square += 2; }

	// odd square position
	int x1 = odd_square/2, y1 = odd_square/2;

	// target position
	unsigned int target = odd_square*odd_square;
	int x2 = x1, y2 = y1;

	int dx = -1, dy = -1;

	for (unsigned int i = 0; i < 2; i++) {
		if (target == input) { break; }

		// traverse x
		for (unsigned int j = 0; j < odd_square && target != input; j++) {
			target--;
			x2 += dx;
		}

		// traverse y
		for (unsigned int j = 0; j < odd_square && target != input; j++) {
			target--;
			y2 += dy;
		}

		// turn around
		dx *= -1;
		dy *= -1;
	}

	unsigned int distance = std::abs(x2) + std::abs(y2);

	return distance;
}

unsigned int neighbor_sum(unsigned int x, unsigned int y, unsigned int grid[GRID_SIZE][GRID_SIZE])
{
	unsigned int i, j;
	unsigned int sum = 0;
	for (j = y-1; j <= y+1; j++) {
		for (i = x-1; i <= x+1; i++) {
			if (i > 0 && i < GRID_SIZE && j > 0 && j < GRID_SIZE) {
				sum += grid[j][i];
			}
		}
	}

	return sum;
}

unsigned int part_two(unsigned int input)
{
	unsigned int grid[GRID_SIZE][GRID_SIZE];
	memset(grid, 0, GRID_SIZE * GRID_SIZE * sizeof(unsigned int));

	unsigned int x = GRID_SIZE / 2, y = GRID_SIZE / 2;
	unsigned int dx = 1, dy = -1;
	unsigned int arm_length = 1;

	grid[y][x] = 1;

	while (grid[y][x] <= input) {
		// navigate x component of arm
		for (unsigned int i = 0; i < arm_length; i++) {
			x += dx;
			grid[y][x] = neighbor_sum(x, y, grid);
			if (grid[y][x] > input) { return grid[y][x]; }
		}

		// navigate y component of arm
		for (unsigned int i = 0; i < arm_length; i++) {
			y += dy;
			grid[y][x] = neighbor_sum(x, y, grid);
			if (grid[y][x] > input) { return grid[y][x]; }
		}

		arm_length++;
		dx *= -1;
		dy *= -1;
	}

	return grid[y][x];
}

int main()
{
	unsigned int part_one_result, part_two_result;

	AOC::Timer::start();

	part_one_result = part_one(TARGET);
	part_two_result = part_two(TARGET);

	AOC::Timer::stop();

	AOC::output_result(part_one_result, part_two_result, AOC::Timer::elapsed());
}
