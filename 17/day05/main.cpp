#include <cstdlib>

#include "common.h"

int count_jumps(int* jumps, int n, bool part2 = false)
{
	int pos = 0;
	int steps = 0;
	while (pos >= 0 && pos < n) {
		int jump = jumps[pos];
		if (jump >= 3 && part2) {
			jumps[pos]--;
		}
		else {
			jumps[pos]++;
		}
		pos += jump;

		steps++;
	}

	return steps;
}

int main()
{
	int part_one_result, part_two_result;

	std::vector<std::string> lines = AOC::get_lines("input.txt");

	int* jumps1 = new int[lines.size()];
	int* jumps2 = new int[lines.size()];

	for (unsigned int i = 0; i < lines.size(); i++) {
		jumps1[i] = strtol(lines[i].c_str(), nullptr, 10);
		jumps2[i] = strtol(lines[i].c_str(), nullptr, 10);
	}

	AOC::Timer::start();

	part_one_result = count_jumps(jumps1, lines.size());
	part_two_result = count_jumps(jumps2, lines.size(), true);

	AOC::Timer::stop();

	AOC::output_result(part_one_result, part_two_result, AOC::Timer::elapsed());

	delete [] jumps1;
	delete [] jumps2;
}
