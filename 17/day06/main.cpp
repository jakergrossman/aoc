#include <map>
#include <sstream>
#include <algorithm>

#include "common.h"

using std::vector;

void distribute(vector<int> &blocks) {
    auto it = std::max_element(blocks.begin(), blocks.end());
    auto pos = it - blocks.begin();
    int value = *it;
    *it = 0;

    for (auto i = 0; i < value; i++) {
        blocks[(pos + i + 1) % blocks.size()]++;
    }
}

std::pair<unsigned int, unsigned int> count_loop(vector<int> blocks)
{
    unsigned int cycles = 0;
    std::map<vector<int>, int> seen;

    seen[blocks] = 0;
    distribute(blocks);
    cycles++;

    while (seen.find(blocks) == seen.end()) {
        seen[blocks] = cycles;
        distribute(blocks);

        cycles++;
    }

	return { cycles, cycles - seen[blocks] };
}

int main()
{
    vector<std::string> lines = AOC::get_lines("input.txt");

    vector<int> input;
    for (unsigned int i = 0; i < lines.size(); i++) {
        std::stringstream stream(lines[i]);
        int buf;
        while (stream >> buf) {
            input.push_back(buf);
        }
    }


	AOC::Timer::start();

    std::pair<unsigned int, unsigned int> result = count_loop(input);

	AOC::Timer::stop();

    AOC::output_result(result.first, result.second, AOC::Timer::elapsed());
}
