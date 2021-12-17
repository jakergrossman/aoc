#include <iostream>

#include <cstdlib>

#include "common.h"

int part_one(std::string input) {
    int sum = 0;
    int len = input.length();
    int i;
    for (i = 0; i < len; i++) {
        // circular connections
        if (input[i] == input[(i+1) % len]) {
            sum += static_cast<int>(input[i]) - 48;
        }
    }

    return sum;
}

int part_two(std::string input) {
    int sum = 0;
    int len = input.length();
    int i;
    for (i = 0; i < len; i++) {
        // circular connections
        if (input[i] == input[(i+len/2) % len]) {
            sum += static_cast<int>(input[i]) - 48;
        }
    }

    return sum;
}

int main() {
    std::vector<std::string> lines = AOC::get_lines("input.txt");

    if (lines.size() < 1) {
        std::cerr << "Malformed input, expected at least one line." << std::endl;
        std::exit(1);
    }

    std::string input = lines[0];

    int part_one_result, part_two_result;

    AOC::Timer::start();

    part_one_result = part_one(input);
    part_two_result = part_two(input);

    AOC::Timer::stop();

    AOC::output_result(part_one_result, part_two_result, AOC::Timer::elapsed());
}
