#include <iostream>
#include <vector>
#include <climits>
#include <sstream>

#include "common.h"

using std::vector;

int part_one(vector<vector<int> > input) {
    vector<int> differences(input.size());
    for (unsigned int i = 0; i < input.size(); i++) {
        int min = INT_MAX, max = INT_MIN;
        for (unsigned int j = 0; j < input[i].size(); j++) {
            if (input[i][j] < min) min = input[i][j];
            if (input[i][j] > max) max = input[i][j];
        }
        differences.push_back(max - min);
    }

    int sum = 0;
    for (unsigned int i = 0; i < differences.size(); i++) {
        sum += differences[i];
    }
    return sum;
}

int part_two(vector<vector<int> > input) {
    vector<int> quotients(input.size());
    unsigned int i, j, k;
    unsigned int dividend, divisor;
    for (i = 0; i < input.size(); i++) {
        for (j = 0; j < input[i].size(); j++) {
            for (k = 0; k < input[i].size(); k++) {
                dividend = input[i][j];
                divisor = input[i][k];
                if (j != k && divisor != 0 && dividend % divisor == 0) {
                    quotients.push_back(dividend / divisor);
                    break;
                }
            }
        }
    }

    int sum = 0;
    for (unsigned int i = 0; i < quotients.size(); i++) {
        sum += quotients[i];
    }
    return sum;
}

int main() {
    std::vector<std::string> lines = AOC::get_lines("input.txt");

    vector<vector<int> > input;
    for (unsigned int i = 0; i < lines.size(); i++) {
        std::stringstream stream(lines[i]);
        vector<int> line_elements;
        int buf;
        while (stream >> buf) {
            line_elements.push_back(buf);
        }
        input.push_back(line_elements);
    }

    int part_one_result, part_two_result;

    AOC::Timer::start();

    part_one_result = part_one(input);
    part_two_result = part_two(input);

    AOC::Timer::stop();

    AOC::output_result(part_one_result, part_two_result, AOC::Timer::elapsed());
}
