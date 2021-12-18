#pragma once

#include <iostream>
#include <chrono>
#include <string>
#include <vector>

using namespace std::chrono;

namespace AOC {
    namespace Timer {
        extern high_resolution_clock::time_point start_;
        extern high_resolution_clock::time_point stop_;
        high_resolution_clock::time_point start();
        high_resolution_clock::time_point stop();
        duration<double> elapsed();
        std::string human_readable(duration<double>);
    }

    template <typename T, typename U>
    void output_result(T part_one, U part_two, duration<double> elapsed);

    std::vector<std::string> get_lines(const char* filename);
}

template <typename T, typename U>
void AOC::output_result(T part_one, U part_two, duration<double> elapsed)
{
    std::cout << "Part 1: " << part_one << std::endl;
    std::cout << "Part 2: " << part_two << std::endl;
    std::cout << "Time: " << AOC::Timer::human_readable(elapsed) << std::endl;
}
