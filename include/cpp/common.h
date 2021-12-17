#pragma once

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

    template <typename T>
    void output_result(T part_one, T part_two, duration<double> elapsed);

    std::vector<std::string> get_lines(const char* filename);
}
