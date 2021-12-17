#include "common.h"

#include <iostream>
#include <fstream>
#include <string>
#include <chrono>

#include <cstdlib>
#include <cmath>

using namespace std::chrono;

high_resolution_clock::time_point AOC::Timer::start_;
high_resolution_clock::time_point AOC::Timer::stop_;

high_resolution_clock::time_point AOC::Timer::start()
{
    AOC::Timer::start_ = time_point_cast<nanoseconds>(high_resolution_clock::now());
    return AOC::Timer::start_;
}

high_resolution_clock::time_point AOC::Timer::stop()
{
    AOC::Timer::stop_ = time_point_cast<nanoseconds>(high_resolution_clock::now());
    return stop_;
}

duration<double> AOC::Timer::elapsed()
{
    duration<double> elapsed_ = duration_cast<duration<double>>(AOC::Timer::stop_ - Timer::start_);
    return elapsed_;
}

std::string AOC::Timer::human_readable(duration<double> duration) {
    auto t = duration.count();
    std::string unit;
    unsigned int magnitude;

    if (t >= 1) {
        unit = "s";
        magnitude = t;
    }
    else if (t >= 0.001) {
        unit = "ms";
        magnitude = t * 1000;
    }
    else if (t >= 0.000001) {
        unit = "μs";
        magnitude = t * 1000000;
    } else {
        unit = "ns";
        magnitude = t * 1000000000;
    }

    return std::to_string((unsigned int)std::floor(magnitude)) + " " + unit;
}

template void AOC::output_result<unsigned int>(unsigned int part_one, unsigned int part_two, duration<double> elapsed);
template void AOC::output_result<int>(int part_one, int part_two, duration<double> elapsed);
template void AOC::output_result<std::string>(std::string part_one, std::string part_two, duration<double> elapsed);

template <typename T>
void AOC::output_result(T part_one, T part_two, duration<double> elapsed)
{
    std::cout << "Part 1: " << part_one << std::endl;
    std::cout << "Part 2: " << part_two << std::endl;
    std::cout << "Time: " << AOC::Timer::human_readable(elapsed) << std::endl;
}

// read lines of input
std::vector<std::string> AOC::get_lines(const char* filename) {
    std::ifstream input_file(filename);
    if (!input_file) {
        std::cerr << "Could not open " << filename << std::endl;
        std::exit(1);
    }

    std::vector<std::string> lines;
    std::string line;
    while (std::getline(input_file, line)) {
        lines.push_back(line);
    }

    return lines;
}
