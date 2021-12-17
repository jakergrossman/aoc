#include <iostream>
#include <fstream>

#include "common.h"

int part_one()
{
	return -1;
}


int part_two()
{
	return -1;
}

int main()
{
	int part_one_result, part_two_result;

	AOC::Timer::start();

	part_one_result = part_one();
	part_two_result = part_two();

	AOC::Timer::stop();

	AOC::output_result(part_one_result, part_two_result, AOC::Timer::elapsed());
}
