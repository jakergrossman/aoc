#include <sstream>
#include <map>
#include <string>
#include <algorithm>

#include "common.h"

using std::vector;
using std::string;

bool validate_duplicates(string phrase)
{
	std::stringstream stream(phrase);
	string word;
	std::map<string, bool> seen_words;

	while (stream >> word) {
		if (seen_words.find(word) != seen_words.end()) {
			// already seen this word
			return false;
		}
		seen_words[word] = true;
	}

	return true;
}

unsigned int part_one(vector<string> phrases)
{
	unsigned int valid_passwords = 0;
	for (auto it = phrases.begin(); it < phrases.end(); it++) {
		if (validate_duplicates(*it)) {
			valid_passwords++;
		}
	}
	return valid_passwords;
}

bool is_anagram(string a, string b) {
	std::sort(a.begin(), a.end());
	std::sort(b.begin(), b.end());
	return a.compare(b) == 0;
}

bool validate_anagrams(string phrase) {
	std::stringstream stream(phrase);
	string word;
	vector<string> words;
	while (stream >> word) { words.push_back(word); }

	for (auto a = words.begin(); a < words.end(); a++) {
		for (auto b = words.begin(); b < words.end(); b++) {
			if (a != b && is_anagram(*a, *b)) {
				return false;
			}
		}
	}

	return true;
}

unsigned int part_two(vector<string> phrases)
{
	unsigned int valid_passwords = 0;
	for (auto it = phrases.begin(); it < phrases.end(); it++) {
		if (validate_anagrams(*it)) {
			valid_passwords++;
		}
	}
	return valid_passwords;
}

int main()
{
	int part_one_result, part_two_result;

	vector<string> lines = AOC::get_lines("input.txt");

	AOC::Timer::start();

	part_one_result = part_one(lines);
	part_two_result = part_two(lines);

	AOC::Timer::stop();

	AOC::output_result(part_one_result, part_two_result, AOC::Timer::elapsed());
}
