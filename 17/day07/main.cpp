#include <map>
#include <vector>
#include <sstream>
#include <algorithm>
#include <set>

#include "common.h"

using std::string;
using std::map;

std::set<string> parents, children;
map<string, unsigned int> program_weights;
map<string, std::vector<string> > child_table; // key-value pairs of parents -> children

map<string, unsigned int> tower_costs; // cost of a tower whose base is the key

void process_input(std::vector<string> lines) {
    for (auto const& line : lines) {
        std::stringstream stream (line);
        // register this program
        string parent_id;
        string value;

        stream >> parent_id;
        stream >> value;

        unsigned int cost = strtol(value.substr(1).c_str(), nullptr, 10);
        program_weights[parent_id] = cost;

        string child_id;

        if (stream.peek() != EOF) {
            // has children
            stream >> child_id; // throw away '->'
            while (stream >> child_id) {

                // remove comma
                child_id.erase(std::remove(child_id.begin(), child_id.end(), ','), child_id.end());

                // register child with parent
                if (child_table.find(parent_id) == child_table.end()) {
                    // no other children yet, create vector
                    child_table[parent_id] = std::vector<string>();
                    child_table[parent_id].push_back(child_id);
                }
                else {
                    child_table[parent_id].push_back(child_id);
                }

                children.insert(child_id);
            }
        }
        parents.insert(parent_id);
    }
}

unsigned int tower_cost(string base)
{
    if (tower_costs.find(base) != tower_costs.end()) {
        // already calculated this cost
        return tower_costs[base];
    }
    else if (child_table.find(base) != child_table.end()) {
        // find cost of children
        unsigned int sum = 0;
        for (auto child : child_table[base]) {
            sum += tower_cost(child);
        }
        sum += program_weights[base];
        tower_costs[base] = sum;
        return sum;
    } else {
        // leaf node
        tower_costs[base] = program_weights[base];
        return program_weights[base];
    }
}

int part_two(string base)
{
    // assumes valid input, so doesn't check if
    // children exist, since we should never branch
    // where that becomes the case
    string current = base;
    int previous_difference;

    while (true) {
        if (child_table[current].size() == 1) {
            // only child
            current = child_table[current][0];
            continue;
        }

        std::vector<unsigned int> weights;
        for (auto child : child_table[current]) {
            weights.push_back(tower_cost(child));
        }

        if (std::adjacent_find(weights.begin(), weights.end(), std::not_equal_to<unsigned int>()) == weights.end()) {
            // every weight is even, this is the unbalanced node
            return program_weights[current] + previous_difference;
        } else if (child_table[current].size() == 2) {
            // pick the one that brings us to correct weight
            unsigned int a = program_weights[child_table[current][0]];
            unsigned int b = program_weights[child_table[current][1]];
            if (a + previous_difference == b) {
                current = child_table[current][0];
            } else {
                current = child_table[current][1];
            }
        } else {
            // at least 3 elements
            string a = child_table[current][0];
            string b = child_table[current][1];
            string c = child_table[current][2];
            if (tower_cost(a) != tower_cost(b) && tower_cost(a) != tower_cost(c)) {
                // first element is the unbalanced one
                previous_difference = tower_cost(a) - tower_cost(b);
                current = child_table[current][0];
                continue;
            }

            // check rest of elements starting at 1
            for (unsigned int i = 1; i < child_table[current].size(); i++) {
                if (tower_cost(child_table[current][i]) != tower_cost(child_table[current][i-1])) {
                    previous_difference = tower_cost(child_table[current][i-1]) - tower_cost(child_table[current][i]);
                    current = child_table[current][i];
                }
            }
        }
    }
}

int main()
{
    std::vector<string> input = AOC::get_lines("input.txt");

    process_input(input);

    AOC::Timer::start();

    // only element in `parents` not in `children` is the root
    std::set<string> result;
    std::set_difference(parents.begin(), parents.end(), children.begin(), children.end(),
            std::inserter(result, result.end()));

    string root = *(result.begin());
    int part_two_result = part_two(root);

    AOC::Timer::stop();

    AOC::output_result(root, part_two_result, AOC::Timer::elapsed());
}
