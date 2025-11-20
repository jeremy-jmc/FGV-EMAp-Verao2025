#include "csv_reader.h"
#include <fstream>
#include <sstream>
#include <iostream>
#include <algorithm>

CSVReader::CSVReader(const std::string& filename) : filename(filename) {
    read_data();
}

void CSVReader::read_data() {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Could not open the file " << filename << std::endl;
        return;
    }

    std::string line;
    // Skip header
    if (std::getline(file, line)) {
        // Header is skipped
    }

    while (std::getline(file, line)) {
        std::vector<std::string> row;
        std::stringstream ss(line);
        std::string value;
        while (std::getline(ss, value, ',')) {
            row.push_back(value);
        }
        data.push_back(row);
    }
    file.close();
}

std::vector<std::vector<std::string>> CSVReader::getData() {
    // The first row in the CSV is the depot, but the python code reads it as a normal client and then extracts it.
    // To replicate that, I need to find the depot row and put it at the beginning.
    // The problem is the python code uses the original index to identify the depot.
    // The CSV file has an 'id' column. Let's assume the depot is the one with id 0.
    
    std::vector<std::vector<std::string>> reordered_data;
    std::vector<std::string> depot_row;

    // Find and remove depot row
    auto it = std::remove_if(data.begin(), data.end(),
        [&](const std::vector<std::string>& row) {
            if (std::stoi(row[0]) == 0) {
                depot_row = row;
                return true;
            }
            return false;
        });
    data.erase(it, data.end());

    if (!depot_row.empty()) {
        reordered_data.push_back(depot_row);
    }

    for(const auto& row : data){
        reordered_data.push_back(row);
    }

    return reordered_data;
}
