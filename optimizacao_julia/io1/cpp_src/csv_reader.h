#ifndef CSV_READER_H
#define CSV_READER_H

#include <string>
#include <vector>

class CSVReader {
public:
    CSVReader(const std::string& filename);
    std::vector<std::vector<std::string>> getData();

private:
    std::string filename;
    std::vector<std::vector<std::string>> data;
    void read_data();
};

#endif // CSV_READER_H
