#include "include/FileProcessor.h"

#include <fstream>  //ifstream, is_open, getline
#include <stdexcept>
#include <string>

namespace sloc {

bool FileProcessor::is_quote(const size_t& str_index, const std::string& line) const {
    if (line[str_index] == '"') {
        // Contar quantos caracteres de escape consecutivos precedem a aspas
        size_t backslashes = 0;
        for (size_t i = str_index; i > 0; --i) {
            if (line[i - 1] == '\\') {
                backslashes++;
            } else {
                break;
            }
        }
        // Se o número de barras invertidas consecutivas for par, a aspas não está escapada
        return backslashes % 2 == 0;
    }
    return false;
}

bool FileProcessor::is_end_of_multiline(const size_t& str_index, const std::string& line) const {
    return is_inside_line(str_index, line) and line[str_index] == multiline_comment_symbol.second[0]
           and line[str_index + 1] == multiline_comment_symbol.second[1];
}

bool FileProcessor::is_start_of_multiline(const size_t& str_index, const std::string& line) const {
    return is_inside_line(str_index, line) and line[str_index] == multiline_comment_symbol.first[0]
           and line[str_index + 1] == multiline_comment_symbol.first[1];
}

bool FileProcessor::is_inline_comment(const size_t& str_index, const std::string& line) const {
    return line[str_index] == inline_comment_symbol[0]
           and line[str_index + 1] == inline_comment_symbol[1];
}

bool FileProcessor::is_inside_line(const size_t& str_index, const std::string& line) const {
    return str_index + 1 < line.size();
}

void FileProcessor::trim(std::string& line) const {
    auto start = line.find_first_not_of(" \t\n\v\f\r");
    auto end = line.find_last_not_of(" \t\n\v\f\r");
    if (start == std::string::npos) {
        line.clear();
    } else {
        line = line.substr(start, end - start + 1);
    }
}

File FileProcessor::process(File& file) const {
    std::ifstream file_stream(file.path);
    std::string line;
    if (!file_stream.is_open()) {
        throw std::runtime_error("Failed to open file: " + file.path);
    }
    Flag flag;
    while (std::getline(file_stream, line)) {
        process_line(flag, file, line);
    }
    file_stream.close();
    return file;
}

void FileProcessor::process_line(Flag& flag, File& file, std::string& line) const {
    trim(line);
    ++file.line_total;
    if (line.empty()) {
        ++file.blank_count;
        return;
    }
    bool processed = false;
    for (size_t i = 0; i < line.length(); ++i) {
        if (is_quote(i, line) and !flag.in_mult) {
            flag.in_str = !flag.in_str;
        }
        if (!flag.in_str and !flag.in_mult and is_inside_line(i, line)) {
            if (is_start_of_multiline(i, line)) {
                flag.in_mult = true;
                file.comment_count++;
            } else if (is_inline_comment(i, line)) {
                if (i != 0) {
                    file.loc_count++;  // To check if there is code before the comment
                }
                file.comment_count++;
                processed = true;
                break;
            }
        } else if (!flag.in_str and flag.in_mult and is_inside_line(i, line)) {
            if (is_end_of_multiline(i, line)) {
                flag.in_mult = false;
            }
        }
    }
    if (flag.in_mult) {
        file.comment_count++;
    }
    if (!flag.in_str and !flag.in_mult and !processed) {
        file.loc_count++;
    }
}
}  // namespace sloc
