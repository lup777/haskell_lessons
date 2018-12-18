// math_parse.cpp
#include <iostream>
#include <algorithm>
#include <string>
#include <regex>
#include <vector>

using namespace std;

#define FIRST "1 AND (AA OR (BB AND 0) AND FF OR (CC XOR DD AND ZZ))"

string operators[4] = {"AND", "OR", "XOR", "NOT"};
vector<string> data_section;
vector<string> code_section;
vector<string> instructions;
string assembler;

vector<string> split(string str, string delim) {
  vector<string> result;
  std::smatch m;

  while(regex_search (str, m, regex("(\\S+)\\s+(.*)"))) {
    if(m.length() > 1) {
      result.push_back(m[1]);
      str = m[2];
    }
  }

  return result;
}

vector<size_t> get_levels(vector<string>& v) {
  vector<size_t> levels;
  levels.resize(v.size(), 0);

  size_t lvl = 0;
  for(size_t i = 0; i < v.size(); i++) {
    if(v[i].compare("{") == 0) {
      lvl ++;
    } else if (v[i].compare("}") == 0) {
      lvl --;
    }
    levels[i] = lvl;
  }
  return levels;
}

bool eval_level_once(vector<string>& v);
size_t get_max_level(vector<string>& v);
void show_vector(vector<string>& v);
bool is_operator(string str);
bool is_a_brace(string str);
bool clean_braces_once(vector<string>& v);
void clean_braces(vector<string>& v);
bool is_variable(string str);
void fill_code_section(string op, string left, string right, string result);

int main() {
  string str = string(FIRST);

  size_t pos = string::npos;
  while((pos = str.find("(", 0)) != string::npos) {
    if (pos != string::npos) {
      str = str.substr(0, pos) +
        string(" { ") +
        str.substr(pos + 1, str.length());
    }
  }


  while((pos = str.find(")", 0)) != string::npos) {
    if (pos != string::npos) {
      str = str.substr(0, pos) +
        string(" } ") +
        str.substr(pos + 1, str.length());
    }
  }


  vector<string> v = split(str, string(" "));

  size_t level = get_max_level(v);

  assembler = ".CODE\n\n";

  while(eval_level_once(v)) {
    show_vector(v);
    clean_braces(v);
    show_vector(v);
  }

  level = get_max_level(v);

  while(eval_level_once(v)) {
    show_vector(v);
    clean_braces(v);
    show_vector(v);
  }

  string data = ".DATA\n\n";
  for(auto x: data_section) {
    data += x + " DW ?\n";
  }
  data += "\n\n";

  assembler = data + "\n" + assembler;

  cout << "data_section: " << endl;
  show_vector(data_section);

  cout << "instructions: " << endl;
  show_vector(instructions);

  cout << "ASSEMBLER:" << endl;
  cout << assembler << endl;

  cout << "\n";
  return 0;
}

void show_vector(vector<string>& v) {
  cout << "vector: ";
  for(auto x: v) {
    cout << " " << x;
  }
  cout << "\n";
}

void clean_braces(vector<string>& v) {
  while(clean_braces_once(v)) {}
}

bool clean_braces_once(vector<string>& v) {
  if (v.size() < 2) {
    cout << "v.size < 2\n";
    return false;
  }
  for(auto it = v.begin() + 1; it != v.end() - 1; it++) {
    if (is_a_brace(*(it-1))) {
      if (is_a_brace(*(it+1))) {
        //cout << "erase: " << *(it-1) << " - " << *(it+1) << endl;
        *(it-1) = *it;
        v.erase(it);
        v.erase(it);
        return true;
      }
    }
  }
  return false;
}

bool is_a_brace(string str) {
  if (str.compare("{") == 0)
    return true;
  else if (str.compare("}") == 0)
    return true;

  return false;
}

bool is_operator(string str) {
  for(auto x: operators) {
    if (str.compare(x) == 0)
      return true;
  }
  return false;
}

size_t get_max_level(vector<string>& v) {
  vector<size_t> levels = get_levels(v);
  return *(max_element(levels.begin(), levels.end()));
}

bool eval_level_once(vector<string>& v) {
  vector<size_t> levels = get_levels(v);
  size_t level = get_max_level(v);

  static size_t tmp = 0;
  bool result = false;

  for(size_t i = 0; i < v.size(); i++) {
    if (levels[i] == level) {
      if (is_operator(v[i])) {

        string result_location = string("X_") + to_string(tmp);
        fill_code_section(v[i], v[i-1], v[i+1], result_location);

        v[i-1] = result_location;

        //data_section.push_back(v[i-1] + " DB ?\n");
        data_section.push_back(v[i-1]);

        v.erase(v.begin() + i);
        v.erase(v.begin() + i);

        tmp ++;
        return true;
      }
    }
  }
  return false;
}

bool is_variable(string str) {
  for(auto x: data_section)
    if (x.compare(str) == 0)
      return true;
  return false;
}

void fill_code_section(string op, string left, string right, string result) {
  string str = "";
  string cmd;
  if (op.compare("AND") == 0) {
    cmd = string("and");
  } else if(op.compare("OR") == 0) {
    cmd = string("or");
  } else if(op.compare("XOR") == 0) {
    cmd = string("xor");
  } else if(op.compare("NOT") == 0) {
    cmd = string("not");
  }
  cout << result << " = "
       << left << " " << op << " " << right << endl;

  if (is_variable(left))
    str += "mov ax, DWORD PTR [" + left + "]\n";
  else
    str += "mov ax, " + left + "\n";

  if((right.compare("1") == 0) ||
     (right.compare("0") == 0))
    str += cmd + " ax, " + right + "\n";
  else
    str += cmd + " ax, DWORD PTR [" + right + "]\n";

  str += "mov DWORD PTR [" + result + "], ax\n\n";

  assembler += str;
  code_section.push_back(str);
}
