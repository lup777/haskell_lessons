//sander25.cpp
#include <iostream>
#include <fstream>
#include <string>
#include <unistd.h>
#include <stdexcept>

//using namespace std;

template< class T>
class Maybe {
 public:
  Maybe()
    : value_exists_(false) {}

  Maybe(T value)
    : value_(value)
    , value_exists_(true) {}

  Maybe(const Maybe<T>& from) {
    value_exists_ = from.Just();
    if (value_exists_) {
      value_ = from.Get();
    }
  }

  T Get() const {
    return value_;
  }

  bool Just() const {
    return value_exists_;
  }

  bool Nothing() const {
    return !Just();
  }

  Maybe<T>& operator = (const Maybe<T>& from) {
    value_exists_ = from.Just();
    if (value_exists_) {
      value_ = from.Get();
    }
    return *this;
  }

 private:
  bool value_exists_;
  T value_;
};

Maybe<int> MaybeConvertToInt(const std::string str) {
#if 1
  try {
    return Maybe<int>(std::stoi(str));
  }
  catch (const std::invalid_argument& ia) {
    //cout << "error: invalid_argument: " << ia.what() << endl;
  }
  catch (const std::out_of_range& oor) {
    //cout << "error: out_of_range: " << oor.what() <<  endl;
  }
  catch (std::exception& e) {
    //cout << "error: other: " << e.what() << endl;
  }
  
  return Maybe<int>();
#else
  return Maybe<int>(std::stoi(str));
#endif
}

Maybe<int> maybeGetIntFromFile(const char* fname) {
  std::string line;
  std::ifstream inf(fname);
  if (access(fname, R_OK) == 0) {
    if (inf.is_open()) {
      std::getline(inf, line);
    
      return MaybeConvertToInt(line);
    
    } else {
      //cout << "dailed to open file" << endl;
    }
  } else {
    //cout << "file does not exists" << endl;
  }
  return Maybe<int>();
}

int main() {
  Maybe<int> number;
  number = maybeGetIntFromFile("test2.txt");

  if (number.Just()) {
    std::cout << "test2.txt -> " << number.Get() << std::endl;
  } else {
    std::cout << "test2.txt -> failed" << std::endl;
  }
  return 0;
}
