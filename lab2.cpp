// lab2.cpp
#include <iostream>
#include <fstream>

using namespace std;

int main()
{
    ifstream ifs;
    ofstream ofs;
    long long int num1, num2;
    
    ifs.open("input.txt", ifstream::in);
    ofs.open("output.txt", ofstream::out);

    ifs >> num1 >> num2;
    ofs << num1 + (num2 * num2);
        
    return 0;
}
