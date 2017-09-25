// lab3.cpp
#include <iostream>
#include <fstream>
#include <vector>

using namespace std;
typedef signed long long int int64_t;

template<typename T>
void show_vector(vector<T>& v)
{
    typename vector<T>::iterator it;
    for(it =  v.begin(); it != v.end(); ++it)
    {
        cout << *it << " ";
    }
    cout << endl;
}

vector<int> isort(vector<int64_t>& v);

template<typename T>
void vector_to_stream(vector<T> array, ofstream& ofs)
{
    for(int i = 0; i < array.size(); ++i)
    {
        if(i < array.size() - 1)
        {
            ofs << array[i] << " ";
        }
        else
        {
            ofs << array[i];
        }
    }
    ofs << endl;
}

int main()
{
    ifstream ifs;
    ofstream ofs;
    vector<int64_t> array;
    int amount;
    
    ifs.open("input.txt", ifstream::in);
    ofs.open("output.txt", ofstream::out);

    ifs >> amount;

    for(int i = 0; (i < amount) && (!ifs.eof()); ++i)
    {
        int64_t tmp;
        ifs >> tmp;
        array.push_back(tmp);
    }

    vector<int> indexes = isort(array);

    vector_to_stream<int>(indexes, ofs);
    vector_to_stream<int64_t>(array, ofs);

    return 0;
}


vector<int> isort(vector<int64_t>& v)
{
    int index;
    int64_t elem;
    vector<int> new_indexes;

    new_indexes.push_back(1);
    if(v.size() <= 1)
        return new_indexes;

    
    for(int j = 1; j < v.size(); j++)
    {
        int index = j + 1;
        for(int i = j; i > 0; --i)
        {
            if(v[i-1] > v[i])
            {
                swap(v[i-1], v[i]);
                index = i;
            }
        }
        new_indexes.push_back(index);
    }
    return new_indexes;
}
