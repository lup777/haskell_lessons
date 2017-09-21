// algo.cpp

#include <iostream>
#include <ctime>
#include <string>
#include <cstdlib>

using namespace std;

void fillWithRandomInts(int* arr, int len, int min, int max );
void sort_by_insertions(int* arr, int len);
void show_array(int* arr, int len);

int main (){
  {
    int len = 100;
    int arr[len];
    fillWithRandomInts(arr, len/*len*/, 0/*min*/, /*max*/100);
    sort_by_insertions(arr, len);
    show_array(arr, len);
  }

  
  
  return 0;
}

void sort_by_insertions(int *arr, int len) {
  // сортировка  вставками
  int i, j;
  for(i = len - 2; i > 0 ; --i)
    for(j = i; j < len; ++j)
      if( arr[j-1] > arr[j])
        swap(arr[j-1], arr[j]);
}







void show_array(int* arr, int len) {
  string str_out;
  for(int i = 0; i < len; ++i) {
    str_out += to_string(arr[i]);
    
    if(i != len) {
      str_out += string(", ");
    }
  }
  cout << "array: " << str_out << endl;
}

void fillWithRandomInts(int* arr, int len, int min, int max ) {
  string str_out;
  srand( time(0) );
  
  for(int i = 0; i < len; ++i) {
    arr[i] = min + rand() % max;
    str_out += to_string(arr[i]);
    
    if(i != len) {
      str_out += string(", ");
    }
  }
  
  cout << "random array generated: " << str_out << endl;
}
