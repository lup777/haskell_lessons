// algo.cpp

#include <iostream>
#include <fstream>
#include <ctime>
#include <string>
#include <cstdlib>
#include <list>

using namespace std;
class ItemList;
class Item;

void fillWithRandomInts(int* arr, int len, int min, int max );
ItemList isort(ItemList il);
ItemList insert(Item i, ItemList il);
ItemList read_input();

#define Int int

class Item
{
public:
  Item(long long int i)
    {
      m_i = i;
    }

  Item& operator= (const Item& object)
    {
      if(this != &object)
      {
        m_i = object.m_i;
      }
      else
      {}
      
      return *this;
    }
  
  friend ostream& operator<<(ostream& os, const Item& i)
    {
      os << i.m_i;
      return os;
    }
  
  friend const bool operator == (const Item &left, const Item &right)
    {
      return left.m_i == right.m_i;
    }

  friend const bool operator != (const Item &left, const Item &right)
    {
      return left.m_i != right.m_i;
    }

  friend const bool operator < (const Item &left, const Item &right)
    {
      return left.m_i < right.m_i;
    }

  friend const bool operator > (const Item &left, const Item &right)
    {
      return left.m_i > right.m_i;
    }

  friend const bool operator <= (const Item &left, const Item &right)
    {
      return left.m_i <= right.m_i;
    }

  friend const bool operator >= (const Item &left, const Item &right)
    {
      return left.m_i >= right.m_i;
    }

  Item(const Item& object)
    {
      m_i = object.m_i;
    }

  Item(const Item&& object)
    {
      m_i = object.m_i;
    }
  
  long long int m_i;
};

class ItemList
{
public:
  ItemList()
    {
      
    }

    ItemList(Item i)
    {
      m_il.push_back(i);
    }

  ItemList(const ItemList& obj)
    {
      //cout << "ItemList copy constructor" << endl;
      m_il = obj.m_il;
    }

  ItemList& operator= (const ItemList& object)
    {
      //cout << "copy operator" << endl;
      if(this != &object)
      {
        m_il = object.m_il;
      }
      else
      {}
      
      return *this;
    }

  ItemList(const ItemList&& obj)
    {
      //cout << "ItemList move constructor" << endl;
      m_il = obj.m_il;
    }

  friend ItemList cons(Item i, ItemList il)
    {
      ItemList new_il(il);
      new_il.m_il.push_front(i);
      //cout << "new ItemList size = " << new_il.size() << endl;
      return new_il; //copy
    }

  friend ItemList cons(ItemList il, Item i)
    {
      ItemList new_il(il);
      new_il.m_il.push_back(i);
      //cout << "new ItemList size = " << new_il.size() << endl;
      return new_il; //copy
    }


  friend ItemList cons(ItemList il1, ItemList il2)
    {
      ItemList new_il(il1);
      while(il2.size() > 0)
      {
        new_il.m_il.push_front(il2.m_il.front());
        il2.m_il.pop_front();
      }
      
      //cout << "new ItemList size = " << new_il.size() << endl;
      return new_il; //copy
    }

  friend Item head(ItemList il)
    {
      return *il.m_il.begin();
    }

  friend ItemList tail(ItemList il)
    {
      ItemList new_il(il);
      new_il.m_il.pop_front();
      return new_il;
    }
 
  long long int size()
    {
      return m_il.size();
    }

  friend ostream& operator<<(ostream& os, const ItemList& il)
    {
      ItemList list(il);

      while(list.size() != 0)
      {
        os << head(list);
        list = tail(list);
        if(list.size() != 0)
          os << ", ";
      }

      return os;
    }

  friend ItemList operator += (ItemList& il, const Item& i )
    {
      return cons(i, il);
    }

  list<Item> m_il;
};

int main ()
{
  ItemList il = read_input();
  
  cout << "        " << il << endl;
  cout << "sorted: " << isort(il) << endl;
  
  return 0;
}

ItemList read_input()
{
  ifstream ifs;
  ifs.open("input.txt", ifstream::in);

  int num_of_numbers;
  ItemList il;

  ifs >> num_of_numbers;
  cout << "num_of_numbers: " << num_of_numbers << endl;

  while(!ifs.eof() && il.size() < num_of_numbers)
  {
    long long int tmp;
    ifs >> tmp;
    il = cons(il, Item(tmp));
  }
  return il;
}

ItemList isort(ItemList il)
{
  if(il.size() == 0)
    return ItemList();
  else
    return insert(head(il), isort(tail(il)));
}

ItemList insert(Item i, ItemList il)
{
  if(il.m_il.size() == 0)
    return ItemList(i);
  else if(i > head(il))
    return cons(head(il), insert(i, tail(il)));
  else
    return cons(i, il);
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
