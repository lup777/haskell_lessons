#ifndef TREE_H
#define TREE_H
#include <QPair>
#include <QSharedPointer>
#include <functional>

#include "node.h"


class Tree
{
public:
  Tree();
  PNode Add(PNode node);
  PNode Add(int value);
  PNode Add(int value, QPoint position);
  void Apply(std::function<void(PNode)> f);

 private:
  static void Apply(PNode node, std::function<void(PNode)> f);
  static void Insert(PNode parent, PNode node);
  static PNode terminator_;
  PNode root_;
};

#endif // TREE_H
