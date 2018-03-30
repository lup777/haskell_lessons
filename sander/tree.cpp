#include "tree.h"
#include <QDebug>

PNode Tree::terminator_ = QSharedPointer<Node>(new Node());

Tree::Tree()
  : root_(terminator_)
{
  Add(50, QPoint(50, 50));
  Add(70, QPoint(70, 70));
  Add(30, QPoint(30, 30));
}

PNode Tree::Add(PNode node) {
  node->SetRight(terminator_);
  node->SetLeft(terminator_);
  Insert( root_, node);
  return node;
}

PNode Tree::Add(int value) {
  return Add(QSharedPointer<Node>(new Node(value)));
}

PNode Tree::Add(int value, QPoint position) {
  PNode node = Add(QSharedPointer<Node>(new Node(value)));
  node->SetPosition(position);
  return node;
}

void Tree::Insert(PNode parent, PNode node) {
  if (!parent->IsTerminator()) {
    if (node->Value() > parent->Value()) {
      qDebug() << "insert to right";
      Insert(parent->Right(), node);
    } else if (node->Value() < parent->Value()) {
      qDebug() << "insert to left";
      Insert(parent->Left(), node);
    } else {
      // already exists
      qDebug() << "insert exists";
      *parent = *node;
    }
  } else {
    qDebug() << "insert here";
    *parent = *node;
  }
}

void Tree::Apply(std::function<void(PNode)> f) {
  Tree::Apply(root_, f);
}

void Tree::Apply(PNode node, std::function<void(PNode)> f) {
  if (!node->IsTerminator()) {
    qDebug() << "Apply to " << node->Value();
    f(node);
    Tree::Apply(node->Right(), f);
    Tree::Apply(node->Left(), f);
  } else {
    qDebug() << "Apply to terminator";
  }
}
