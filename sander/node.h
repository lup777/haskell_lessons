#ifndef NODE_H
#define NODE_H
#include <QPoint>
#include <QColor>
#include <QSharedPointer>

class Node
{
 public:
  Node(int value);
  Node();

  QPoint Position();

  void SetPosition(QPoint pos);

  QSharedPointer<Node> Right() {
    return right_;
  }

  QSharedPointer<Node> Left() {
    return left_;
  }

  void SetRight(QSharedPointer<Node> node) {
    right_ = node;
  }

  void SetLeft(QSharedPointer<Node> node) {
    left_ = node;
  }

  QPoint Radius() {
    return radius_;
  }

  int Value() {
    return value_;
  }

  friend bool operator !=(Node left, Node right) {
    return left.Value() != right.Value();
  }

  friend bool operator ==(Node left, Node right) {
    return left.Value() == right.Value();
  }

  bool IsTerminator() {
    return is_terminator_;
  }

 private:
  QPoint pos_; //real position
  QColor color_;
  QSharedPointer<Node> right_;
  QSharedPointer<Node> left_;
  QPoint radius_;
  int value_;
  bool is_terminator_;
};

typedef QSharedPointer<Node> PNode;

#endif // NODE_H
