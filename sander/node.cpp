#include "node.h"

Node::Node()
  : is_terminator_(true){
}

Node::Node(int value)
  : pos_(0, 0)
  , radius_(10, 10)
  , value_(value)
  , is_terminator_(false) {
}

QPoint Node::Position() {
  return pos_;
}

void Node::SetPosition(QPoint pos) {
  pos_ = pos;
}
