#include "cnode.h"
#include <QSharedPointer>
#include <QDebug>
#include <QColor>

CNode::CNode(int key, int value, int x, int y, QSharedPointer<CNode> parent)
    : key_(key)
    , value_(value)
    , b_terminator_(false)
    , x_(x)
    , y_(y)
    , color_(Qt::green)
    , parent_(parent)
    , dx_(0)
    , dy_(0)
{
    qDebug()  << "new Node(" << key << ", " << value << ")";
    if(b_terminator_ == true)
        return;
    if(!parent_.isNull()) {
        v_level_ = parent_->VLevel();
    }

    left_ = QSharedPointer<CNode>(new CNode());
    right_ = QSharedPointer<CNode>(new CNode());
}

CNode::CNode(int key, int value, int x, int y)
    : key_(key)
    , value_(value)
    , b_terminator_(false)
    , x_(x)
    , y_(y)
    , color_(Qt::green)
    , v_level_(0)
{
    qDebug()  << "new Node(" << key << ", " << value << ")";
    if(b_terminator_ == true)
        return;

    left_ = QSharedPointer<CNode>(new CNode());
    right_ = QSharedPointer<CNode>(new CNode());
}

CNode::CNode()
    : b_terminator_(true)
{
    qDebug()  << "new Node(terminator)";
}

CNode::~CNode() {
    if(b_terminator_ == true)
        qDebug()  << "~Node(terminator)";
    else
        qDebug()  << "~Node";
}

int CNode::Key(void) {
    return key_;
}

int CNode::Value(void) {
    return value_;
}

QSharedPointer<CNode> CNode::MakeLeft(int key, int value, QSharedPointer<CNode> parent ) {
    left_ = QSharedPointer<CNode>(new CNode(key, value, key, y_ + 50, parent));
    left_->SetVLevel(v_level_ - 1);
    MoveAllFromLevel(v_level_, GetRoot());
    return left_;
}

QSharedPointer<CNode> CNode::MakeRight(int key, int value, QSharedPointer<CNode> parent){
    right_ = QSharedPointer<CNode>(new CNode(key, value, key, y_ + 50, parent));
    right_->SetVLevel(v_level_ + 1);
    MoveAllFromLevel(v_level_, GetRoot());
    return right_;
}

int CNode::X() {
    return x_;
}
int CNode::Y() {
    return y_;
}

void CNode::SetX(int x) {
    x_ = x;
}
void CNode::SetY(int y) {
    y_ = y;
}

QColor CNode::Color(void) {
    return  color_;
}
bool CNode::IsTerminator(void) {
    return b_terminator_;
}

QSharedPointer<CNode> CNode::Left(){
    return left_;
}

QSharedPointer<CNode> CNode::Right() {
    return right_;
}

QSharedPointer<CNode> CNode::Parent() {
    return parent_;
}

void CNode::SetColor(QColor color) {
    color_ = color;
}

int CNode::VLevel() {
    return v_level_;
}

void CNode::SetVLevel(int level) {

    v_level_ = level;
}

QSharedPointer<CNode> CNode::GetRoot() {
    QSharedPointer<CNode> parent = parent_;

    if(parent.isNull()) return parent;

    for(;;) {
        if(parent->Parent().isNull())
            break;
        parent = parent->Parent();
    }
    return parent;
}

void CNode::MoveAllFromLevel(int level, QSharedPointer<CNode> node) {
    if(node.isNull())
        return;

    if(node->IsTerminator())
        return;

    if((level > 0 && node->VLevel() > 0) ||
       (level < 0 && node->VLevel() < 0))
    {
        if(abs(node->VLevel()) >= abs(level))
        {
            node->SetVLevel(node->VLevel() + level);
            MoveAllFromLevel(level, node->Left());
            MoveAllFromLevel(level, node->Right());
        }
    }
}

int CNode::Dx() {
    return dx_;
}

void CNode::SetDx(int dx) {
    dx_ = dx;
}

int CNode::Dy() {
    return dy_;
}

void CNode::SetDy(int dy) {
    dy_ = dy;
}
