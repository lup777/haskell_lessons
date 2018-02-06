#ifndef CNODE_H
#define CNODE_H
#include <QSharedPointer>
#include <QColor>

class CNode
{
public:
    CNode(int key, int value, int x, int y, QSharedPointer<CNode> parent);
    CNode(int key, int value, int x, int y);
    CNode(QSharedPointer<CNode> parent);
    ~CNode();
    int Key(void);
    int Value(void);
    QSharedPointer<CNode> MakeLeft(int key, int value, QSharedPointer<CNode>);
    QSharedPointer<CNode> MakeRight(int key, int value, QSharedPointer<CNode>);
    int X();
    int Y();
    void SetX(int x);
    void SetY(int y);
    QColor Color(void);
    void SetColor(QColor);
    bool IsTerminator(void);
    QSharedPointer<CNode> Left();
    QSharedPointer<CNode> Right();
    QSharedPointer<CNode> Parent();
    int VLevel();
    void SetVLevel(int level);
    QSharedPointer<CNode> GetRoot();
    void MoveAllFromLevel(int level, QSharedPointer<CNode> node);
    CNode();

private:

    QSharedPointer<CNode> left_, right_;
    int key_;
    int value_;
    bool b_terminator_;
    int x_;
    int y_;
    QColor color_;
    QSharedPointer<CNode> parent_;
    int v_level_; // vertical level
};

#endif // CNODE_H
