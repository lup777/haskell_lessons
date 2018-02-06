#ifndef CTREE_H
#define CTREE_H
#include "cnode.h"
#include <functional>


class CTree
{
public:
    CTree();
    CTree(int key, int value, int x, int y);
    QSharedPointer<CNode> Root(void);
    QSharedPointer<CNode> Add(int key, int value, QSharedPointer<CNode> next);
    int MinKey();
    int MaxKey();
    QVector<int> GetNodesOnLevel(int level);
    bool Find(std::function<bool(QSharedPointer<CNode>&)>& f, QSharedPointer<CNode>& result);

private:
    QSharedPointer<CNode> root_;
    QSharedPointer<CNode> terminator_node_;
    int min_key_;
    int max_key_;
};

#endif // CTREE_H
