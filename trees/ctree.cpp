#include "ctree.h"
#include <QSharedPointer>
#include <algorithm>
#include <QDebug>
#include <functional>


CTree::CTree()
    : root_(QSharedPointer<CNode>())
    , min_key_(100)
    , max_key_(min_key_)
{
}

CTree::CTree(int key, int value, int x, int y)
    : root_(QSharedPointer<CNode>(new CNode(key, value, x, y)))
    , min_key_(key)
    , max_key_(key)
{
    /*QSharedPointer<CNode> left, right;
    left = root_->MakeLeft(50, 500, root_);
    right = root_->MakeRight(150, 1500, root_);

    left->MakeLeft(25, 250, left);
    left = left->MakeRight(75, 750, left);
    left->MakeRight(87, 870, left);

    right->MakeLeft(125, 1500, right);*/
    Add(0, 10, root_);
    Add(-3, -30, root_);
    Add(50, 500, root_);
    Add(75, 750, root_);
    Add(25, 250, root_);
    Add(30, 200, root_);
    Add(20, 200, root_);
    Add(200, 2000, root_);
    Add(150, 1500, root_);
    Add(190, 1900, root_);
    Add(160, 1600, root_);
    Add(180, 1800, root_);
    Add(170, 1700, root_);
    Add(60, 600, root_);
    Add(85, 850, root_);
}

QSharedPointer<CNode> CTree::Root(void) {
    return root_;
}

int CTree::MinKey() {
    return min_key_;
}

int CTree::MaxKey(){
    return max_key_;
}

QSharedPointer<CNode> CTree::Add(int key, int value, QSharedPointer<CNode> node) {
    min_key_ = std::min(min_key_, key);
    max_key_ = std::max(max_key_, key);
    qDebug() << "min key = " << min_key_;
    qDebug() << "max key = " << max_key_;

    if(key > node->Key()) {
        if(node->Right()->IsTerminator()) {
            return node->MakeRight(key, value, node);
        }
        return Add(key, value, node->Right());
    }
    else if(key < node->Key()) {
        if(node->Left()->IsTerminator()) {
            return node->MakeLeft(key, value, node);
        }
        return Add(key, value, node->Left());
    }
    return node;
}

QVector<int> CTree::GetNodesOnLevel(int level_) {
    QVector<int>  keys_vect;
    std::function<void(QSharedPointer<CNode>, int)> get_all_lieves_on_level =
            [&get_all_lieves_on_level, &keys_vect](QSharedPointer<CNode> node, int level) {
        if(node->IsTerminator())
            return;

        if(node->Y() == level) {
            keys_vect.push_back(node->Key());
            qDebug() << "pushed: " << node->Key();
            return;
        }

        get_all_lieves_on_level(node->Left(), level);
        get_all_lieves_on_level(node->Right(), level);
    };
    get_all_lieves_on_level(root_, level_);
    return keys_vect;
}

bool CTree::Find(std::function<bool(QSharedPointer<CNode> &)> &f, QSharedPointer<CNode>& result) {
    bool bresult = false;
    std::function<void(QSharedPointer<CNode>)> search = [&bresult, &f, &search, &result](QSharedPointer<CNode> node){
        if(!node->IsTerminator()) {
            if(f(node)) {
                result = node;
                bresult = true;
                return;
            }
            search(node->Left());
            search(node->Right());
        }
    };
    search(root_);
    return bresult;
}
