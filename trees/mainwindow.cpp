#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "cnode.h"
#include "ctree.h"
#include <QDebug>
#include <QString>
#include <QMouseEvent>
#include <functional>
#include <QSharedPointer>

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
    , tree_(100, 1000, (this->width() / 2) - 10, 20)
    , node_rect_(QRect(0, 0, 26, 26))
    , selected_node_(QSharedPointer<CNode>(new CNode()))
{
    ui->setupUi(this);

}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::paintEvent(QPaintEvent *event)
{
    Q_UNUSED(event);

    std::function<void(QSharedPointer<CNode>)> draw_line = [this](QSharedPointer<CNode> node){
        QPainter painter(this); // Создаём объект отрисовщика
        // Устанавливаем кисть абриса
        painter.setPen(QPen(Qt::black, 1, Qt::SolidLine, Qt::FlatCap));

        int x = CorrectX(node->Key());

        painter.setBrush(QBrush(node->Color(), Qt::SolidPattern));

        if(!node->Parent().isNull())
        {
            int parent_x = CorrectX(node->Parent()->Key());

            painter.drawLine(x + (node_rect_.width() / 2),
                            node->Y() + (node_rect_.width() / 2),
                            parent_x + (node_rect_.width() / 2),
                            node->Parent()->Y() + (node_rect_.width() / 2));
        }
    };

    std::function<void(QSharedPointer<CNode>)> draw_lines = [this, &draw_line, &draw_lines](QSharedPointer<CNode> node){
        if(!node->IsTerminator()) {
            draw_line(node);
            draw_lines(node->Left());
            draw_lines(node->Right());
        }
    };

    draw_lines(tree_.Root());
    DrawAllNode(tree_.Root());
}

void MainWindow::DrawAllNode(QSharedPointer<CNode> node) {
    //qDebug() << "DrawAllNode >>>";

    if(node->IsTerminator())
        return;

    DrawNode(node);
    DrawAllNode(node->Left());
    DrawAllNode(node->Right());
}

void MainWindow::DrawNode(QSharedPointer<CNode> node) {

    QPainter painter(this); // Создаём объект отрисовщика
    // Устанавливаем кисть абриса
    painter.setPen(QPen(Qt::black, 1, Qt::SolidLine, Qt::FlatCap));

    int x = CorrectX(node->Key());

    painter.setBrush(QBrush(node->Color(), Qt::SolidPattern));
    painter.drawEllipse(x, node->Y(), node_rect_.width(), node_rect_.height());
    node->SetX(x);
    painter.drawText(QRect(x+ 2, node->Y()+5, x + 17, node->Y()),
                     QString::number(node->Key()));
}

void MainWindow::on_pushButton_clicked() //add
{
    qDebug() << "SBOX value: " << ui->spinBox->value();
    QSharedPointer<CNode> created_node = tree_.Add(ui->spinBox->value(), ui->spinBox->value() * 10, tree_.Root());
    tree_.GetNodesOnLevel(created_node->Y());
    this->repaint();
}

void MainWindow::mouseMoveEvent (QMouseEvent * event) {
    /*if(!selected_node_->IsTerminator()) {
        selected_node_->SetX(event->x());
        selected_node_->SetY(event->y());
        this->repaint();
    }*/
}

void MainWindow::mouseReleaseEvent( QMouseEvent * event) {
    selected_node_ = QSharedPointer<CNode>(new CNode());
}

void MainWindow::mousePressEvent( QMouseEvent * event){
    int x = event->x();
    int y = event->y();

    QSharedPointer<CNode> selected;
    std::function<bool(QSharedPointer<CNode>&)> cmp = [&x, &y, this](QSharedPointer<CNode>& node){
        int node_x = node->X();
        if((x > node_x) && (x <= node_x + 50)) {
            if((y > node->Y()) && (y <= node->Y() + 50)) {
                return true;
            }
        }
        return false;
    };
    if( tree_.Find(cmp, selected) ) {
        qDebug() << "seceted: " << selected->Key();
        selected_node_ = selected;
        if(selected->Color() == Qt::green) {
            selected->SetColor(Qt::red);
        } else {
            selected->SetColor(Qt::green);
        }
    }
    this->repaint();
}

float MainWindow::CorrectX(int x) {
    float left_field = 10;
    float right_field = 10;
    static float fields = left_field + right_field;

    float step = (float)(tree_.MaxKey() - tree_.MinKey() + fields) / (float)(this->width());

    if(tree_.MinKey() < 0) {
        x += abs(tree_.MinKey());
    }

    return x / step;
}
