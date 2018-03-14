#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "cnode.h"
#include "ctree.h"
#include <QDebug>
#include <QString>
#include <QMouseEvent>
#include <functional>
#include <QSharedPointer>
#include <QImage>

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
    , tree_(100, 1000, 100, 50)
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
    QRect rect = this->rect();
    QImage buffer = drawImage(rect);
    QPainter p(this);
    p.drawImage(buffer.rect(), buffer, this->rect());
}

QImage MainWindow::drawImage(QRect rect)
{
    QImage image(rect.width(), rect.height(), QImage::Format_RGB16);

    QPainter painter(&image); // Создаём объект отрисовщика
        // Устанавливаем кисть абриса
    painter.setPen(QPen(Qt::black, 1, Qt::SolidLine, Qt::FlatCap));
    painter.setBrush(QBrush(Qt::red, Qt::SolidPattern));

    std::function<void(QSharedPointer<CNode>)> draw_line = [this, &image, &painter](QSharedPointer<CNode> node){
        int x = node->X() + node->Dx();
        int y = node->Y() + node->Dy();

        if(!node->Parent().isNull())
        {
            int parent_x = node->Parent()->X() + node->Parent()->Dx();
            int parent_y = node->Parent()->Y() + node->Parent()->Dy();

            painter.drawLine(x + (node_rect_.width() / 2),
                            y + (node_rect_.width() / 2),
                            parent_x + (node_rect_.width() / 2),
                            parent_y + (node_rect_.width() / 2));
        }
    };

    std::function<void(QSharedPointer<CNode>)> draw_lines = [this, &draw_line, &draw_lines](QSharedPointer<CNode> node){
        if(!node->IsTerminator()) {
            draw_line(node);
            draw_lines(node->Left());
            draw_lines(node->Right());
        }
    };

    std::function<void(QSharedPointer<CNode>)> draw_node = [this, &painter](QSharedPointer<CNode> node) {

        int x = node->X() + node->Dx();
        int y = node->Y() + node->Dy();

        painter.setBrush(QBrush(node->Color(), Qt::SolidPattern));
        painter.drawEllipse(x, y, node_rect_.width(), node_rect_.height());
        painter.drawText(QRect(x+ 2, y+5, x + 17, y),
                         QString::number(node->Key()));
    };

    std::function<void(QSharedPointer<CNode>)> draw_all_node =
            [this, &draw_node, &draw_all_node](QSharedPointer<CNode> node) {
        //qDebug() << "DrawAllNode >>>";

        if(node->IsTerminator())
            return;

        draw_node(node);
        draw_all_node(node->Left());
        draw_all_node(node->Right());
    };


    tree_.ApplyForAll([this](QSharedPointer<CNode> node) {
      if (!node.isNull()) {
        int tree_middle = this->tree_.MinKey() + this->tree_.MaxKey() / 2;
        int window_middle = this->width() / 2;
        node->SetDx(window_middle - tree_middle);
      }
    });

    painter.fillRect(this->rect(), Qt::white );
    draw_lines(tree_.Root());
    draw_all_node(tree_.Root());
    return image;
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

    int x = CorrectX(node->X()) + node->Dx();
    int y = node->Y() + node->Dy();

    painter.setBrush(QBrush(node->Color(), Qt::SolidPattern));
    painter.drawEllipse(x, y, node_rect_.width(), node_rect_.height());
    painter.drawText(QRect(x+ 2, y+5, x + 17, y),
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
   if(!selected_node_->IsTerminator()) {
        //selected_node_->SetDx(event->x() - mouse_click_point_.x());
         //selected_node_->SetX(event->x());
        //selected_node_->SetDy(event->y() - mouse_click_point_.y());
         //selected_node_->SetY(event->y());
        //qDebug() << "Dx = " << selected_node_->Dx();
        this->repaint();
    }
}

void MainWindow::mouseReleaseEvent( QMouseEvent * event) {
    selected_node_ = QSharedPointer<CNode>(new CNode());
}

void MainWindow::mousePressEvent( QMouseEvent * event){
    int x = event->x();
    int y = event->y();


    QSharedPointer<CNode> selected;
    std::function<bool(QSharedPointer<CNode>&)> cmp = [&x, &y, this](QSharedPointer<CNode>& node){
        int node_x = node->X() + node->Dx();
        int node_y = node->Y() + node->Dy();
        if((x > node_x) && (x <= node_x + node_rect_.width())) {
            if((y > node_y) && (y <= node_y + node_rect_.height())) {
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
    mouse_click_point_ = QPoint(x, y);
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
