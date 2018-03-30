#include "mainwindow.h"
#include "ui_mainwindow.h"
#include <QImage>
#include <QPainter>
#include <QColor>
#include <QPaintEvent>
#include <QDebug>

#include "node.h"

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
}

MainWindow::~MainWindow()
{
    delete ui;
}

void MainWindow::paintEvent(QPaintEvent *event)
{
    setImage();

    QPainter window_painter(this);

    mutex_.lock();
    window_painter.drawImage(event->rect(), image_buffer_, image_buffer_.rect());
    mutex_.unlock();
}

void MainWindow::setImage()
{
    mutex_.lock();
    QImage buffer(this->width(), this->height(), QImage::Format_Mono);

    QPainter buffer_painter(&buffer); // Создаём объект отрисовщика
    // Устанавливаем кисть абриса
    QPen pen(Qt::black);
    pen.setCapStyle(Qt::RoundCap);
    pen.setWidth(1);
    pen.setColor(Qt::red);
    buffer_painter.setPen(pen);

    QBrush brush;
    brush.setColor(Qt::red);
    buffer_painter.setBrush(brush);

    buffer_painter.fillRect(this->rect(), Qt::white );

    //start drawing

    tree_.Apply([&buffer_painter](PNode node) {
      buffer_painter.drawEllipse(node->Position(), node->Radius().x(), node->Radius().y());
      qDebug() << "tree_.Apply(f)";
    });
    //end draw

    tree_.Apply([](PNode node) {
      if (node->IsTerminator()) {
        qDebug() << node->Value() << "is terminator";
      } else {
        qDebug() << node->Value() << "is not terminator";
      }
    });

    image_buffer_ = buffer;
    mutex_.unlock();
}
