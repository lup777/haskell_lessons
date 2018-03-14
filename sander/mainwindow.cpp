#include "mainwindow.h"
#include "ui_mainwindow.h"
#include <QImage>
#include <QPainter>
#include <QColor>
#include <QPaintEvent>

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
    buffer_painter.drawLine(this->rect().x(), this->rect().y(), this->rect().width(), this->rect().height());
    //end draw

    image_buffer_ = buffer;
    mutex_.unlock();
}
