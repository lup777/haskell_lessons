#include "mainwindow.h"
#include "ui_mainwindow.h"

#include <QImage>
#include <QPainter>
#include <QDebug>
#include <QWheelEvent>
#include <QPair>
#include <QRgb>
#include <functional>
#include <QVector>
#include <stdio.h>

MainWindow::MainWindow(QWidget *parent) :
  QMainWindow(parent),
  ui(new Ui::MainWindow)
{
  ui->setupUi(this);
  ui->textEdit->hide();
  ui->fontComboBox->setCurrentText("Nimbus Mono L");
}

void MainWindow::wheelEvent(QWheelEvent *event) {
  if (event->delta() > 0)
    ui->spinBox->setValue(ui->spinBox->value() + 1);
  else
    ui->spinBox->setValue(ui->spinBox->value() - 1);
}

void MainWindow::paintEvent(QPaintEvent *) {
  //qDebug() << "draw to window";
  QPainter p(this);
  QRect r = this->rect();
  QImage img = makeImage();


  //p.drawImage((r.width() / 2) - (img.rect().width() / 2), (r.height() / 2) - (img.rect().height() / 2), makeImage());
  p.drawImage(QRect(0, 85,this->width(), this->height() - 85), img);
}

QImage MainWindow::makeImage(void) {
  QFont font = ui->fontComboBox->currentFont();
  font.setPointSize(ui->spinBox->value());

  QFontMetrics fm(font);
  QString txt = ui->lineEdit->text();
  QSize size = fm.size(Qt::TextSingleLine, txt);

  //QImage img(size, QImage::Format_Mono);
  int new_x = ((size.width() + 7) / 8) * 8;
  QImage img(QSize(new_x,size.height()), QImage::Format_Mono);
  img.fill(QColor(0, 0, 0, 255));


  {
    //qDebug() << "draw to imgae";
    QPainter p(&img);
    p.setFont(font);
    p.drawText(img.rect(), Qt::AlignCenter, ui->lineEdit->text());
  }
  ui->label->setText(QString::number(new_x) + ":" + QString::number(img.size().height()));
  return img.mirrored(true, false);
}

QImage MainWindow::makeImage(char ch) {
  QFont font = ui->fontComboBox->currentFont();
  font.setPointSize(ui->spinBox->value());

  QFontMetrics fm(font);
  QString txt = QString(ch);
  QSize size = fm.size(Qt::TextSingleLine, txt);

  //QImage img(size, QImage::Format_Mono);
  int new_x = ((size.width() + 7) / 8) * 8;
  QImage img(QSize(new_x,size.height()), QImage::Format_Mono);
  img.fill(QColor(0, 0, 0, 255));
  ui->label->setText(QString::number(new_x) + ":" + QString::number(img.size().height()));

  {
    //qDebug() << "draw to imgae";
    QPainter p(&img);
    p.setFont(font);
    p.drawText(img.rect(), Qt::AlignCenter, ui->lineEdit->text());
  }
  return img.mirrored(true, false);
}

MainWindow::~MainWindow()
{
  delete ui;
}

void MainWindow::on_spinBox_valueChanged(const QString &arg1)
{
    this->update();
}

void MainWindow::on_fontComboBox_currentFontChanged(const QFont &f)
{
    this->update();
}

void MainWindow::on_lineEdit_textChanged(const QString &arg1)
{
    this->update();
}


void MainWindow::on_pushButton_clicked()
{
  if (ui->textEdit->isVisible()) {
    ui->textEdit->hide();
    return;
  }

  ui->textEdit->move(0,50);
  ui->textEdit->resize(this->size().width(), this->size().height() - 100);

  //QImage img = makeImage();
  QImage img = makeImage();

  QVector<unsigned char> buffer = imgToBytesArray(img);

  QString code;

  int new_x = ((img.size().width() + 7) / 8) * 8;
  code += QString("0x") + QString::number(new_x, 16) + QString(", ");
  code += QString("0x") + QString::number(img.size().height(), 16) + QString(", ");

  for(int i = 0; i < buffer.size(); i++) {
    if (i != 0)
      code += ", ";
    code += "0x";
    code += QString::number( ((unsigned char)~(buffer[i])), 16 );
  }
  ui->textEdit->setText(code);
  ui->textEdit->show();

  //MakeFontBytesString();
}


QVector<unsigned char> MainWindow::imgToBytesArray(QImage img) {

  std::function<QPair<int,int>(int pixel)>  pixel_to_byte = [] (int pixel) {
    int byte = pixel / 8;
    int shift = 7 - (pixel - (byte * 8));
    return QPair<int, int>(byte, shift); // byte index and index for left shift
    };

  auto set_pixel = [] (unsigned char* ch, int pixel_shift) {
    unsigned char mask = (0x01 << pixel_shift);
    return *ch |= mask;
    };


  int w_bytes = pixel_to_byte(img.width() - 1).first + 1;
  int h_bits = img.height();
  qDebug() << w_bytes << " : " << h_bits;

  size_t size = w_bytes * h_bits;
  //unsigned char* buffer = new unsigned char[size];

  QVector<unsigned char> buffer;

  buffer.fill(0x00, size);

  for(int line = 0; line < img.height(); line++) {
     for (int x = 0; x < img.width(); x++) {

        QPair<int,int> i = pixel_to_byte(x);
        //qDebug() << "for pixel: " << i << " (" << (i.first + (w_bytes * line));

        size_t byte_idx = i.first + (w_bytes * line);

        if (byte_idx <= size) {
          unsigned char* p = buffer.data() + byte_idx;

          //qDebug() << img.pixel(x,line);
          if (img.pixel(x,line) == 4278190080) {
            //qDebug() << "set pixel: " << i << " (" << (i.first + (w_bytes * line));
            set_pixel(p, i.second);
          }
        } else {
          //qDebug() << "out of buffer " << i.first * line;
        }
      }
    }

  return exchange_cols(buffer, w_bytes);
}

QVector<unsigned char> MainWindow::exchange_cols (QVector<unsigned char> in, int line_width_bytes) {
    QVector<unsigned char> res;
    int lines_num = in.size() / line_width_bytes;

    for (int line = 0; line < lines_num; line++) {
        for (int col = line_width_bytes - 1; col >= 0; col -- ) {

            int id = (line * line_width_bytes) + col;

            res.push_back( in[id] );
        }

    }
    return res;
}

QString MainWindow::MakeFontBytesString(void) {
  for (int i = 0; i < 256; i++) {
    //imgToBytesArray(makeImage());
      //printf("CHAR: %c", (unsigned char)i);
  }
  return QString();
}
