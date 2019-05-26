#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>

namespace Ui {
  class MainWindow;
}

class MainWindow : public QMainWindow
{
  Q_OBJECT

public:
  explicit MainWindow(QWidget *parent = 0);
  void paintEvent(QPaintEvent *);
  void wheelEvent(QWheelEvent *event);
  ~MainWindow();

  QImage makeImage(void);
  QImage makeImage(char ch);
  QVector<unsigned char> imgToBytesArray(QImage img);
  QString MakeFontBytesString(void);
  QVector<unsigned char> exchange_cols (QVector<unsigned char> in, int line_width_bytes);

private slots:
  void on_spinBox_valueChanged(const QString &arg1);

  void on_fontComboBox_currentFontChanged(const QFont &f);

  void on_lineEdit_textChanged(const QString &arg1);

  void on_pushButton_clicked();

private:
  Ui::MainWindow *ui;
};

#endif // MAINWINDOW_H
