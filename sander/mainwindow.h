#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QMutex>

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = 0);
    void paintEvent(QPaintEvent *event);
    void setImage();
    ~MainWindow();

private:
    Ui::MainWindow *ui;
    QMutex mutex_;
    QImage image_buffer_;
};

#endif // MAINWINDOW_H
