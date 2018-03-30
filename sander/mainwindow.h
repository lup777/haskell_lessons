#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QMutex>
#include <QList>
#include <QSharedPointer>

#include "node.h"
#include "tree.h"

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
    Tree tree_;
};

#endif // MAINWINDOW_H
