#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QPainter>
#include "ctree.h"
#include <QSharedPointer>
#include <QRect>
#include <QSpinBox>
#include <QPoint>

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QWidget *parent = 0);
    ~MainWindow();

    void DrawNode(QSharedPointer<CNode> node);
    void DrawAllNode(QSharedPointer<CNode> node);

protected:
    void paintEvent(QPaintEvent *event);
    void mouseMoveEvent( QMouseEvent * event);
    void mousePressEvent( QMouseEvent * event);
    void mouseReleaseEvent( QMouseEvent * event);

private slots:
    void on_pushButton_clicked();

private:
    Ui::MainWindow *ui;
    CTree tree_;
    QSharedPointer<CNode> selected_node_;
    QRect node_rect_;
    QPoint mouse_click_point_;

    float CorrectX(int x);
};

#endif // MAINWINDOW_H
