#-------------------------------------------------
#
# Project created by QtCreator 2018-03-14T09:25:56
#
#-------------------------------------------------

QT       += core gui
CONFIG   += c++11

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = sander
TEMPLATE = app


SOURCES += main.cpp\
        mainwindow.cpp \
    node.cpp \
    tree.cpp

HEADERS  += mainwindow.h \
    node.h \
    tree.h

FORMS    += mainwindow.ui
