#-------------------------------------------------
#
# Project created by QtCreator 2018-02-02T15:42:33
#
#-------------------------------------------------

QT       += core gui
CONFIG   += c++11

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = trees
TEMPLATE = app


SOURCES += main.cpp\
        mainwindow.cpp \
    cnode.cpp \
    ctree.cpp

HEADERS  += mainwindow.h \
    cnode.h \
    ctree.h

FORMS    += mainwindow.ui
