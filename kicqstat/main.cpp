#include <QApplication>
#include <QFont>
#include <QPushButton>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QToolBox>
#include <QTableWidget>
#include <QDialog>

class MainForm: public QDialog {
  Q_OBJECT
  
  public:
  MainForm();
};

class MainTab: public QWidget {
  Q_OBJECT

  public:
  MainTab(QWidget *parent);
};

class StopTab: public QWidget {
  Q_OBJECT
  public:
  StopTab(QWidget *parent);
};




MainForm::MainForm()
  :QDialog(0)
{
  resize(400, 300);
  // here we will setup layouts and tabs
  
}


int main(int argc, char *argv[]) {
  QApplication app(argc, argv);
  MainForm form;
  return form.exec();
}
