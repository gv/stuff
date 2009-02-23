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
	
private:
	QTabWidget *tabs;
	
};

class MainTab: public QWidget {
  Q_OBJECT

  public:
  MainTab(QWidget *parent = NULL);

private:
	QTableWidget *words;
};

class StopTab: public QWidget {
  Q_OBJECT
  public:
  StopTab(QWidget *parent = NULL);
};



