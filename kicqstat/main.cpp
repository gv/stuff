#include <QApplication>
#include "kicqstat.h"


MainTab::MainTab(QWidget *parent):
	QWidget(parent)
{
	// get me a table here
	QVBoxLayout *floors = new QVBoxLayout();
	setLayout(floors);
	words = new QTableWidget(0, 3);
	QStringList labels;
	labels << tr("Out") << tr("In") << tr("Word");
	words->setHorizontalHeaderLabels(labels);
	floors->addWidget(words);
}

StopTab::StopTab(QWidget *parent):
	QWidget(parent)
{


}


MainForm::MainForm()
  :QDialog(0)
{
  resize(400, 300);
  
	// here we will setup layouts and tabs
  tabs = new QTabWidget();
	tabs->addTab(new MainTab(), tr("General"));
	tabs->addTab(new StopTab(), tr("Stopwords"));

	QVBoxLayout *floors = new QVBoxLayout();
	floors->addWidget(tabs);
	setLayout(floors);
	setWindowTitle(tr("Icq Stats will be here"));
}


int main(int argc, char *argv[]) {
  QApplication app(argc, argv);
  MainForm form;
  return form.exec();
}
