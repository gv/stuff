#include <QApplication>
#include "kicqstat.h"
#include "history.h"

/*
	This is our whole program with windows tables and indexes.

	XXX how do we run this from some dll 
*/	
class KicqStat: public IcqStat, public QApplication {
	MainForm *mainForm_;

	KicqStat(int argc, char **argv):
		QApplication(argc, argv)
	{
	}

	// for init
	void bitch(const char *what) {
		
	}

	int init2() {
		// we don't need a form if we can't load data
		if(!init())
			return 0;
		
		// now we need form
		mainForm_ = new MainForm(this);

		// fill it
		unsigned rowCnt = 0;
		for(History::WordIndex::iterator p = history_.begin();
				p != history_.end(); p++) {
			QTableWidgetItem *i = new QTableWidgetItem(p->first);
			mainForm_->mainTab_->words.setItem(0, rowCnt, i);
			rowCnt++;
		}
	}
};
		
/*
	Main tab shows words table
*/
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

/*
	This tab shows stopwords
*/
StopTab::StopTab(QWidget *parent):
	QWidget(parent)
{


}

/*
	This form unites tabs
*/
MainForm::MainForm(IcqStat *prog)
  :QDialog(0),
	 prog_(prog)
{
  resize(400, 300);
  
	// here we will setup layouts and tabs
  tabs = new QTabWidget();
	mainTab_ = new MainTab();
	stopTab_ = new StopTab();
	tabs->addTab(mainTab_, tr("General"));
	tabs->addTab(stopTab_, tr("Stopwords"));

	QVBoxLayout *floors = new QVBoxLayout();
	floors->addWidget(tabs);
	setLayout(floors);
	setWindowTitle(tr("Icq Stats"));
}


/*
	I'll think about this it could be great
*/
int main(int argc, char *argv[]) {
	KicqStat prog(argc, argv);
  return prog.exec();
}
