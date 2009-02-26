#include <regex.h>
#include <string>
#include <vector>
#include <map>
#include <set>

using std::string;
using std::vector;
using std::map;


/*
	input data for addMessage
*/
struct MsgInfo {
	char *body, *from, *to;
};
	

/*
	Counts for (Word, Person)
*/
struct WordStat {
	unsigned in, out;
WordStat(): in(0), out(0);
};

/*
	Complete index
*/
class History {
	typedef string Person;
	typedef string Word;
 public:
	typedef map<Person*, WordStat> WordIndex;
	// all words will be stored in a map
	typedef map<Word, WordStat> Index;

	Index index_;
	set<Person> persons_; // stores names of persons who write and read messages
	
	/*
		struct Msg {
		string body;
		string *to, *from;
		};
		vector <Msg> messages;
	*/
	
 public:
	void addMessage(MsgInfo *message);
		
};

// main program will be here
class IcqStat {
 protected:
	vector<string> stopWords;
	History history_;

 public:
	IcqStat();
	int init();
	virtual int bitch(char* what);
}

// These are data needed to process qip files.
class QipHistoryParser {
	regex_t inHdrPattern_, outHdrPattern_;
 public:
	QipHistoryParser(History *);
 ~QipHistoryParser():
	int parseFile(FILE *fp);
};
	

	
	
	
	


	
