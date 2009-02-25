#include <regex.h>
#include <string>
#include <vector>
#include <map>
#include <set>

using std::string;
using std::vector;
using std::map;

struct WordStat {
	unsigned in, out;
};

struct MsgInfo {
	char *body, *from, *to;
};
	

// All data will lie here
class History {
	struct Msg {
		string body;
		string *to, *from;
	};

	set<string> subjects_;
	typedef map<string, WordStat> Index;
	Index index_;
	// XXX how do i sort them
	//vector <Msg> messages;

 public:
	void addMessage(MsgInfo *message);

};

// main program will be here
class IcqStat {
	History history_;
 public:
	IcqStat();
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
	

	
	
	
	


	
