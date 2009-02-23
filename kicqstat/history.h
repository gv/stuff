#include <regex.h>
#include <string>
#include <vector>
#include <map>
#include <set>

using std::string;
using std::vector;
using std::map;

struct Msg {
	string body;
	string *to, *from;
};

struct WordStat {
	unsigned in, out;
};

// All data will lie here
class History {
	set<string> subjects;
	map<string, WordStat> index;
	// XXX how do i sort them
	//vector <Msg> messages;

 public:
	addMessage(Msg *message);

};

// main program will be here
class IcqStat {
	History history;
 public:
	IcqStat();
	virtual int bitch(char* what);
}

// These are data needed to process qip files.
class QipHistoryParser {
	regex_t inHdrPattern, outHdrPattern;
 public:
	QipHistoryParser();
	
};
	

	
	
	
	


	
