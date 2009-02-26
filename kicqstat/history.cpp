/*
	This program counts words' usage frequencies in ICQ history
	Supports: QIP
*/

#include <dirent.h>
#include "history.h"

#ifdef WIN32
# define S_ISDIR(x) (_S_IFDIR & (x))
#endif

#define LIMITSTR(STR_) (STR_)[sizeof(STR_)-1] = '\0'

const char DIR_SEP = '\\';
const char usersPath[] = "C:\\Program Files\\QIP\\Users";
enum {
	MAX_USERID_LEN = 20;
};


IcqStat::IcqStat() 
{
}

int IcqStat::init() {
	// here we will collect QIP data
	char bf[200];
	DIR *usersDir = opendir(usersPath);
	if(!usersDir) {
		sprintf(bf, " аталог, в котором должны хранитьс€ пользовательские данные, %s, не найден", usersPath);
		// какой-то инновационный метод выходит
		bitch(bf);
		return 0;
	}

	// find a 4365462562 subdirectory
	char histDirName[sizeof(usersPath) + MAX_USERID_LEN + 1];
	int len = strlen(usersPath);
	strcpy(histDirName, usersPath);
	histDirName[len] = DIR_SEP;
	stat stt;
	dirent *sub;
	while(sub = readdir(usersDir)) {
		if('.' == sub->d_name[0])
			continue;
		if(strlen(sub->d_name) > MAX_USERID_LEN) {
			// XXX warn
			continue;
		}
		// form a name
		strcpy(histDirName + len, sub->d_name);

		if(!stat(histDirName, &stt)) {
			/// XXX warn
			continue;
		}

		if(S_ISDIR(stt.mode))
			break;
	}
	
	if(!sub) {
		sprintf(bf, "¬ пользовательском каталоге %s нет подкаталогов", usersPath);
		bitch(bf);
		return 0;
	}

	QipHistoryParser parser(&history);
	DIR *histDir = opendir(histDirName);
	if(!histDir) {
		sprintf(bf, "Can't open %s", histDirName);
		bitch(bf);
		return 0;
	}

	regex_t logFileNamePattern = regcomp("[0-9]+.txt$", 0);
	assert(logFileNamePattern);

	while(sub = readdir(histDir)) {
		if(regexec(&logFileNamePattern, sub->d_name, 0, NULL, 0)) {
			FILE *fp = fopen(sub->d_name, "rt");
			if(!fp) {
				// warn
				continue;
			}

			parser.parseFile(fp);
		}
	}
	regfree(&logFileNamePattern);
}
		

/*
	«агрузка одного квипового файла
*/
		
QipHistoryParser::QipHistoryParser(History *history):
	history_(history)
{
	inHdrPattern_ = regcomp("-+<", 0);
	assert(inHdrPattern_);
	outHdrPattern_ = regcomp("-+>", 0);
	assert(outHdrPattern_);
}

QipHistoryParser::~QipHistoryParser() {
	regfree(inHdrPattern_);
	regfree(outHdrPattern_);
}


int QipHistoryParser::parseFile(FILE *fp) {
	int cnt = 0, err;
	char line[200];
	MsgInfo m;
	m.body  = line;
	do {
		if(!fgets(line, sizeof(line), fp)) 
			break;
		
		err = regexec(inHdrPattern_, line, 0, NULL, 0);
		if(!err) { // incoming header line
			from = "someone";
			to = "me";
			// XXX handle ENOSPACE
		} else {
			err = regexec(outHdrPattern_, line, 0, NULL, 0);
			if(!err) {// outgoing header line
				to = "me";
				from = "someone";
			}
		}
		
		if(err)	continue;
		// read the header information
		fgets(line, sizeof(line), fp);
		
		
		// read body until the empty line
		// XXX make it a single message

		do {
			// empty line is 1 character i guess
			if(fgets(line, sizeof(line), fp) < 1) 
				break;
			history_->addMessage(&m);
		} while(1);
	} while(1);
}

/*
	»ндекс
*/

History::History() {
	wordPattern_ = regcomp("[а-€ј-я-]+", 0);
	assert(wordPattern_);
}

History::~History() {
	regfree(wordPattern_);
}

Person* History::getPerson(const char *name) {
	string temp;
	return &(*subjects_.insert(temp).first);
}

void History::addMessage(MsgInfo *m) {
	/*
	Msg msg;
	msg.body = m->body;
	string subj = m->from;
	msg.from = &(*subjects_.insert(subj).first);
	subj = m->to;
	msg.to = &(*subjects_.insert(subj).first);
	*/

	Person *from = getPerson(m->from);
	Person *to = getPerson(m->to);

	char *tail = m->body;

	// make it lowercase


	

	regmatch_t match;
	int err;
	string word;
	for(; !(err = regexec(&wordPattern_, tail, 1, &match, 0)); 
			tail += match.rm_eo) {
		word.assign(tail + match.rm_so, match.rm_so - match.rm_eo);
		
		Index::iterator wordIndex = index_.find(word);
		if(index_.end() == wordIndex) {
			wordIndex = index_.insert(word);
		} 
		
		WordIndex::iterator submitterIndex = wordIndex->find(from);
		if(wordIndex->end() == submitterIndex) {
			submitterIndex = wordIndex->insert(from);
		}
		submitterIndex->out++;
		
		WordIndex::iterator recipientIndex = wordIndex->find(to);
		if(wordIndex->end() == Index) {
			recipientIndex = wordIndex->insert(to);
		}
		submitterIndex->in++;
	}
	// че все чтоли уже
}
	
