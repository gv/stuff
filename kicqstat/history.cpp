#include <dirent.h>
#include "history.h"

const char DIR_SEP = '\\';
const char usersPath[] = "C:\\Program Files\\QIP\\Users";
enum {
	MAX_USERID_LEN = 20;
};

IcqStat::IcqStat() {
}

int IcqStat::run() {
	// here we will collect QIP data
	char bf[200];
	DIR *usersDir = opendir(usersPath);
	if(!usersDir) {
		sprintf(bf, "Каталог, в котором должны храниться пользовательские данные, %s, не найден", usersPath);
		bitch(bf);
		return 0;
	}

	// find a 4365462562 subdirectory
	DIR *histDir = NULL;
	char histDirName[sizeof(usersPath) + MAX_USERID_LEN + 1];
	stat stt;
	strcpy(histDirName, usersPath);
	histDirName[strlen(usersPath)] = DIR_SEP;
	histDirName[strlen(usersPath) + 1] = '\0';
	dirent *subDir;
	while(subDir = readdir(usersDir)) {
		if('.' == subDir->d_name[0])
			continue;
		if(strlen(subDir->d_name) > MAX_USERID_LEN)
			continue;
		// form a name
		
		strcpy(histDirName + strlen(usersPath) + 1, subDir->d_name);
		stat(histDirName, &stt);
		


	}

}
		
		
		
		
		
		
	
	
