/* 
   This program produces emacs-style TAGS file for JavaScript and KScript programs.
   Handles directories recursively (no it doesn't).
	 Use like this: jsetags . 
	 (no don't)
*/

#include <sys/stat.h>
#include <ctype.h>
#include <errno.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>


#include <regex.h>

#ifdef WIN32
# include <tchar.h>
# include "xgetopt/xgetopt.h"

# define S_ISDIR(x) (_S_IFDIR & (x))

#else
# include <getopt.h>

#endif

// XXX trace to stderr
#define TRACE(LEVEL_, LIST_) if(prog.verbosity >= LEVEL_) {	\
		printf LIST_;																						\
		printf("\n");																						\
	}

#define LIMITSTR(STR_) (STR_)[sizeof(STR_)-1] = '\0'

#define COUNT(X_) (sizeof(X_)/(sizeof((X_)[0])))

#define MAX_PATH	260


char fileNamePatternStr[] = "\\.([jk]s|html?)$";

// XXX consider definitions that span multiple lines
// XXX consider var act = defer(function(a, b) {
char defunPatternStr[] = "([a-zA-Z0-9_]+)[ \t]*=[ \t]*function|function[ \t]+([a-zA-Z0-9_]+)";

struct JsEtags {
	unsigned verbosity;
	regex_t fileNamePattern;
} prog;

struct Analysis {
  regex_t defunPattern;
} analysis;

typedef struct Tag {
  char text[100]; // XXX what does "Tag text" actually mean?
  char name[100];
  int lineNumber, charNumber;
} Tag;

typedef struct TagListEntry {
  struct TagListEntry *next;
  struct Tag d;
} TagListEntry;
  
typedef struct TagFileSection {
  const char* fileName;
  TagListEntry *tags;
} TagFileSection;

struct TagFileProduction {
  char *fileName;
  FILE *fp;
  TagFileSection curSection;
} tagFileProduction = {
  "TAGS"
};

int measureDecimal(int number) {
	int max = 10, maxLen = 1;
	if(number >= 1000000000)
		return 10;

	for(;number >= max; max *= 10) 
		maxLen++;
	return maxLen;
}
	
	

// emacs TAGS section needs to have section data in bytes written in section header
// so before we write them we must calculate it

TagListEntry* addTag() {
  // allocate tag XXX do we realy need to order them?
  TagListEntry **last = &tagFileProduction.curSection.tags;
  while(*last) {
    last = &(*last)->next;
  }

  *last = malloc(sizeof(TagListEntry));
  if(!*last) {
    fprintf(stderr, "AAAA FUCKING HELL DO IT YOURSELF YOU MISERABLE POOR LAZY FAGGOT");
    abort();
  }

  // init list item
  (*last)->next = NULL;

	return *last;
}

// debug output
void showMatch(regmatch_t *rm) {
	char helperBf[200];
	int print = 0;

	memset(helperBf, ' ', sizeof(helperBf)-1);
	LIMITSTR(helperBf);

	if(rm->rm_so >= sizeof(helperBf)) {
		TRACE(2, ("Warning: match start offset too high (%d)", rm->rm_so));
	} else {
		helperBf[rm->rm_so] = '<';
		print = 1;
	}

	if(rm->rm_eo >= sizeof(helperBf)) {
		TRACE(2, ("Warning: match end offset too high (%d)", rm->rm_eo));
	} else {
		helperBf[rm->rm_eo] = '>';
		print = 1;
	}

	if(print) {
		TRACE(2, ("Match: %s", helperBf));
	}
}



void processRegularFile(const char *fileName) {
  FILE *fp = fopen(fileName, "rt");
  char line[200];
  unsigned linesReadCnt = 0;
  regmatch_t funMatch[3], *nameMatch;
	int err, cnt;
	TagListEntry *tag;
	size_t tagsTextSize, nameLen;

//#ifdef DEBUG
//#endif

	TRACE(1, ("Trying to process %s", fileName));

  if(!fp) {
    fprintf(stderr, "Couldn't fopen %s\n", fileName);
    return;
  }

  // start a new section in a TAGS file
  tagFileProduction.curSection.fileName = fileName;
  tagFileProduction.curSection.tags = NULL;

  
  // process file line by line
	// XXX check for binary file
  do {
    if(fgets(line, sizeof(line), fp)) {
      linesReadCnt++;
      err = regexec(&analysis.defunPattern, line, COUNT(funMatch), funMatch, 0);
      if(err) {
				if(REG_ESPACE == err) {
					// XXX crash more violently
					fprintf(stderr, "AAAAAAAAAAAAAAAAAAAAAAAAAAAAA");
					return;
				} else { // no match
					continue;
				}
      } else { // match
				// "function name" submatch is [1] or [2]
				for(nameMatch = funMatch + 1; 
					nameMatch < funMatch + COUNT(funMatch); 
					nameMatch++)
					if(nameMatch->rm_so >= 0)
						break;

				showMatch(funMatch);

				assert(funMatch + COUNT(funMatch) > nameMatch);
				showMatch(nameMatch);

				TRACE(2, ("Match: %s", line));
				tag = addTag();
				tag->d.lineNumber = linesReadCnt;
				tag->d.charNumber = nameMatch->rm_so;
				nameLen = min(sizeof(tag->d.text)-1, funMatch->rm_eo);
				strncpy(tag->d.text, line, nameLen);
        tag->d.text[nameLen] = '\0';
				nameLen = min(sizeof(tag->d.name)-1, (nameMatch->rm_eo - nameMatch->rm_so));
				strncpy(tag->d.name, line + nameMatch->rm_so, nameLen);
				tag->d.name[nameLen] = '\0';
			}
    } else { // file over
      break;
    }
  } while(1);

	TRACE(1, ("%d lines read", linesReadCnt));

  // write a section to the file
  tag = tagFileProduction.curSection.tags;
  // count bytes first
  tagsTextSize = 0;
  for(; tag; tag = tag->next) {
    tagsTextSize += strlen(tag->d.text) + 
      measureDecimal(tag->d.lineNumber) + 
      measureDecimal(tag->d.charNumber) +
      3; // first separator, ',', "\n"

    // obligatory field
    nameLen = strlen(tag->d.name);
    if(nameLen) {
      tagsTextSize += nameLen + 1;
    }
  }
  
  // write header
	// XXX check ENOSPACE
  fprintf(tagFileProduction.fp, "\x0c\n%s,%d\n", 
					tagFileProduction.curSection.fileName, 
					tagsTextSize);

  // write tags
  tag = tagFileProduction.curSection.tags;
  for(cnt = 0; tag; tag = tag->next) {
    fprintf(tagFileProduction.fp, "%s\x7F", tag->d.text);
    if(tag->d.name[0]) {
      fprintf(tagFileProduction.fp, "%s\x01", tag->d.name);
    }
    fprintf(tagFileProduction.fp, "%d,%d\n", tag->d.lineNumber, tag->d.charNumber);
		cnt++;
  }

	TRACE(1, ("Written %d tags", cnt));
	      
  // Clean up section.
  while(tagFileProduction.curSection.tags) {
    TagListEntry *doomed = tagFileProduction.curSection.tags;
    tagFileProduction.curSection.tags = tagFileProduction.curSection.tags->next;
    free(doomed);
  }
}

void processFile(const char *fileName, int checkName);

int processDir(const char *path) {
	DIR *dp;
	struct dirent *de;
	char filePath[MAX_PATH + 1];
	size_t pathLen;
	
	dp = opendir(path);
	if(!dp) {
		fprintf(stderr, "Can't opendir %s\n", path);
		return 0;
	}

	pathLen = strlen(path);
	if(pathLen >= MAX_PATH - 2) {
		fprintf(stderr, "Directory name %s is too long!\n", path);
		return 0;
	}

	strcpy(filePath, path);
	filePath[pathLen++] = '\\';

	while(de = readdir(dp)) {
		// we need to filter out '.' and '..' 
		// and '.svn' too by coincedence
		// why not do it here

		if('.' == de->d_name[0]) {
			continue;
		}

		// form a path
		LIMITSTR(filePath);
		strncpy(filePath + pathLen, de->d_name, sizeof(filePath) - pathLen);
		if(filePath[sizeof(filePath) - 1]) {
			LIMITSTR(filePath);
			fprintf(stderr, "Path beginning with %s is too long\n", filePath);
		} else {
			processFile(filePath, 1);		
		}
	}
	return 1;
}
		

void processFile(const char *fileName, int checkName) {
  int err;
  struct stat stt;

	TRACE(1, ("Trying file %s", fileName));

  // check for directory
  err = stat(fileName, &stt);
  if(ENOENT == err) {
    fprintf(stderr, "File %s not found!\n", fileName);
    return;
  }

  if(S_ISDIR(stt.st_mode)) {
    // call recursively
		processDir(fileName);
    return;
  }

	// If we're traversing a directory tree, we will process
	// only files whos names match a pattern
	if(checkName) {
		err = regexec(&prog.fileNamePattern, fileName, 0, NULL, 0);
		if(ENOSPC == err) {
			fprintf(stderr, "Ran out of space executing a regexp can you believe it\n");
			return;
		} else if(err) { // didn't match
			TRACE(1, ("Skipping %s", fileName));
			return;
		}
	}
  
  processRegularFile(fileName);
}  
  
   
    
int main(int argc, char **argv) {
  char bf[100];
  int c, i, err;
	// Init work mode
	prog.verbosity = 0;

  // Parse options using getopt
  while ((c = getopt (argc, argv, "v")) != -1) {
    switch(c) {
		case 'v':
			prog.verbosity++;
			break;
		case '?':
			if (isprint (optopt))
				fprintf (stderr, "Unknown option `-%c'.\n", optopt);
			else
				fprintf (stderr,
								 "Unknown option character `\\x%x'.\n",
								 optopt);
			return 1;

    default: 
			fprintf(stderr, "It's not happening\n");
			abort();
    }
  }
						    
	TRACE(1, ("Starting being %d verbose", prog.verbosity));
	TRACE(1, ("Tag pattern is \"%s\"", defunPatternStr));

	// Init prog .
	err = regcomp(&prog.fileNamePattern, fileNamePatternStr, REG_EXTENDED);
	if(err) {
    regerror(err, &prog.fileNamePattern, bf, sizeof(bf));
    fprintf(stderr, "Error in defun pattern: %s\n", bf);
    return 1;
	}		
  
  // Init analyzer.
  err = regcomp(&analysis.defunPattern, defunPatternStr, REG_EXTENDED);
  if(err) {
    regerror(err, &analysis.defunPattern, bf, sizeof(bf));
    fprintf(stderr, "Error in defun pattern: %s\n", bf);
    return 1;
  }

  // Init tags file
  tagFileProduction.fp = fopen(tagFileProduction.fileName, "wb");
  if(!tagFileProduction.fp) {
    fprintf(stderr, "Couldn't open %s for writing\n", tagFileProduction.fileName);
    return 1;
  }

  // process input files
  for(i = optind; i < argc; i++) {
    processFile(argv[i], 0);
  }

	//getch();
}
    
  
  
    
  
