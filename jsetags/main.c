/* 
   This program produces emacs-style TAGS file for JavaScript and KScript programs.
   Handles directories recursively (no it doesn't).
	 Use like this: jsetags .
*/

#include <sys/stat.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include <regex.h>

#include <tchar.h>
#include "xgetopt/xgetopt.h"

#define TRACE(LEVEL_, LIST_) if(prog.verbosity >= LEVEL_) {	\
		printf LIST_;																						\
		printf("\n");																						\
	}

char fileNamePattern[] = ".[jk]s$";

// Could put no '.' in the beginning as well
// XXX consider definitions that span multiple lines
char defunPatternString[] = "([a-zA-Z0-9_]+)[ \t]*=[ \t]*function|function[ \t]+([a-zA-Z0-9_]+)";

struct JsEtags {
	unsigned verbosity;
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
  struct Tag;
} TagListEntry;
  
typedef struct TagFileSection {
  char* fileName;
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

void addTag(char *tagName, int tagNameLim, int lineNumber, int charNumber) {
  // allocate tag XXX do we realy need to order them?
  TagListEntry **last = &tagFileProduction.curSection.tags;
  while(*last) {
    last = &(*last)->next;
  }

  *last = malloc(sizeof(TagListEntry));
  if(!*last) {
    fprintf(stderr, "AAAA FUCKING HELL DO IT YOURSELF YOU MISERABLE POOR LAZY FAGGOT");
    return;
  }

  // init list item
  (*last)->next = NULL;

  // store data in it
  strncpy((*last)->text, tagName, tagNameLim);
  (*last)->text[tagNameLim] = 0;
  (*last)->lineNumber = lineNumber;
  (*last)->charNumber = charNumber;
  // set name to an empty string maybe will use it later
  (*last)->name[0] = 0;
}

void processRegularFile(const char *fileName) {
  FILE *fp = fopen(fileName, "rt");
  char line[200];
  unsigned linesReadCnt = 0;
  regmatch_t funNameMatch;
	int err, nameLen, cnt;
	TagListEntry *tag;
	unsigned tagsTextSize;

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
      err = regexec(&analysis.defunPattern, line, 1, &funNameMatch, 0);
      if(err) {
				if(REG_ESPACE == err) {
					// XXX crash more violently
					fprintf(stderr, "AAAAAAAAAAAAAAAAAAAAAAAAAAAAA");
					return;
				} else { // no match
					continue;
				}
      } else { // match
				TRACE(2, ("Match: %s", line));
				addTag(line/* + funNameMatch.rm_so*/, 
							 funNameMatch.rm_eo/* - funNameMatch.rm_so*/,
							 linesReadCnt,
							 funNameMatch.rm_so);
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
    tagsTextSize += strlen(tag->text) + 
      measureDecimal(tag->lineNumber) + 
      measureDecimal(tag->charNumber) +
      3; // first separator, ',', "\n"

    // obligatory field
    nameLen = strlen(tag->name);
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
    fprintf(tagFileProduction.fp, "%s\x7F", tag->text);
    if(tag->name[0]) {
      fprintf(tagFileProduction.fp, "%s\x01", tag->name);
    }
    fprintf(tagFileProduction.fp, "%d,%d\n", tag->lineNumber, tag->charNumber);
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

void processFile(const char *fileName) {
  int err;
  struct stat stt;

	TRACE(1, ("Trying to process %s", fileName));

  // check for directory
  err = stat(fileName, &stt);
  if(ENOENT == err) {
    fprintf(stderr, "File %s not found!\n", fileName);
    return;
  }

  //if(S_ISDIR(stt.st_mode)) {
	if(_S_IFDIR & stt.st_mode) {
    // XXX call recursively
    fprintf(stderr, "%s is a directory\n", fileName);
    return;
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
	TRACE(1, ("Tag pattern is \"%s\"", defunPatternString));
  
  // Init analyzer
  err = regcomp(&analysis.defunPattern, defunPatternString, REG_EXTENDED);
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
    processFile(argv[i]);
  }

	//getch();
}
    
  
  
    
  
