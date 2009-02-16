/* 
   This program produces emacs-style TAGS file for JavaScript and KScript programs.
   Handles directories recursively (no it doesn't).
*/

#include <sys/stat.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

// Could put no '.' in the beginning as well
char defunPatternString[] = "\\.([a-zA-Z0-9_]+)\\s*=\\s*function|function\\s+.([a-zA-Z0-9_]+)";

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



void parseFile(const char *fileName) {
  // check for directory
  int err;
  struct stat stt;
  err = stat(fileName, &stt);
  if(ENOENT == err) {
    fprintf(stderr, "File %s not found!\n", fileName);
    return;
  }

  if(S_ISDIR(stt.st_mode)) {
    // XXX call recursively
    fprintf(stderr, "%s is a directory\n", fileName);
    return;
  }
  
  
  FILE *fp = fopen(fileName, "rt");
  char line[200];
  unsigned linesReadCnt = 0;

  if(!fp) {
    fprintf(stderr, "Couldn't fopen %s\n", fileName);
    return;
  }

  // start a new section in a TAGS file
  tagFileProduction.curSection.fileName = fileName;
  tagFileProduction.curSection.tags = NULL;

  
  // process file line by line
  do {
    if(fgets(line, sizeof(line), fp)) {
      linesReadCnt++;
      regmatch_t funNameMatch;
      int execErr = regexec(&analysis.defunPattern, line, 1, &funNameMatch, 0);
      if(execErr) {
	if(REG_ESPACE == execErr) {
	  // XXX crash more violently
	  fprintf(stderr, "AAAAAAAAAAAAAAAAAAAAAAAAAAAAA");
	  return;
	} else { // no match
	  continue;
	}
      } else { // match
	addTag(line + funNameMatch.rm_so, 
	       funNameMatch.rm_eo - funNameMatch.rm_so,
	       linesReadCnt,
	       funNameMatch.rm_so);
      }
    } else { // file over
      break;
    }
  } while(1);

  // write a section to the file
  TagListEntry *tag = tagFileProduction.curSection.tags;
  // count bytes first
  unsigned tagsTextSize = 0;
  for(; tag; tag = tag->next) {
    tagsTextSize += strlen(tag->text) + 
      measureDecimal(tag->lineNumber) + 
      measureDecimal(tag->charNumber) +
      3; // first separator, ',', "\n"

    // obligatory field
    int nameLen = strlen(tag->name);
    if(nameLen) {
      tagsTextSize += nameLen + 1;
    }
  }
  
  // write header
  fprintf(tagFileProduction.fp, "\x0c\n%s,%d\n", 
	  tagFileProduction.curSection.fileName, 
	  tagsTextSize);

  // write tags
  tag = tagFileProduction.curSection.tags;
  for(; tag; tag = tag->next) {
    fprintf(tagFileProduction.fp, "%s\xF7", tag->text);
    if(tag->name[0]) {
      fprintf(tagFileProduction.fp, "%s\x01", tag->name);
    }
    fprintf(tagFileProduction.fp, "%d,%d\n", tag->lineNumber, tag->charNumber);
  }
	      
  // Clean up section.
  while(tagFileProduction.curSection.tags) {
    TagListEntry *doomed = tagFileProduction.curSection.tags;
    tagFileProduction.curSection.tags = tagFileProduction.curSection.tags->next;
    free(doomed);
  }
}  
  
   
    
int main(int argc, char **argv) {
  char bf[100];
  int c, i;
  // Parse options using getopt
  while ((c = getopt (argc, argv, "abc:")) != -1) {
    switch(c) {
      // XXX put opions here
    default: break;
    }
  }
						    
   
  
  // Init analyzer
  int regErrCode = regcomp(&analysis.defunPattern, defunPatternString, 0);
  if(regErrCode) {
    regerror(regErrCode, &analysis.defunPattern, bf, sizeof(bf));
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
    parseFile(argv[i]);
  }
}
    
  
  
    
  
