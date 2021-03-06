#include <stdlib.h>
#include "tokenizer.h"

extern const struct Language javaLanguage;
extern const struct Language jsLanguage;
static const struct Language *languages[] = {
	&javaLanguage,
	&jsLanguage
};
 
struct Language *chooseLanguage(struct File *pf) {
	const struct Language **l;

	l = languages + countof(languages);
	while(--l >= languages) {
		if((*l)->couldDoPath(pf->path)) {
			break;
		}
	}
	if(l < languages) {
		pf->language = NULL;
	} else {
		pf->language = *l;
	}
	return pf->language;
}
		
	
