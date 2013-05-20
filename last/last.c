#define UNICODE
#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <tchar.h>

#include "last.h"

wchar_t path[] = L"D:\\";
HANDLE dir;
DWORD resultSize;
char buf[4096];
HANDLE over;

struct Entry *top[STORE_SIZE];

static struct Entry **find(const wchar_t *path, DWORD pathLen) {
	struct Entry **e = top, *x;
	int len = pathLen / sizeof (wchar_t);
	
	while(e < top + STORE_SIZE && *e) {
		if(wcslen((*e)->path) == len) {
			if(!wcsncmp((*e)->path, path, len))
				break;
		}
		e++;
	}

	return e;
}

static void touch(const wchar_t *path, DWORD pathLen) {
	struct Entry **e, *x;
	int len = pathLen / sizeof (wchar_t);
	
	e = find(path, pathLen);

	if(e >= top + STORE_SIZE) {
		e--;
		free(*e);
		*e = NULL;
	}

	if(*e) {
		x = *e;
	} else {
		x = (struct Entry*) malloc(sizeof(struct Entry));
		if(!x)
			abort();
		memcpy(x->path, path, pathLen);
		x->path[len] = 0;
	}
	
	memmove(top + 1, top, (char*)e - (char*)top);
	top[0] = x;

	updateUi();
}
		

VOID CALLBACK onChange(DWORD errCode,	DWORD dwNumberOfBytesTransfered,				
	LPOVERLAPPED ov) {
	BOOL success;
	char *base = buf;

	if(errCode != 0) {
		fprintf(stderr, "errCode %d\n", errCode);
		SetEvent(over);
		return;
	}

	while(dwNumberOfBytesTransfered) {
		FILE_NOTIFY_INFORMATION *change = (FILE_NOTIFY_INFORMATION*)base;
		wchar_t *path = change->FileName;
		wprintf(L"c ");
		while(change->FileNameLength) {
			putwc(*path++, stdout);
			change->FileNameLength -= sizeof(wchar_t);
		}
		puts("");

		if(!change->NextEntryOffset)
			break;
		base += change->NextEntryOffset;
	}

	success = ReadDirectoryChangesW(
		dir,	buf,	sizeof buf,					// length of buffer
		TRUE,                        // recursive
		FILE_NOTIFY_CHANGE_LAST_WRITE |
		FILE_NOTIFY_CHANGE_CREATION |
		FILE_NOTIFY_CHANGE_FILE_NAME,        // filter conditions
		&resultSize,                           // bytes returned
		ov,                      // overlapped buffer
		onChange);           // completion routine
	
	if(!success) {
		fprintf(stderr, "no rdc\n");
		SetEvent(over);
	} 
}

int _tmain(int argc, TCHAR *argv[]) {
	DWORD r;
	OVERLAPPED ov = {0};
	
	over = CreateEvent(NULL, FALSE, FALSE, NULL);

	dir = CreateFile(path, 
		FILE_LIST_DIRECTORY,                // access (read/write) mode
		FILE_SHARE_READ						// share mode
		| FILE_SHARE_WRITE
		| FILE_SHARE_DELETE,
		NULL,                               // security descriptor
		OPEN_EXISTING,                      // how to create
		FILE_FLAG_BACKUP_SEMANTICS			// file attributes
		 | FILE_FLAG_OVERLAPPED,
		NULL);                              // file with attributes to copy

	if(INVALID_HANDLE_VALUE == dir) {
		fprintf(stderr, "Didn't open dir\n");
		return 1;
	}
	
	touch(L"<EMPTY>", 14);
	
	if(startUi()) {
		fprintf(stderr, "no ui\n");
		return 1;
	}
	
	onChange(0, 0, &ov); 
	do {
		r = MsgWaitForMultipleObjects(1, &over, FALSE, INFINITE, QS_ALLINPUT);

		if(WAIT_OBJECT_0 + 1 == r) {
			MSG msg;
			if (PeekMessage(&msg,  NULL, 0, 0, PM_REMOVE)){
				TranslateMessage(&msg);
				DispatchMessage(&msg);
			} 
			continue;
		}
			
		if(WAIT_IO_COMPLETION == r)
			continue;
		
		fprintf(stderr, "WAIT: %d\n", r);
		return 1;
	} while(r != WAIT_OBJECT_0);
}
 
	
