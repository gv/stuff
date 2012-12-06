#define UNICODE
#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <tchar.h>

wchar_t path[] = L"D:\\";
HANDLE dir;
DWORD resultSize;
char buf[4096];
HANDLE over;

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
	
	onChange(0, 0, &ov); 
	do {
		r = WaitForSingleObjectEx(over, INFINITE, TRUE);
		
		if(WAIT_IO_COMPLETION == r)
			continue;
		
		fprintf(stderr, "WAIT: %d\n", r);
		return 1;
	} while(r != WAIT_OBJECT_0);
}
 
	
