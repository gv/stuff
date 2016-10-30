#include <stdio.h>
#define UNICODE
#include <windows.h>
#include <tlhelp32.h>

static int usage(wchar_t **argv) {
	fwprintf(stderr, L"Usage: %s STRING\n", argv[0]);
	return 1;
}

struct Search {
	int count, total;
	wchar_t *str;
	int errors87Count, errors18count;
};

static BOOL CALLBACK cb(HWND w, LPARAM l) {
	struct Search *p = (struct Search*)l;
	TCHAR c[256];
	p->total++;

	if(!GetWindowText(w, c, 256)) {
		int r = GetLastError();
		if(r == 87)
			p->errors87Count++;
		//else if(r != ERROR_SUCCESS) 
		//	fprintf(stderr, "Warning %d hwnd=%d\n", r, w);
	} else {
		//fwprintf(stderr, L"%s\n", c);
		if(wcsstr(c, p->str)) {
			p->count++;
			return FALSE;
		}
	}

	EnumChildWindows(w, cb, l);
	
	return TRUE;
}

static int expect1(wchar_t *str) {
	struct Search s = {0, 0, str};
	for(;;) {
		s.total = 0;
		
		if(!EnumWindows(cb, (LPARAM)&s)) {
			fprintf(stderr, "Error %d\n", GetLastError());
			return 1;
		}
		if(s.count) {
			fwprintf(stderr, L"%d windows found (in %d)\n", s.count, s.total);
			return 0;
		}
		Sleep(500);
	}
}

static int expect(wchar_t *str) {
	struct Search s = {0, 0, str};
	THREADENTRY32 te;
	unsigned numThreads;

	fwprintf(stderr, L"Waiting for \"%s\"...\n", str);
	
	for(;;) {
		s.total = 0;
		numThreads = 0;
		s.errors87Count = 0;
		HANDLE ts = CreateToolhelp32Snapshot( TH32CS_SNAPTHREAD, 0 ); 
		if(ts == INVALID_HANDLE_VALUE) {
			fprintf(stderr, "CreateToolhelp32Snapshot: error %d\n", GetLastError());
			return 1;
		}

		te.dwSize = sizeof te;

		if(!Thread32First(ts, &te)) {
			fprintf(stderr, "Thread32First: error %d\n", GetLastError());
			return 1;
		}

		do {
			/*
			s.total = s.errors87Count = 0;
			fprintf(stderr, "Thread %d in %d: ",
				te.th32ThreadID, te.th32OwnerProcessID);
			*/
			EnumThreadWindows(te.th32ThreadID, cb, (LPARAM)&s);
			numThreads++;
			if(s.count)
				break;
			/*
			fwprintf(stderr,
				L"%d windows found (in %d, %d threads, %d invalid parameters)\n",
				s.count, s.total, numThreads, s.errors87Count);
			*/
		} while(Thread32Next(ts, &te));

		fwprintf(stderr,
			L"%d windows found (in %d, %d threads, %d invalid parameters)\n",
			s.count, s.total, numThreads, s.errors87Count);
		if(s.count) {
			return 0;
		}
		Sleep(500);
	}
}


int wmain(int argc, wchar_t **argv) {
	if(argc < 2)
		return usage(argv);
	SetConsoleTitle(L"***");
	return expect(argv[1]);
}
