#include <stdio.h>

#include <tchar.h>

#include <windows.h>
#include <mmsystem.h>

void debug(const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	fprintf(stderr, "\n");
}


const unsigned int MAX_VOLUME = 0xFFFF;
#define KBMIX_LOUDER 1
#define KBMIX_QUIETER 2

HMIXER mixer = NULL;
MIXERCONTROL control;

HWND w;

MIXERCONTROLDETAILS details;
MIXERCONTROLDETAILS_UNSIGNED volume;

int adjustVolume(int how) {
	MMRESULT r;
	details.cbStruct = sizeof(MIXERCONTROLDETAILS);
	details.dwControlID = control.dwControlID;
	details.cChannels = 1; // as if they were uniform
	details.cbDetails = sizeof volume;
	details.paDetails = &volume;
	r = mixerGetControlDetails(mixer, &details, MIXER_GETCONTROLDETAILSF_VALUE);
	if(MMSYSERR_NOERROR != r) {
		fprintf(stderr, "mixerGetControlDetails: %d\n", r);
		return 1;
	}
		
	debug("volume %d", volume.dwValue);
	
	if(KBMIX_LOUDER == how) {
		volume.dwValue += MAX_VOLUME/16;
		if(volume.dwValue > MAX_VOLUME)
			volume.dwValue = MAX_VOLUME;
	} else {
		if(volume.dwValue > MAX_VOLUME/16)
			volume.dwValue -= MAX_VOLUME/16;
		else
			volume.dwValue = 0;
	}
			
	r = mixerSetControlDetails(mixer, &details, MIXER_GETCONTROLDETAILSF_VALUE);
	if(MMSYSERR_NOERROR != r) {
		fprintf(stderr, "sd: %d\n", r);
		return 1;
	}
	
	return 0;
}

int showUi(int really) {
	ShowWindow(w, really);
	if(really) {
		SetWindowPos(w ,       // handle to window
			HWND_TOPMOST,  // placement-order handle
			0,  // horizontal position
			0,  // vertical position
			0,  // width
			0,  // height
			SWP_SHOWWINDOW | SWP_NOSIZE | SWP_NOMOVE // window-positioning options
		);		
		KillTimer(w, 1);
		SetTimer(w, 1, 1000, NULL);
		InvalidateRect(w, NULL, TRUE);
	}
	return 0;
}

char uiMsg[256];

static LRESULT CALLBACK 
handleEveryWindowMessage(HWND w, UINT m, WPARAM wParam,	LPARAM lParam) {
	PAINTSTRUCT paints;
	HDC c;

	switch(m) {
	case WM_KEYDOWN:
		debug("key %d", wParam);
		return 0;

	case WM_PAINT:
		c = BeginPaint(w, &paints);
		TextOut(c, 10, 3, uiMsg, strlen(uiMsg));
		EndPaint(w, &paints);
		return 0;
		
	case WM_HOTKEY:
		adjustVolume(wParam);
		sprintf(uiMsg, "Volume: %d%%", volume.dwValue / (MAX_VOLUME/100));
		showUi(1);
		return 0;

	case WM_TIMER:
		showUi(0);
		return 0;
	}
	return DefWindowProc(w, m, wParam, lParam);
}

HWND hiddenParentWindow;

int startUi() {
	WNDCLASS wc;
	RECT r;

	wc.style         = 0;
	wc.lpfnWndProc   = handleEveryWindowMessage;
	wc.cbClsExtra    = 0;
	wc.cbWndExtra    = 0;
	wc.hInstance     = 0;
	wc.hIcon         = 0;
	wc.hCursor       = LoadCursor(NULL, IDC_ARROW);
	wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
	wc.lpszMenuName  = NULL;
	wc.lpszClassName = _T("kbmixWindowClass");
	if (!RegisterClass(&wc))
		goto fail;

	SystemParametersInfo(SPI_GETWORKAREA, 0, &r, 0);

	hiddenParentWindow = CreateWindow(_T("kbmixWindowClass"), 
		"Keyboard-mix user interface hidden parent window", 
		WS_POPUP | WS_BORDER,
		r.right - 210, r.bottom - 35, 200, 20, 
		NULL, NULL, GetModuleHandle(0), NULL);
	if (!hiddenParentWindow) {
		debug("no window");
		goto fail;
	}

	w = CreateWindow(_T("kbmixWindowClass"), "Keyboard-mix user interface", 
		WS_POPUP | WS_BORDER,
		r.right - 210, r.bottom - 35, 200, 20, 
		hiddenParentWindow, NULL, GetModuleHandle(0), NULL);
	if (!w) {
		debug("no window");
		goto fail;
	}

	if(!RegisterHotKey(w, KBMIX_LOUDER, MOD_WIN, VK_OEM_PLUS)) {
		fprintf(stderr, "No hotkey\n");
		goto fail;
	}

	if(!RegisterHotKey(w, KBMIX_LOUDER, MOD_WIN, VK_ADD)) {
		fprintf(stderr, "No hotkey\n");
		goto fail;
	}

	if(!RegisterHotKey(w, KBMIX_QUIETER, MOD_WIN, VK_OEM_MINUS)) {
		fprintf(stderr, "No hotkey\n");
		goto fail;
	}

	if(!RegisterHotKey(w, KBMIX_QUIETER, MOD_WIN, VK_SUBTRACT)) {
		fprintf(stderr, "No hotkey\n");
		goto fail;
	}


	return 0;

 fail:
	// caller must do stopUi();
	return 1;
}	

int doUi() {
	MSG msg;
	while (GetMessage(&msg,  NULL, 0, 0)){
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}
	return 0;
}
	
	

int main() {
	MMRESULT r;
	MIXERCAPS caps = {0};
	MIXERLINE line;
	MIXERLINECONTROLS mlcs;
	
	int i;

	r = mixerOpen(&mixer, 0, NULL, 0, MIXER_OBJECTF_WAVEOUT);
	if(MMSYSERR_NOERROR != r) {
		fprintf(stderr, "open: %d\n", r);
		return 1;
	}

	r = mixerGetDevCaps(mixer, &caps, sizeof caps);
	if(MMSYSERR_NOERROR != r) {
		fprintf(stderr, "caps: %d\n", r);
		return 1;
	}

	debug("%d destinations", caps.cDestinations);

	line.cbStruct = sizeof(MIXERLINE);
	line.dwDestination = 0;
	line.dwSource = 0;
	r = mixerGetLineInfo(mixer, &line, 
		MIXER_OBJECTF_HMIXER | MIXER_GETLINEINFOF_DESTINATION);
	if(MMSYSERR_NOERROR != r) {
		fprintf(stderr, "li: %d\n", r);
		return 1;
	}

	debug("%d controls", line.cControls);
	
	mlcs.cbStruct = sizeof(MIXERLINECONTROLS);
	mlcs.dwLineID = line.dwLineID;
	mlcs.dwControlType = MIXERCONTROL_CONTROLTYPE_VOLUME;
	mlcs.cbmxctrl = sizeof(MIXERCONTROL);
	mlcs.pamxctrl = &control;
	r = mixerGetLineControls(mixer, &mlcs, MIXER_GETLINECONTROLSF_ONEBYTYPE);
	if(MMSYSERR_NOERROR != r) {
		fprintf(stderr, "lc: %d\n", r);
		return 1;
	}

	//debug("%d channels", control.cChannels);
	debug("control id: %d", control.dwControlID);

	//adjustVolume(KBMIX_QUIETER);

	if(startUi())
		return 1;

	return doUi();
}

	
