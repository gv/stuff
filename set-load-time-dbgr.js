var sh = new ActiveXObject('WScript.Shell');

function print(x) {
	WScript.Echo(x);
}

function changeReg(kp, val) {
	try { var old = sh.RegRead(kp); } catch(e) { old = e }
	sh.RegWrite(kp, val, "REG_SZ");
	print(kp + ": was " + old + ", set " + val);
}

changeReg("HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Image File Execution Options\\loader.exe\\Debugger", "\\\\xx\\dbg64\\windbg.exe");
