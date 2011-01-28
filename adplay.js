var needToInstall = !WScript.Arguments.Named.Exists('noinstall');

DEBUG = 1;

function print(x) {
	return WScript.Echo(x);
}

var sh = new ActiveXObject('WScript.Shell');

function trace(x) {
	DEBUG && print('* ' + x);
}

// get a console
/*if(WScript.FullName.match(new RegExp('wscript', 'i'))) {
  var cmdLine = 'cscript /nologo ' + WScript.ScriptFullName + ' /pause';
	for(var i = 0; i < WScript.Arguments.Unnamed.length; i++) {
		cmdLine += ' ' + WScript.Arguments.Unnamed.Item(i);
	}
  sh.Run(cmdLine);
  WScript.Quit();
}
*/

// Install to shell
if(needToInstall) {
	var mediaSuffixes = ["mp3"];
	var cmdLine = 'cscript /nologo ' + WScript.ScriptFullName + 
		' /pause /noinstall "%L" ';
	var className = "folder", i = 0, suffix;
	while(suffix = mediaSuffixes[i++]) {
		var classNameRecPath = "HKCR\\." + suffix + "\\";
		className = sh.RegRead(classNameRecPath);
		var classPath = "HKCR\\" + className;
		var keyPath = classPath + "\\shell\\adpushplay\\";
		try {
			var key = sh.RegRead(keyPath + "command\\");
		} catch(e) {
			var key = '<not found>';
		}
		trace('cmdLine for ' + classPath + ' is "' + key + '"');
		if(key != cmdLine) {
			sh.RegWrite(keyPath, 'Play in android', 'REG_SZ');
			sh.RegWrite(keyPath + "command\\", cmdLine, 'REG_SZ');
			print("Installed shell context menu for " + classPath);
		}

	};
}		
	
var args = WScript.Arguments.Unnamed;
if(args.length) {
	sh.Run("adb push \"" + args.Item(0) + "\" /sdcard/pushed.mp3", 5, true);
	sh.Run("adb shell am start -t audio/mp3 -d file:///sdcard/pushed.mp3");
}
