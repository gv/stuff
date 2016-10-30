var sh = new ActiveXObject('WScript.Shell');
var fs = new ActiveXObject("Scripting.FileSystemObject");

function print(x) {
	WScript.Echo(x);
}

function changeReg(kp, val, type) {
	try { var old = sh.RegRead(kp); } catch(e) { old = e }
	sh.RegWrite(kp, val, type || "REG_SZ");
	print(kp + ": was " + old + ", set " + val);
}

function quote(s) { return '"' + s + '"' }

function haveUac() {
	var v = WScript.Version.split(".");
	return +v[0] >=5 && +v[1] > 6;
}

var mode = "";
if(WScript.Arguments.Named.Exists("add") || WScript.Arguments.Unnamed.length)
	mode += "add";
if(WScript.Arguments.Named.Exists("del"))
	mode += "del";

if(mode.length > 3) {
	WScript.Echo("usage : paths.js [/add | /del] dir");
	WScript.Quit(1);
}
/*
if(haveUac() && !WScript.Arguments.Named.Exists("didelevate")) {
	var face = new ActiveXObject("Shell.Application");
	var command = quote(WScript.ScriptFullName) + " /didelevate /" + mode;
	for(var i = 0; i < WScript.Arguments.Unnamed.length; i++) {
		command += ' "' + WScript.Arguments.Unnamed.Item(i) + '"';
	}
	face.ShellExecute("wscript", command, "", "runas", 1);
	WScript.Quit(1);
}
*/
// get a console
var sh = new ActiveXObject('WScript.Shell');
if(WScript.FullName.match(new RegExp('wscript', 'i'))) {
  var cmdLine = 'cscript /nologo /' + mode + ' ' + WScript.ScriptFullName;
	for(var i = 0; i < WScript.Arguments.Unnamed.length; i++) {
		cmdLine += ' "' + WScript.Arguments.Unnamed.Item(i) + '"';
	}
  sh.Run(cmdLine + "& pause");
  WScript.Quit();
}

var desktopKeyPath = 
	"HKLM\\Software\\Microsoft\\Windows\\currentversion\\app paths";

if(WScript.Arguments.Named.Exists("a")) {
	sh.Run("cmd /k reg query \"" + desktopKeyPath + "\" /s|more && exit");
	WScript.Quit();
}

if(WScript.Arguments.Named.Exists("aadd")) {
	var path = WScript.Arguments.Unnamed.Item(0);
	var name = path.split("\\").pop();
	var kp = desktopKeyPath + "\\" + name + "\\";
	try { var oldVal = sh.RegRead(kp); } catch(e) {}
	sh.RegWrite(kp, path, "REG_SZ");
	print(kp + ": was \"" + oldVal + "\", set to \"" + path + "\"");
	WScript.Quit();
}

var userKeyPath = "HKEY_CURRENT_USER\\Environment\\path";
try { var userPaths = sh.RegRead(userKeyPath).split(";"); } catch(e) {
	userPaths = [];
}

var sysKeyPath = "HKLM\\SYSTEM\\CurrentControlSet\\Control\\" + 
	"Session Manager\\Environment\\PATH";
var sysPaths = sh.RegRead(sysKeyPath).split(";");

var message = "";
for(var i in sysPaths)
	message += ("SYS " + sysPaths[i]) + "\n";
print(message);

var message = "";
for(var i in userPaths)
	message += ("USR " + userPaths[i] + "\n");
print(message);

if("add" == mode) {
	for(var j = 0; j < WScript.Arguments.Unnamed.length; j++) {
		var p = WScript.Arguments.Unnamed.Item(j);
		userPaths.push(p);
	}
}	

function printCommands(dirPath) {
	print("scanning " + dirPath);
	var dir = fs.GetFolder(dirPath);
	for(var en = new Enumerator(dir.Files); !en.atEnd(); en.moveNext()) {
		var file = en.item();
		var m = file.Name.match(new RegExp("^(.+)\\.(exe|bat|com)$"));
		if(m) {
			var path = dirPath + "\\" + file.Name;
			var cmd = m[1];
			print(cmd + " = " + path);
		}
	}
}

var used = {};

function filterPaths(paths) {
	var newPaths = [];
	for(var i in paths) {
		var dirPath = paths[i];
		if(dirPath == "")
			continue;
		dirPath = sh.ExpandEnvironmentStrings(dirPath);
		dirPath = dirPath.toLowerCase();
		dirPath = dirPath.replace(new RegExp("\\\\+$", "g"), "");
		if(used[dirPath]) {
			print("Used: " + dirPath);
			continue;
		}

		used[dirPath] = {};


		if(!fs.FolderExists(dirPath)) {
			print("Nonexistent: " + dirPath);
			continue;
		}

		if("del" == mode) {
			for(var j = 0; j < WScript.Arguments.Unnamed.length; j++) {
				var p = WScript.Arguments.Unnamed.Item(j);
				if(dirPath == p.toLowerCase()) 
					break;
			}

			if(j < WScript.Arguments.Unnamed.length) {
				print("Will delete: " + dirPath);
				continue;
			}
		}
	
		//printCommands(dirPath);
		newPaths.push(dirPath);
	}
	return newPaths;
}


var newSysPaths = filterPaths(sysPaths);
var newUserPaths = filterPaths(userPaths);


if(sysPaths + ""  != newSysPaths + "") {
	try { 
		changeReg(sysKeyPath, newSysPaths.join(";"), "REG_EXPAND_SZ");
	} catch(e) {
		print("writing " + sysKeyPath + ": " + e);
	}
}
if(userPaths + "" != newUserPaths + "")
	changeReg(userKeyPath, newUserPaths.join(";"), "REG_EXPAND_SZ");

function updateEnv() {
	/*
	sh.Run("control");
	sh.AppActivate("Control Panel");
	WScript.Sleep(200);
	sh.SendKeys("^w");

	sh.Run("control");
	sh.AppActivate("Control Panel");
	WScript.Sleep(1000);
	var groups = ("environment,{Down},{Down},{Enter}").split(","), g;
	while(g = groups.shift()) {
		sh.SendKeys(g);
		WScript.Sleep(300);
	}
	*/

	sh.Run("C:\\Windows\\system32\\rundll32.exe " + 
		"sysdm.cpl,EditEnvironmentVariables");
}

updateEnv();
