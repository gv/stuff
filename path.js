var sh = new ActiveXObject('WScript.Shell');
var fs = new ActiveXObject("Scripting.FileSystemObject");

function print(x) {
	WScript.Echo(x);
}

var needAdd = WScript.Arguments.Named.Exists("add");
if(!needAdd) {
	var needDelete = WScript.Arguments.Named.Exists("del");
}

var userKeyPath = "HKEY_CURRENT_USER\\Environment\\path";
var userPaths = sh.RegRead(userKeyPath).split(";");

var sysKeyPath = "HKLM\\SYSTEM\\CurrentControlSet\\Control\\" + 
	"Session Manager\\Environment\\PATH";
var sysPaths = sh.RegRead(sysKeyPath).split(";");

for(var i in sysPaths)
	print("SYS " + sysPaths[i]);
print("");

for(var i in userPaths)
	print("USR " + userPaths[i]);
print("");

if(needAdd) {
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
		//print("");
		var dirPath = paths[i];
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

		if(needDelete) {
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
	
sh.RegWrite(sysKeyPath, newSysPaths.join(";"), "REG_EXPAND_SZ");
sh.RegWrite(userKeyPath, newUserPaths.join(";"), "REG_EXPAND_SZ");
