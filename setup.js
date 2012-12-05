// run this with admin rights if you are me

var sh = new ActiveXObject('WScript.Shell');

function print(x) {
	WScript.Echo(x);
}

function changeReg(kp, val) {
	try { var old = sh.RegRead(kp); } catch(e) { old = e }
	sh.RegWrite(kp, val, "REG_SZ");
	print(kp + ": was " + old + ", set " + val);
}


var vsSuffixes = "ncb|suo";
if(WScript.Arguments.Named.Exists("open")) {
	var path = WScript.Arguments.Unnamed.Item(0);
	path = path.replace(new RegExp("(" + vsSuffixes + ")$"), "sln");
	sh.Run(path);
	WScript.Quit();
}

if(WScript.FullName.match(new RegExp('wscript', 'i'))) {
	WScript.Quit();
}

print("SETTING FILE ASSOCIATIONS");

var sxs = vsSuffixes.split("|"), s;
var cmd = "wscript " + WScript.ScriptFullName + " /open %1";
while(s = sxs.shift()) {
	try { var t = sh.RegRead("HKCR\\." + s + "\\"); } catch(e) {continue};
	if(!t)
		continue;
	var cp = "HKCR\\" + t + "\\shell\\open\\command\\";
	print("Setting " + cp + " to " + cmd);
	sh.RegWrite(cp, cmd, "REG_SZ");
}


/*
	SYSTEM
*/
	

/*
WScript.Echo("Adding an admin user with non blank password...");

sh.Run("net user root q /add");
sh.Run("net localgroup Администраторы root /add");

// TODO GRANT ADMIN

var cp = "HKCR\\exefile\\shell\\runasroot\\";
var cmd = "runas /user:root \"%1\"";
sh.RegWrite(cp, cmd, "REG_SZ");
sh.RegWrite(cp + "command\\", cmd, "REG_SZ"); 
*/

WScript.Echo("Disabling admin shares...");

var p = "HKLM\\SYSTEM\\CurrentControlSet\\Services\\LanManServer\\Parameters\\";
sh.RegWrite(p + "AutoShareServer", 0, "REG_DWORD");
sh.RegWrite(p + "AutoShareWks", 0, "REG_DWORD");

sh.Run("net share c$ /delete");
sh.Run("net share d$ /delete");
sh.Run("net share admin$ /delete");

print("Cleaning up PATH...");

var pathPath = "HKLM\\SYSTEM\\CurrentControlSet\\Control\\" + 
	"Session Manager\\Environment\\PATH";
var p = sh.RegRead(pathPath);
p = p.split(";");
var newPath = "", used = {};
for(var i in p) {
	var dirPath = p[i];
	if(used[dirPath]) {
		WScript.Echo("double: " + dirPath);
		continue;
	}

	used[dirPath] = 1;

	if(newPath)
		newPath += ";";
	newPath += dirPath;
}

//print(newPath);
sh.RegWrite(pathPath, newPath+";;", "REG_EXPAND_SZ");
	
print("Enabling remote desktop access...");

sh.RegWrite("HKLM\\SYSTEM\\CurrentControlSet\\Control\\Terminal Server\\" + 
	"fDenyTSConnections", 0, "REG_DWORD");

print("Enabling quick edit in console...");

sh.Run("REG.EXE add HKCU\\Console /v QuickEdit /t REG_DWORD /d 1 /f");

/*print("Setting default kb to en-us...");

changeReg("HKEY_CURRENT_USER\\keyboard layout\\preload\\1", 
	"00000409" || "00002009"); 
changeReg("HKEY_CURRENT_USER\\keyboard layout\\preload\\2", "00000419" || 
"00010C1A");*/

/*
	APPLICATIONS
*/

changeReg("HKCU\\Environment\\NODE_DISABLE_COLORS", 1);
