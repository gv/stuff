/* 
   Prints a list of preloaded DLLs in Windows
*/

var sh = new ActiveXObject('WScript.Shell');
var fs = new ActiveXObject("Scripting.FileSystemObject");

function print(x) {
	WScript.Echo(x);
}

var kp = 
	"HKLM\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Windows\\AppInit_DLLs";

print(kp);
sh.Run("cmd /k reg query \"" + kp + "\" /s|less && exit");
