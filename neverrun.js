// run this under your user account before giving the machine to another person

function print(x) {
	WScript.Echo(x);
}

var sh = new ActiveXObject("WScript.Shell");
var fs = new ActiveXObject("Scripting.FileSystemObject");

var pp = sh.ExpandEnvironmentStrings("%USERPROFILE%\\Application Data") + 
	"\\Mozilla\\Firefox\\Profiles\\";
var profilesDir = fs.GetFolder(pp);
for(var en = new Enumerator(profilesDir.SubFolders); 
		!en.atEnd(); en.moveNext()) {
	var pd = en.item();
	
	function del(p) {
		print("deleting " + p);
		//fs.DeleteFile(p);
		sh.Exec("RMDIR /S /Q \"" + p + "\"");
	}
	del(pp + pd.Name);
	//del(pp + pd.Name + "\\cookies.sqlite");
	//del(pp + pd.Name + "\\localstore.rdf");
}


	

print("IE: history");
sh.Run("RunDll32.exe InetCpl.cpl,ClearMyTracksByProcess 1");
print("IE: cache");
sh.Run("RunDll32.exe InetCpl.cpl,ClearMyTracksByProcess 8");
print("IE: cookies");
sh.Run("RunDll32.exe InetCpl.cpl,ClearMyTracksByProcess 2");
print("IE: passwords");
sh.Run("RunDll32.exe InetCpl.cpl,ClearMyTracksByProcess 32");
print("IE: forms");
sh.Run("RunDll32.exe InetCpl.cpl,ClearMyTracksByProcess 16");
