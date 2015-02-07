function quote(s) { return '"' + s + '"' }

function print(x) {	WScript.Echo(x) }

function haveUac() {
	var v = WScript.Version.split(".");
	return +v[0] >=5 && +v[1] > 6;
}

var sh = new ActiveXObject('WScript.Shell');

if(WScript.Arguments.Unnamed.length)
	var mode = WScript.Arguments.Unnamed.Item(0);
else
	mode = "";

if(haveUac() && !WScript.Arguments.Named.Exists("didelevate")) {
	var face = new ActiveXObject("Shell.Application");
	var command = ["/k", 
		"cscript", quote(WScript.ScriptFullName),  "/didelevate",  mode, "&", 
		"w32tm /resync"
	];
	face.ShellExecute("cmd", command.join(" "), "", "runas", 1);
	WScript.Quit(0);
}

if(!WScript.Arguments.Unnamed.Length) {
	print("Usage: dmn.js [/in] <password>");
	WScript.Quit(1);
}

var server = WScript.Arguments.Unnamed.Length > 1 ?
	WScript.Arguments.Unnamed.Item(1) : ".";

var wmi = GetObject("winmgmts:\\\\" + server + "\\root\\cimv2");

var strWQL = "select * from Win32_ComputerSystem";
var objInstances = wmi.ExecQuery(strWQL);

function describe(r) {
	if(2692 == r) {
		var d = "were not in any domain";
	} else	if(2691 == r) {
		var d = "already in domain";
	} else if(0 == r) {
		var d = "success";
	} else if(5 == r) {
		var d = "access denied";
	}
	return r + " " + (d ? d : "");
}

for(var e = new Enumerator(objInstances); !e.atEnd();e.moveNext()) {
	var p = e.item();
	print(
		"Name=" + p.Name + ", Caption=" + p.Caption + "\n" +
		"Domain=" + p.Domain + "\n" + 
		"UserName=" + p.UserName);

	var pw = WScript.Arguments.Unnamed.Item(0);
	if(!WScript.Arguments.Named.Exists("IN")) {
		var r = p.UnjoinDomainOrWorkgroup(pw, "programmers\\vg", 0);
		print("unjoin result=" + describe(r));
	}
	var r = p.JoinDomainOrWorkgroup("programmers", pw,
		"programmers\\vg", null, 3);
	print("result=" + describe(r));
}
