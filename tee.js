if (WScript.FullName.match(new RegExp('wscript', 'i')) ||
    WScript.Arguments.Unnamed.length != 1) {
    WScript.Echo(
        "tee.js: a script to duplicate console output to a file\n" +
            "usage: cscript tee.js PATH");
    WScript.Quit(1);
}

var fs = new ActiveXObject('Scripting.FileSystemObject');
var file = fs.CreateTextFile(
    WScript.Arguments.Unnamed.Item(0), true /*overwrite*/, true /*unicode*/);
while (!WScript.StdIn.AtEndOfStream) {
    var c = WScript.StdIn.Read(1);
    WScript.StdOut.Write(c);
    file.Write(c);
}
    
