
DEBUG = true;

function print(x) {
	return WScript.Echo(x);
}

function trace(x) {
	DEBUG && print(x);
}

function keys(m) {
	var rv = [];
	for(var key in m)
		rv.push(key);
	return rv;
}

var app = WScript.CreateObject("iTunes.Application"), lib;
var srcs = app.Sources;
for(var i = 1 /* sic! */; i <= srcs.Count; i++) {
	var src = srcs.Item(i);
	trace(src.Name + ' (' + src.Kind + ')');

	if(DEBUG || 2 == src.Kind) { // IPod here
		var pls = src.Playlists;
		for(var j = 1; j <= pls.Count; j++) {
			var pl = pls.Item(j);
			trace('  ' + pl.Name + ' (' + pl.Kind + ')');

			if(2 == src.Kind && 1 == pl.Kind) { // "Library" kind of playlist
				lib = pl;
				if(!DEBUG)
					break;
			}
		}
		
		if(!DEBUG)
			break;
	}
}

if(!lib) {
	print('Target library not found');
	WScript.Exit();
}


var fs = new ActiveXObject('Scripting.FileSystemObject'), 
	args = WScript.Arguments.Unnamed;

for(var i = 0; i < args.length; i++) {
	var dirPath = args.Item(i);
	var dir = fs.GetFolder(dirPath), paths = [];
	for(var en = new Enumerator(dir.Files); !en.atEnd(); en.moveNext()) {
		var file = en.item();
		if(file.Name.match(new RegExp("\\.mp3$")))
			paths.push(dirPath + "\\" + file.Name);
	}


	trace('now going to add...');
	var op = lib.AddFiles(paths);

	print('Adding ' + paths.length + ' files in ' + dirPath + ' ...');

	if(!op) {
		print("AddFiles returned null somehow");
		WScript.Quit();
	}

	while(op.InProgress) {
		var tks = op.Tracks;
		tks && trace('loaded ' + tks.Count);
		WScript.Sleep(1);
	}

	var tks = op.Tracks;
	trace(tks.Count + ' tracks added');

	var artistNamesMap = {}, albumTitlesMap = {};
	for(var i = 1; i <= op.Tracks.Count; i++) {
		var tk = op.Tracks.Item(i);
		trace('"' + tk.Album + '" from "' + tk.Artist + '" (' + tk.TrackNumber + ')');
		if(tk.Artist) 
			artistNamesMap[tk.Artist] = true;
		if(tk.Album)
			albumTitlesMap[tk.Album] = true;
	}

	var artistNames = keys(artistNamesMap), albumTitles = keys(albumTitlesMap);
	
	function askUser(options) {
		// If naming is already consistent, don't bother me
		if(1 == options.length)
			if(options[0] != "")
				return options[0];

		for(var i = 0; i < options.length; i++) {
			print(i + 1 + "\t" + options[i]);
		}
		
		print("Choose one or just enter another value");
		var line = WScript.StdIn.ReadLine();
		var n = parseInt(line, 10), rv;
		if(n)
			rv = options[n-1];
		return rv || line;
	}

	var artistName = askUser(artistNames);
	var albumTitle = askUser(albumTitles);

	for(var i = 1; i <= op.Tracks.Count; i++) {
		var tk = op.Tracks.Item(i);
		trace('Setting ' + i + ' of ' + op.Tracks.Count);
		tk.Album = albumTitle;
		tk.Artist = artistName;
	}
}



trace('done');
