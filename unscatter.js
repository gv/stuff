
DEBUG = true;

function print(x) {
	return WScript.Echo(x);
}

function trace(x) {
	DEBUG && print('* ' + x);
}

function keys(m) {
	var rv = [];
	for(var key in m)
		rv.push(key);
	return rv;
}


// get a console
var sh = new ActiveXObject('WScript.Shell');
if(WScript.FullName.match(new RegExp('wscript', 'i'))) {
  var cmdLine = 'cscript /nologo ' + WScript.ScriptFullName + ' /pause';
  sh.Run(cmdLine);
  WScript.Quit();
}


function run() {
	// install to shell
	if(!WScript.Arguments.Named.Exists('noinstall')) {
		var keyPath = "HKCR\\Folder\\shell\\pushtune\\",
			cmdLine = 'cscript /nologo ' + WScript.ScriptFullName + 
			' /pause /noinstall "%L"';
		try {
		var key = sh.RegRead(keyPath + "command\\");
		} catch(e) {
			var key = '<not found>';
		}
		trace('cmdLine is "' + key + '"');
		if(key != cmdLine) {
			sh.RegWrite(keyPath, 'Push to iPod', 'REG_SZ');
			sh.RegWrite(keyPath + "command\\", cmdLine, 'REG_SZ');
			print("Installed");
		}
	}		
	

	var app = WScript.CreateObject("iTunes.Application"), lib;
	var srcs = app.Sources;
	trace(' == SOURCES == ');
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
	trace("\n");

	if(!lib) {
		print('Target library not found');
		WScript.Quit();
	}


	var fs = new ActiveXObject('Scripting.FileSystemObject'), 
		args = WScript.Arguments.Unnamed;

	for(var i = 0; i < args.length; i++) {
		var dirPath = args.Item(i);
		trace('arg: ' + dirPath);
		var dir = fs.GetFolder(dirPath), paths = [], files = [];
		for(var en = new Enumerator(dir.Files); !en.atEnd(); en.moveNext()) {
			var file = en.item();
			if(file.Name.match(new RegExp("\\.mp3$"))) {
				var path = dirPath + "\\" + file.Name;
				trace('file:' + path);
				//paths.push(path);
				files.push({path: path, name: file.Name});
			}
		}


		/*
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
		*/

		var artistNamesMap = {}, albumTitlesMap = {};
		for(var j in files) {
			var file = files[j];
			trace('adding ' + file.name);
			var op = lib.AddFile(file.path);
			if(!op) {
				print("AddFiles returned null somehow");
				continue;
			}
			while(op.InProgress) {
				WScript.Sleep(1);
			}
			
			for(var i = 1; i <= op.Tracks.Count; i++) {
				var tk = op.Tracks.Item(i);
				trace(tk.TrackNumber + '\tn:"' + tk.Name + 
					'", l:"' + tk.Album + 
					'", a:"' + tk.Artist + '"');
				if(tk.Artist) 
					artistNamesMap[tk.Artist] = true;
				if(tk.Album)
					albumTitlesMap[tk.Album] = true;
				file.tk = tk;
			}
		}

		var artistNames = keys(artistNamesMap), albumTitles = keys(albumTitlesMap);
		var dirty = false;

		function askUser(options) {
			// If naming is already consistent, don't bother me
			if(1 == options.length)
				if(options[0] != "")
					return options[0];

			for(var i = 0; i < options.length; i++) {
				print('(' + (i + 1) + ")\t" + options[i]);
			}
		
			print("Choose a number or just enter different name");
			var line = WScript.StdIn.ReadLine();
			var n = parseInt(line, 10), rv;
			if(n)
				rv = options[n-1];
			return rv || line;
		}

		var artistName = askUser(artistNames);
		var albumTitle = askUser(albumTitles);

		print('Setting ' + files.length + ' tracks to "' +
			albumTitle + '" from "' + artistName + '"');
		//for(var i = 1; i <= op.Tracks.Count; i++) {
		for(var j = 0; j < files.length; j++) {
			//var tk = op.Tracks.Item(i);
			var tk = files[j].tk;
			trace('Setting ' + j + ' of ' + op.Tracks.Count);
			tk.Album = albumTitle;
			tk.Artist = artistName;
		}
	}

	trace('done');
}

run();

if(WScript.Arguments.Named.Exists('pause')) {
	print('Press ENTER to exit');
	WScript.StdIn.ReadLine();
}
