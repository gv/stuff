var needToPrintMencoderArgs = WScript.Arguments.Named.Exists('pma');
if(needToPrintMencoderArgs) {
	output = "";
}

var needToInstall = !WScript.Arguments.Named.Exists('noinstall');
		
DEBUG = !needToPrintMencoderArgs;


function print(x) {
	return WScript.Echo(x);
}

// get a console
var sh = new ActiveXObject('WScript.Shell');
if(WScript.FullName.match(new RegExp('wscript', 'i'))) {
  var cmdLine = 'cscript /nologo ' + WScript.ScriptFullName + ' /pause';
	for(var i = 0; i < WScript.Arguments.Unnamed.length; i++) {
		cmdLine += ' ' + WScript.Arguments.Unnamed.Item(i);
	}
  sh.Run(cmdLine);
  WScript.Quit();
}

var srcTypeNames = [
	'ITSourceKindUnknown', 
  'ITSourceKindLibrary', 
  'ITSourceKindIPod', 
  'ITSourceKindAudioCD', 
  'ITSourceKindMP3CD', 
  'ITSourceKindDevice', 
  'ITSourceKindRadioTuner', 
  'ITSourceKindSharedLibrary' 
];
var playlistTypeNames = [
	'ITPlaylistKindUnknown',  
	'ITPlaylistKindLibrary',
	'ITPlaylistKindUser',
	'ITPlaylistKindCD',  
	'ITPlaylistKindDevice',  
	'ITPlaylistKindRadioTuner'
];

var vidExts = ['avi', 'flv'];

function trace(x) {
	DEBUG && print('* ' + x);
}

function keys(m) {
	var rv = [];
	for(var key in m)
		rv.push(key);
	return rv;
}

function askUser(optionsMap, name) {
	var options = [];
	for(var k in optionsMap) {
		options.push({value: k, count: optionsMap[k]});
	}
			
	if(0 == options.length) {
		print('Enter value for "' + name + '":');
		return WScript.StdIn.ReadLine();
	}

	// If naming is already consistent, don't bother me
	if(1 == options.length)
		if(options[0].value != "") {
			print(name + ' is assumed to be "' + options[0].value + '"');
			return options[0].value;
		}

	options.sort(function(l, r) {
			if(l.count == r.count)
				return 0;
			if(r.count > l.count)
				return 1;
			return -1;
		});
				
	for(var i = 0; i < options.length; i++) {
		print('(' + (i + 1) + ")\t" + options[i].value);
	}
		
	print('Pick value for "' + name + 
		"\" \n(enter number or text, default \"" + options[0].value + '")');
	var line = WScript.StdIn.ReadLine();
	if('' == line) {
		return options[0].value;
	}
				
	var n = parseInt(line, 10), rv;
	if(n)
		rv = options[n-1];
	return rv && rv.value || line;
}

function printTwoCols(l, r) {
	var line = '   ' + l;
	while(line.length < 38)
		line += ' ';
	line = line.substring(0, 38) + '  ' + r.substring(0, 38);
	print(line);
}
			
function askUserForBoolean(question, defaultAnswer) {
	print(question  + ' (' + (defaultAnswer ? 'Y/n' : 'y/N') + ')?');
	while(1) {
		var line = WScript.StdIn.ReadLine().substring(0, 1).toLowerCase();
		if('' == line)
			return defaultAnswer;
		if('y' == line)
			return true;
		if('n' == line)
			return false;
	}
}

function set(file, propName, val) {
	if(file['tk' + propName] != val) {
		file['tk' + propName] = val;
		file.tk[propName] = val;
	}
}


//
//  MAIN PROGRAM
//

function run() {
	// Install to shell
	if(needToInstall) {
		var cmdLine = 'cscript /nologo ' + WScript.ScriptFullName + 
			' /pause /noinstall "%L" ';
		var className = "folder", i = 0;
		do {
			var classPath = "HKCR\\" + className;
			var keyPath = classPath + "\\shell\\pushtune\\";
			try {
				var key = sh.RegRead(keyPath + "command\\");
			} catch(e) {
				var key = '<not found>';
			}
			trace('cmdLine for ' + classPath + ' is "' + key + '"');
			if(key != cmdLine) {
				sh.RegWrite(keyPath, 'Push to iPod', 'REG_SZ');
				sh.RegWrite(keyPath + "command\\", cmdLine, 'REG_SZ');
				print("Installed shell context menu for " + classPath);
			}

			var suffix = vidExts[i++];
			if(!suffix)
				break;
			var classNameRecPath = "HKCR\\." + suffix + "\\";
			className = sh.RegRead(classNameRecPath);
		} while(className);
	}		
	
	// Find iPod
	var app = WScript.CreateObject("iTunes.Application"), lib, iPod, ourPlaylist;
	var srcs = app.Sources;
	trace(' == SOURCES == ');
	for(var i = 1 /* sic! */; i <= srcs.Count; i++) {
		var src = srcs.Item(i);
		trace(src.Name + ' (' + src.Kind + ') ' + srcTypeNames[src.Kind]);

		if(DEBUG || 2 == src.Kind) { // IPod here
			var pls = src.Playlists;
			for(var j = 1; j <= pls.Count; j++) {
				var pl = pls.Item(j);
				trace('  ' + pl.Name + ' (' + pl.Kind + ') ' + 
					playlistTypeNames[pl.Kind] );

				if(2 == src.Kind) {
					iPod = src;
					if(1 == pl.Kind) { // "Library" kind of playlist
						lib = pl;
					}
					if('Pushed' == pl.Name) {
						ourPlaylist = pl;
					}
				}
			}
		}
	}
	trace("\n");

	if(!iPod) {
		print('iPod not found');
	} else {
		if(!ourPlaylist) {
			trace('Creating "Pushed" playlist...');
			ourPlaylist = app.CreatePlaylistInSource('Pushed', iPod);
			if(!ourPlaylist) {
				print("Can't create 'Pushed' playlist");
			}
		}
	}
	
	if(ourPlaylist)
		lib = ourPlaylist;

	var fs = new ActiveXObject('Scripting.FileSystemObject'), 
		args = WScript.Arguments.Unnamed;
	
	//
	//   *** ARG LOOP ***
	//
	for(var i = 0; i < args.length; i++) { 
		var path = args.Item(i);
		trace('arg: ' + path);
		
		var files = [];

		var vidExtPattern = new RegExp("\\.(" + vidExts.join('|') + ")$");
		// Assume no directory is named Album.avi . 
		if(path.match(vidExtPattern)) {
			// It's a video, convert to mp4
			var outputPath = path.replace(vidExtPattern, '.mp4');
			if(outputPath.indexOf('http://') >= 0) { // nonlocal
				outputPath = outputPath.replace(new RegExp('[:/&]+', 'g'), '_');
			}

			var converted = false;
			var outputTempPath = outputPath+ '-incomplete.mp4'
			var mencoderPath = "d:\\programs\\MPlayer-p4-svn-29355\\mencoder.exe";
			var mencoderOpts = ' -vf scale=480:-10,harddup -lavfopts format=mp4 ' + 
				'-faacopts mpeg=4:object=2:raw:br=128 -oac faac -ovc x264 -sws 9 ' + 
				'-x264encopts nocabac:level_idc=30:bframes=0:global_header:threads=auto:' + 
				'subq=5:frameref=6:partitions=all:trellis=1:chroma_me:me=umh:' +
				'bitrate=500 -of lavf ';
			var fileOpts = ' -o "' + outputTempPath;
			var subPath = path.replace(vidExtPattern, '.srt');
			if(fs.FileExists(subPath)) {
				// mencoder takes a comma separated path list for a -sub parameter.
				fileOpts += ' -sub "' + subPath.replace(new RegExp(',', 'g'), "\\,") + '"';
			}
			fileOpts +=	' "' + path + '"';
			
			if(needToPrintMencoderArgs) {
				output += fileOpts;
				continue;
			} else {
				var cmdLine = mencoderPath + mencoderOpts + fileOpts;
				trace(cmdLine);
				var exitCode = sh.Run(cmdLine, 5, true);
				trace(exitCode);
				if(!exitCode)
					converted = true;

				if(!converted) {
					var vlcPath = "d:\\Programs\\vlc-1.1.0-git-20090710-2203\\vlc.exe";
					var cmdLine = vlcPath + 
						" -vvv --sout=#transcode{vcodec=mp4v,vb=1024,scale=1," +
						"height=320,width=480,acodec=mp4a,ab=128,channels=2,soverlay}" + 
						":duplicate{dst=std{access=file,mux=mp4,dst=" + 
						outputTempPath + "}} --run-time 30 " + path + " vlc://quit";
					sh.Run(cmdLine, 5, true);
					converted = true;
				}

				if(converted) {
					fs.MoveFile(outputTempPath, outputPath);
					files.push({path: outputPath, name: outputPath});
				}
			}
		} else { // it's a directory with music
			var dirPath = path;
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
		}


		// Now we know what to push

		if(!lib) {
			print("Can't push: Target library not found");
			continue;
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

		// AddFiles() seems faster, but I use AddFile() here, cause I need to be 
		// sure which track corresponds to which file

		var artistNamesMap = {}, albumTitlesMap = {};
		for(var j = 0; j < files.length; j++) {
			var file = files[j];
			trace('adding ' + file.name);
			var op = lib.AddFile(file.path);
			if(!op) {
				// biggest problem by now
				print("AddFile returned null somehow");
				files.splice(j--, 1);
				continue;
			}
			while(op.InProgress) {
				WScript.Sleep(1);
			}

			if(!op.Tracks.Count) {
				trace('Adding ' + file.path + ' yielded no tracks!');
				files.splice(j--, 1);
				continue;
			}
			
			if(op.Tracks.Count > 1) {
				// Dunno, just in case
				print('Adding ' + file.path + ' yielded more than 1 track!');
			}
			
			//for(var k = 1; k <= op.Tracks.Count; k++) {
			var tk = op.Tracks.Item(1);
			// cache these cause reads are slow
			file.tkTrackNumber = tk.TrackNumber;
			file.tkName = tk.Name;
			file.tkAlbum = tk.Album;
			file.tkArtist = tk.Artist;

			trace(file.tkTrackNumber + ' n:"' + file.tkName + 
				  '", l:"' + file.tkAlbum + 
				  '", a:"' + file.tkArtist + '"');

			function count(word, map) {
				if(word) {
					if(map[word])
						map[word]++;
					else map[word] = 1;
				}
			}
			
			count(file.tkArtist, artistNamesMap);
			count(file.tkAlbum, albumTitlesMap);
			file.tk = tk;
		}


		if(!files.length) {
			print('No files from ' + dirPath + ' added!');
			continue;
		}

		var artistName = askUser(artistNamesMap, 'Artist name');

		// Fix title
		// If we suspect album title contains band name, we just make a title
		// with it removed and include it as another option
		for(var albumTitle in albumTitlesMap) {
			if(albumTitle.substring(0, artistName.length).toLowerCase() == 
			artistName.toLowerCase()) {
				var supposedTitle = albumTitle.substring(artistName.length).
					replace(new RegExp("^[-., \t]+"), '');
				
				if(albumTitlesMap[supposedTitle])
					albumTitlesMap[supposedTitle] += albumTitlesMap[albumTitle] + 1;
				else 
					albumTitlesMap[supposedTitle] = albumTitlesMap[albumTitle] + 1;
			}
		}

		var albumTitle = askUser(albumTitlesMap, 'Album title');
		
		// Fix names
		for(var len = 0; ; len++) {
			break;


		}
		
		// Fix track numbers
		// files are ordered by names
		var prevNumber = -1;
		for(var j = 0; j < files.length; j++) {
			if(files[j].tk.TrackNumber <= prevNumber)
				break;
			prevNumber = files[j].tk.TrackNumber;
		}
		
		var needToFixTrackNumbers = false;
		if(j < files.length) {
			// Sort by track numbers
			var numbered = files.concat();
			numbered.sort(function(l, r) {
					if(l.tk.TrackNumber == r.tk.TrackNumber)
						return 0;
					if(l.tk.TrackNumber < r.tk.TrackNumber)
						return -1;
					return 1;
				});
			
			printTwoCols("By track number", "By filename");
			printTwoCols("--", "--");
			for(var j in files) {
				printTwoCols(numbered[j].name, files[j].name);
			}

			printTwoCols("--", "--");
			needToFixTrackNumbers = askUserForBoolean(
				'Track numbers seem to be messed up, want to order by filenames instead',
				true);
			
		}

		print('Setting ' + files.length + ' tracks to "' +
			albumTitle + '" from "' + artistName + '"');
		//for(var i = 1; i <= op.Tracks.Count; i++) {
		for(var j = 0; j < files.length; j++) {
			//var tk = op.Tracks.Item(i);
			var file = files[j];
			print('Setting ' + j + ' of ' + files.length);

			set(file, 'Album', albumTitle);
			if(artistName != '-') {
				set(file, 'Artist', artistName);
			} else {
				file.tk.Compilation = true;
			}
			if(needToFixTrackNumbers)
				set(file, 'TrackNumber', j);
		}
	}

	if(needToPrintMencoderArgs) {
		print('mencoder ' + mencoderOpts + output);
	}

	trace('done');
}

run();

if(WScript.Arguments.Named.Exists('pause')) {
	print('Press ENTER to exit');
	WScript.StdIn.ReadLine();
}
