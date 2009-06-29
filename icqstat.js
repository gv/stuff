var DEBUG = false;
var DEBUG = true;

function trace(s) {
	if(DEBUG) print(s);
}

function print(s) {
	return WScript.Echo(s);
}

function QipHistory(stream, userName) {
	this.userName = userName;
	this.stream = stream;
}

QipHistory.prototype.readMsg = function() {
	do {
		if(this.stream.AtEndOfStream) return;

		var line = this.stream.ReadLine();
		var incomingHeader = line.match(new RegExp('-+<'));
		var outgoingHeader = line.match(new RegExp('-+>'));
	} while(!(incomingHeader || outgoingHeader));

	// XXX read usernames
	var msg = new Msg();
	if(incomingHeader) {
		msg.from = 'someone';
		msg.to = this.userName;
	} else {
		msg.from  = this.userName;
		msg.to = 'someone';
	}

	if(this.stream.AtEndOfStream) return;
	var header = this.stream.readLine();
	msg.body = '';

	do {
		if(this.stream.AtEndOfStream) return msg;
		var line = this.stream.readLine();
		msg.body += line;
	} while(line.length);
	
	return msg;
};

function Msg(body, from, to) {
	this.body = body;
}


var fs = new ActiveXObject('Scripting.FileSystemObject'),
	usersPath = "C:\\Program Files\\QIP\\Users";

function IcqStat() {
	// defaults
	this.stopWords = ('в на от не и что я а это что по то у ок как ты да ' +
		'там все ' +
		'нет из так есть мне с если к но для еще или за ну о').
		split(' ');
	this.lim = 40;
	this.page = 0;
	this.screenWidth = 80;
	this.screenHeight = 25;
	this.interactive = true;
	this.pauseAfterAll = false;

	this.initShell();
	this.addCommand(['incoming'],
									'Упорядочивать по частоте во входящих',
									function(prog, args) {
										prog.setWordStatDisplay().filter = 'incoming';
									});
	this.addCommand(['outgoing'],
									'Упорядочивать по частоте во исходящих',
									function(prog) {
										prog.setWordStatDisplay().filter = 'outgoing';
									});
	this.addCommand(['quit'],
									'Выйти из программы (Ctrl-C)',
									function(prog) {
										prog.exit = true;
									});
}

IcqStat.prototype.setWordStatDisplay = function() {
	if(!(this.display instanceof WordStatDisplay)) {
		trace('setting wordstat display');
		this.display = new WordStatDisplay(this);
	}
	return this.display;
};

IcqStat.prototype.isStopWord = function(word) {
	for(var i in this.stopWords) {
		if(word == this.stopWords[i]) return true;
	}
	return false;
};


IcqStat.prototype.loadArgs = function(args) {
	//print(args.length);
	this.pauseAfterAll = args.Named.Exists('pause');
		
};

IcqStat.prototype.loadConf = function() {
	
};

IcqStat.prototype.run = function() {
	// екранчик
	if(WScript.FullName.match(new RegExp('wscript', 'i'))) {
		// мы запускаемся не в консоли
		var sh = new ActiveXObject('WScript.Shell');
		// скопируем коммандную строку, добавим установку ждать
		// нажатия клавиши после работы, чтобы окно не сразу 
		// закрывалось
		var cmdLine = 'cscript /nologo ' + WScript.ScriptFullName + ' /pause';
		trace(cmdLine);
		sh.Run(cmdLine);
		return;
	} else {
		//print('xxx');
	}
	
	// начнем действовать
	try {
		var usersDir = fs.GetFolder(usersPath);
	} catch(e) {
		print('Каталог ' + usersPath + ' не найден!');
		throw e;
	}

	// Найдем какого-нибудь одного пользователя
	var en = new Enumerator(usersDir.SubFolders);
	if(en.atEnd()) {
		print('В каталоге ' + usersPath + ' не найдено ни одного подкаталога');
		throw 123;
	}

	var userId = en.item().Name;
	var histDir = fs.GetFolder(usersPath + "\\" + userId + "\\" + "History");

	var  wordsMap = {}, wordsArr = [];
	// Возьмем все файлы
	for(var en = new Enumerator(histDir.Files); !en.atEnd(); en.moveNext()) {
		var file = en.item();
		if(!file.Name.match(new RegExp('[0-9]+.txt')))
			continue;

		trace('Читаем ' + file.Name);
		var strm = file.OpenAsTextStream();
		var hist = new QipHistory(strm, 'me'), msgCnt = 0;

		for(;;) {
			var msg = hist.readMsg();
			if(!msg) break;

			var body = msg.body.toLowerCase();
			var bodyWords = body.match(new RegExp('[а-я]+', 'g'));
			if(bodyWords) {
				for(var i = 0; i < bodyWords.length; i++) {
					var word = bodyWords[i];

					if(!wordsMap[word]) {
						wordsArr.push(wordsMap[word] = {
								count: 0, 
								word: word, 
								incoming: [],
								outgoing: []});
					}
					wordsMap[word]
						[('me' == msg.from) ? 'outgoing' : 'incoming'].
						push(msg);
				}
			}
			msgCnt++;
		}
		trace(msgCnt + ' сообщений');
	}
	this.wordsArr = wordsArr;


	if(this.interactive) {
		this.setWordStatDisplay();
		this.interact();
	} else if(this.pauseAfterAll) {
		print('Нажмите Enter когда будете готовы выйти из программы');
		WScript.StdIn.Read(1);
	}
};

// ---------- Shell -----------------

function extend(base, derived) {
	var f = function() {};
	f.prototype = base.prototype;
	derived.prototype = new f();
	return derived;
}

function Display() {}

Display.prototype.render = function(prog) {
	throw 123;
};

Display.prototype.getNextDisplay = function() {
	return this;
};

Display.prototype.getTitle = function() {
	return '';
};
	
IcqStat.prototype.render = function() {
	this.display.render(this);
};

IcqStat.prototype.interact = function() {
	this.exit = false;
	do {
		print('---------------------------------------------------------------------');
		for(var i = 20; i; i--) print('');
		this.render();
		// wait for a command
		print('');
		print('');
		print('enter command (h for help)>');
		var cmd = WScript.StdIn.ReadLine().replace(new RegExp('^\s+|\s+$', 'g'), '');
		var tokens = cmd.split(new RegExp('\s+', 'g'));
		
		var cmdName = tokens[0];
		if(cmdName.length) {
			// find the most suitable command
			var cmds = this.resolveCmd(cmdName);
			switch(cmds.length) {
			case 0: print('Нет такой команды ' + cmdName); break;
			case 1: cmds[0].run(this, tokens); break;
			default: print('Команду ' + cmdName + ' можно понять по-разному'); 
			}
		} else {
			// default command
			this.display = this.display.getNextDisplay(this);
		}
	} while(!this.exit);
};

IcqStat.prototype.addCommand = function(names, desc, run) {
	this.commands.push({
			names: names, desc: desc, run: run
		});
};

IcqStat.prototype.initShell = function() {
	this.commands = [];
	this.addCommand(['help', 'usage'/*, '?'*/],
		'Напечатать сводку команд',
		function(prog) {
			var UsageDisplay = extend(Display, function(prog) {
					this.prevDisplay = prog.display;
				});
										
			UsageDisplay.prototype.render = function(prog) {
				for(var i in prog.commands) {
					var cmd = prog.commands[i], text = null;
					for(var j in cmd.names) {
						var name = cmd.names[j];
						for(var k = 1; k <= name.length; k++) {
							var part = name.substring(0, k);
							//trace('resolving ' + part);
							var resolutions = prog.resolveCmd(part);
							if(resolutions.length < 2) break;
						}
						if(name.length > part.length)
							part += '[' + name.substring(part.length) + ']';
						text = text ? text + "\n" + part : part;
					}
					// align
					for(var width = part.length; width < 20; width++)
						text += ' ';
					print(text + cmd.desc);
				}
				print("Пустая команда показывает продолжение текущего вида");
			};
										
			UsageDisplay.prototype.getNextDisplay = function() {
				return this.prevDisplay;
			}

			trace('setting UsageDisplay');
			prog.display = new UsageDisplay(prog);
		});
};
													
													
												
// returns array
IcqStat.prototype.resolveCmd = function(name) {
	for(var i = 0, r = []; i < this.commands.length; i++) {
		var cmd = this.commands[i];
		if(cmd.names.join("\n").match(new RegExp('^' + name, 'm'))) {
			r.push(cmd);
			break;
		}
	}
	return r;
};
	


// ------ Icqstat display -----------------------

WordStatDisplay = extend(Display, function(prog) {
		this.skip = 0;
		this.lim = 25;
		this.factor = 1; // -1 for reverse
		this.order = 'outgoing';
	});

WordStatDisplay.prototype.render = function(prog) {
	print('words sorted by ' + this.order + 
				' frequency starting at ' + this.skip)
	print('');
	// Упорядочим по количеству вхождений
	prog.wordsArr.sort(('incoming' == this.order) ?
										 function(l, r) {
											 if(l.incoming.length == r.incoming.length) return 0;
											 return l.incoming.length < r.incoming.length ? 1 : -1;
										 } :
										 function(l, r) {
											 if(l.outgoing.length == r.outgoing.length) return 0;
											 return l.outgoing.length < r.outgoing.length ? 1 : -1;
										 });
	

	// Выведем
	var lim = this.lim, skip = this.skip;
	print("\tВход\tВыход\tСлово");
	print("--------------------------------------------------");
	for(var i in prog.wordsArr) {
		var stat = prog.wordsArr[i];
		if(prog.isStopWord(stat.word)) continue;
		if(skip-- > 0) continue;
		if(!--lim) break;

		print("\t" + stat.incoming.length + 
					"\t" + stat.outgoing.length + 
					"\t" + stat.word);
	}
};

WordStatDisplay.prototype.getNextDisplay = function(prog) {
	this.skip += this.lim;
	return this;
};




// main program

var prog = new IcqStat();
prog.loadArgs(WScript.Arguments);
prog.loadConf();
prog.run();
