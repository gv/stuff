if("console" in window) {
	warn = function(x) { console.log(x); };
} else {
	warn = function(x) { /* TODO */ };
}

var statusNode = document.getElementById("status");

function hideStatus() {
	statusNode.style.display = "none";
}

function setStatus(s) {
	statusNode.style.display = "";
	statusNode.innerHTML = s;
}


//
//    CONNECTION
//

ins = {};

function In(id) {
	this.id = id;
	this.version = -1;
	this.observers = [];
	this.state = {what: "state"};
	ins[id] = this;
}

In.prototype.addObserver = function(obs) {
	this.observers.push(obs);
	obs.processMessage(this.state, this);
	//obs.update(this);
}

In.prototype.processMessage = function(m) {
	if("state" == m.what) {
		this.state = m;
	}

	for(var i in this.observers) {
		this.observers[i].processMessage(m, this);
	}
}

/*In.prototype.reload = function() {
	var m = {
		what: "reload",
		id: this.id
	}
	s.send(JSON.stringify(m));
};*/


function connect() {
	var path = //location.protocol.replace("http", "ws") + 
		"wss://" + location.hostname + "/s";
	//alert(path);
	s = new WebSocket(path);
	s.onopen = function(ev) {
		setStatus("Joining...");
		var m = {
			what: "createPerson"
		};
		send(m);
	};
	
	s.onmessage = function(ev) {
		hideStatus();
		//alert(ev.data);
		var m = JSON.parse(ev.data);
		var id = m.dn;
		if(!id) {
			warn(ev.data + ": No destignation!");
			return;
		}
		
		var client = ins[id];
		
		if(!client) { // ASSUMING WE DON'T HAVE A CLIENT YET
			client = new In(id);
			var b = new RoomBrowser();
			room.addObserver(b);
			client.addObserver(b);
		}
		client.processMessage(m);
	};
	
	s.onclose = function(ev) {
		setStatus("Closed");
		alert(JSON.stringify(ev));
		
	};

	s.onerror = function(ev) {
		alert(JSON.stringify(ev));
	};
}

function send(m) {
	s.send(JSON.stringify(m));
}


//
//     UI
//

function RoomBrowser() {
	var stage = document.body;
	this.node = stage.appendChild(document.createElement("DIV"));
	this.node.className = "roomBrowser";

	this.peoplePanel = this.node.appendChild(document.createElement("DIV"));
	this.peoplePanel.className = "people";
	var h = this.peoplePanel.appendChild(document.createElement("H1"));
	h.innerHTML = "People";
	this.peopleStage = this.peoplePanel.appendChild(document.createElement("DIV"));
	this.peopleStage.className = "peopleStage";
	this.people = {};

	this.loginPanel = this.node.appendChild(document.createElement("DIV"));
	this.loginPanel.className = "login";
	this.nameInp = this.loginPanel.appendChild(document.createElement("INPUT"));
	this.loginBtn = this.loginPanel.appendChild(document.createElement("BUTTON"));
	this.loginBtn.innerHTML = "Login";
	
	this.logoutPanel = this.node.appendChild(document.createElement("DIV"));
	this.logoutPanel.className = "logout";
	this.logoutBtn = this.logoutPanel.appendChild(document.createElement("BUTTON"));
	this.logoutBtn.innerHTML = "Logout";
	
	this.conversationPanel = this.node.appendChild(document.createElement("DIV"));
	this.conversationPanel.className = "conversation";
	var h = this.conversationPanel.appendChild(document.createElement("H1"));
	h.innerHTML = "Conversation";
	this.conversationStage = this.conversationPanel.appendChild(
		document.createElement("DIV"));
	this.conversationStage.className = "comments";

	this.tribunePanel = this.conversationPanel.appendChild(
		document.createElement("DIV"));
	this.tribunePanel.className = "tribune";
	
	this.sayInp = this.tribunePanel.appendChild(document.createElement("INPUT"));
	this.sayInp.type = "text";
	this.sayBtn = this.tribunePanel.appendChild(document.createElement("BUTTON"));
	this.sayBtn.innerHTML = "Say";

	this.logoutPanel.style.display = 'none';

	var b = this;
	this.sayBtn.onclick = function() { b.say(); };
	document.onkeydown = function(ev) {
		if(13 == ev.keyCode) 
			b.say();
	};

	this.loginBtn.onclick = function() { b.login(); };
}

RoomBrowser.prototype.login = function() {
	var name = this.nameInp.value;
	if(name) {
		var m = {
			what: "updatePerson",
			name: name
		};
		setStatus("Sending...");
		send(m);
	}
};

RoomBrowser.prototype.say = function() {
	var text = this.sayInp.value;
	if(text) {
		var m = {
			what: "createComment",
			text: text
		};
		setStatus("Sending...");
		send(m);
		this.sayInp.value = "";
	}
};

RoomBrowser.prototype.updatePerson = function(p) {
	var person = this.people[p.id];
	if(!person) {
		warn('No person: ' + p.id);
		return;
	}

	var node = person.node;
	person.name = p.name;
	node.className = "person";
	if(p.name) {
		node.className += " withName"; 
		node.innerHTML = p.name;
	} else {
		node.innerHTML = "*" + p.id + "*";
	}
	this.updateConversation();
};		
	

RoomBrowser.prototype.addPerson = function(p) {
	p.node = this.peopleStage.appendChild(
		document.createElement("DIV"));
	this.people[p.id] = p;
	this.updatePerson(p);
};


RoomBrowser.prototype.updateConversation = function() {
	this.conversationStage.innerHTML = "";
	for(var i in this.conversation) {
		var item = this.conversation[i];
		this.addComment(item);
	}
};
	
RoomBrowser.prototype.addComment = function(item) {
	var node = this.conversationStage.appendChild(document.createElement("DIV"));
	node.className = "comment";
	var author = item.author;
	var a = this.people[author];
	if(a && a.name)
		author = a.name;
	node.innerHTML = "<b>" + author + "</b>: " + item.text;
};

RoomBrowser.prototype.processMessage = function(m, client) {
	switch(m.what) {
	case "state":
		if("room" == m.dn) {
			if(m.people) {
				this.people = {};
				this.peopleStage.innerHTML = "";
				for(var i in m.people) {
					var p = m.people[i];
					this.addPerson(p);
				}
			}

			if(m.conversation) {
				this.conversation = m.conversation;
				this.updateConversation();
			}
		} else { // player state here



		}
		break;

	case "createComment":
		this.conversation.push(m);
		this.addComment(m);
		break;

	case "createPerson":
		this.addPerson(m);
		break;

	case "updatePerson":
		this.updatePerson(m);
		break;
	}
}

//
//    MAIN
//   
					

try {
	if(!("WebSocket" in window))
		throw "No WebSockets!";
	setStatus("Connecting...");
	room = new In("room");
	connect();
	//new RoomBrowser();	
} catch(e) {
	alert(e);
}

