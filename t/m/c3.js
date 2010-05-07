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


function Connection() {
	this.ins = {};
}

Connection.prototype.connect = function() {
	var path = "wss://" + location.hostname + "/s";
	var s = new WebSocket(path), c = this;
	this.s = s;
	s.onopen = function(ev) {
		setStatus("Joining...");
		var m = document.cookie.match("dontpanic_p=([^;]+)");
		if(m) {
			var data = m[1].split('|');
			m = {
				what: "openPerson",
				id: data[0],
				password: data[1]
			};
		} else {
			var m = {
				what: "createPerson"
			};
		}
		c.send(m);
	};
	
	s.onmessage = function(ev) { c.onmessage(ev); };

	s.onclose = function(ev) {
		setStatus("Closed");
		alert(JSON.stringify(ev));
		
	};

	s.onerror = function(ev) {
		alert(JSON.stringify(ev));
	};
};

Connection.prototype.send = function(m) {
	this.s.send(JSON.stringify(m));
};

Connection.prototype.onmessage = function(ev) {
	hideStatus();
	if(ev.data.charAt(0) == '{') {
		var m = JSON.parse(ev.data);
	} else {
		var m = {errMsg: ev.data};
	}

	if(m.errMsg) {
		alert("Connection: " + m.errMsg);
		return;
	}

	if("person" == m.what) {
		document.cookie = "dontpanic_p=" + m.id + '|' + m.password;
	} else {
		if(!m.dn) {
			warn(ev.data + ": No destignation!");
			return;
		}
		
		var client = this.ins[m.dn];
		
		if(!client) { 
			if(this.person) {
				alert("SECOND PERSON!");
			}
			this.person = client = new In(m.dn);
			var b = new RoomBrowser();
			room.addObserver(b);
			client.addObserver(b);
		}
		client.processMessage(m);
	}
};

Connection.prototype.areWe = function(id) {
	return this.person && this.person.id == id;
};


function In(id) {
	this.id = id;
	this.version = -1;
	this.observers = [];
	this.state = {what: "state"};
	connection.ins[id] = this;
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


//
//     UI
//

function RoomBrowser() {
	this.people = {};
	this.invitations = [];

	var stage = document.body;
	this.node = stage.appendChild(document.createElement("DIV"));
	this.node.className = "roomBrowser";

	this.peoplePanel = this.node.appendChild(document.createElement("DIV"));
	this.peoplePanel.className = "people";
	var h = this.peoplePanel.appendChild(document.createElement("H1"));
	h.innerHTML = "People";
	this.peopleStage = this.peoplePanel.appendChild(document.createElement("DIV"));
	this.peopleStage.className = "peopleStage";
	/*this.invitePanel = this.peoplePanel.appendChild(document.createElement("DIV"));
	this.inviteBtn = this.invitePanel.appendChild(document.createElement("BUTTON"));
	this.inviteBtn.innerHTML = "invite";*/

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
	this.invitationsStage = this.conversationPanel.appendChild(
		document.createElement("DIV"));

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
	this.sayInp.onkeydown = function(ev) {
		if(13 == ev.keyCode) 
			b.say();
	};

	this.loginBtn.onclick = function() { b.login(); };
	this.nameInp.onkeydown = function(ev) {
		if(13 == ev.keyCode)
			b.login();
	};

}

			

RoomBrowser.prototype.login = function() {
	var name = this.nameInp.value;
	if(name) {
		var m = {
			what: "updatePerson",
			name: name
		};
		setStatus("Sending...");
		connection.send(m);
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
		connection.send(m);
		this.sayInp.value = "";
	}
};

RoomBrowser.prototype.updatePerson = function(p) {
	var person = this.people[p.id];
	if(!person) {
		warn('No person: ' + p.id);
		return;
	}

	var node = person.nameNode;
	person.name = p.name;
	node.className = "person";
	if(p.name) {
		node.className += " withName"; 
		node.innerHTML = p.name;
	} else {
		node.innerHTML = "*" + p.id + "*";
	}

	if(connection.person && connection.person.id == p.id)
		node.className += " you";
	this.updateConversation();
};		
	

RoomBrowser.prototype.addPerson = function(p) {
	p.node = this.peopleStage.appendChild(
		document.createElement("DIV"));
	p.nameNode = p.node.appendChild(document.createElement("SPAN"));
	if(!connection.areWe(p.id)) {
		p.inviteBtn = p.node.appendChild(document.createElement("BUTTON"));
		p.inviteBtn.innerHTML = "Invite";
		var b = this, id = p.id;
		p.inviteBtn.onclick = function() {
			var m = {
				what: "createInvitation",
				target: id
			};
			setStatus("Inviting...");
			connection.send(m);
		};
	}

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

RoomBrowser.prototype.createPersonsNameNode = function(id) {
	var p = this.people[id], n = document.createElement("SPAN");
	n.className = "person";
	if(p && p.name) {
		n.innerHTML = p.name;
		n.className += " withName";
	} else {
		n.innerHTML = "*" + id + "*";
	}

	if(connection.person && connection.person.id == id) {
		n.className += " you";
	}
	
	return n;
}

RoomBrowser.prototype.addInvitation = function(inv) {
	this.invitations.push(inv);
	inv.node = this.invitationsStage.appendChild(document.createElement("DIV"));
	inv.btn = inv.node.appendChild(document.createElement("BUTTON"));
	var s = inv.btn.appendChild(document.createElement("SPAN"));
	s.innerHTML = "Play with ";
	inv.btn.appendChild(this.createPersonsNameNode(inv.src));
};


function TicTacToeBrowser(state) {
	
	
RoomBrowser.prototype.addGame = function(m) {
	this.games[m.id] = 

RoomBrowser.prototype.processMessage = function(m, client) {
	if(m.game) {
		this.games[m.game].processMessage(m, client);
		return;
	}

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
			if(m.invitations) {
				this.invitations = [];
				this.invitationsStage.innerHTML = "";
				for(var i in this.invitations)
					this.addInvitation(this.invitations[i]);
			}

			if(m.games) {
				this.games = {};
				this.gamesStage.innerHTML = "";
				for(var id in m.games)
					this.addGame(m.games[i]);
			}
		}
		break;

	case "createGame":
		this.addGame(m);
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

	case "createInvitation":
		this.addInvitation(m);
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
	connection = new Connection();
	room = new In("room");
	connection.connect();
} catch(e) {
	alert(e);
}

