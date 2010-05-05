if(console) {
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

setStatus("Connecting...");

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
	var path = "ws://" + location.host + "/s"
	alert(path);
	s = new WebSocket(path);
	s.onopen = function(ev) {
		setStatus("Joining...");
		var m = {
			what: "newHere"
		};
		send(m);
	};
	
	s.onmessage = function(ev) {
		hideStatus();
		//alert(ev.data);
		var m = JSON.parse(ev.data);
		var id = m.id;
		if(!id) {
			warn(ev.data + ": No id!");
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
	};
}

function send(m) {
	s.send(JSON.stringify(m));
}


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

	this.loginPanel = this.node.appendChild(document.createElement("DIV"));
	this.loginPanel.className = "login";
	this.nameInp = this.loginPanel.appendChild(document.createElement("INPUT"));
	this.loginBtn = this.loginPanel.appendChild(document.createElement("BUTTON"));
	this.loginBtn.innerHTML = "Login";
	
	this.logoutPanel = this.node.appendChild(document.createElement("DIV"));
	this.logoutPanel.className = "logout";
	this.logoutBtn = this.logoutPanel.appendChild(document.createElement("BUTTON"));
	this.logoutBtn.innerHTML = "Logout";
	
	this.chatPanel = this.node.appendChild(document.createElement("DIV"));
	this.chatPanel.className = "chat";
	var h = this.chatPanel.appendChild(document.createElement("H1"));
	h.innerHTML = "Conversation";
	this.conversationPanel = this.chatPanel.appendChild(
		document.createElement("DIV"));
	this.conversationPanel.className = "conversation";

	this.tribunePanel = this.chatPanel.appendChild(document.createElement("DIV"));
	this.tribunePanel.className = "send";
	
	this.sayInp = this.tribunePanel.appendChild(document.createElement("INPUT"));
	this.sayInp.type = "text";
	this.sayBtn = this.tribunePanel.appendChild(document.createElement("BUTTON"));
	this.sayBtn.innerHTML = "Say";

	this.loginPanel.style.display = 'none';
	this.logoutPanel.style.display = 'none';

	var b = this;
	this.sayBtn.onclick = function() { b.say(); };
	document.onkeydown = function(ev) {
		if(13 == ev.keyCode) 
			b.say();
	};
}

RoomBrowser.prototype.say = function() {
	var speech = this.sayInp.value;
	if(speech) {
		var m = {
			what: "iSay",
			speech: speech
		};
		setStatus("Sending...");
		send(m);
		this.sayInp.value = "";
	}
};

RoomBrowser.prototype.addChatItem = function(item) {
	var node = this.conversationPanel.appendChild(document.createElement("DIV"));
	node.className = "chatItem";
	node.innerHTML = "<b>" + item.speaker + "</b>: " + item.speech;
};
	

RoomBrowser.prototype.processMessage = function(m, client) {
	switch(m.what) {
	case "state":
		if("room" == m.id) {
			this.peopleStage.innerHTML = "";
			if(m.people) {
				for(var i in m.people) {
					var p = m.people[i];
					var node = this.peopleStage.appendChild(
						document.createElement("DIV"));
					node.className = "person";
					if(p.name) {
						node.className += "withName"; 
						node.innerHTML = p.name;
					} else {
						node.innerHTML = "*" + p.id + "*";
					}
				}
			}

			if(m.chat) {
				this.conversationPanel.innerHTML = "";
				for(var i in m.chat) {
					var item = m.chat[i];
					this.addChatItem(item);
				}
			}
		} else { // player state here



		}
		break;

	case "iSay":
		this.addChatItem(m);
	}
}
					
					

try {
	if(!("WebSocket" in window))
		throw "No WebSockets!";
	room = new In("room");
	connect();
	//new RoomBrowser();	
} catch(e) {
	alert(e);
}

