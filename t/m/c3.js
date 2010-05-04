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
	ins[id] = this;
}

In.prototype.reload = function() {
	var m = {
		what: "reload",
		id: this.id
	}
	s.send(JSON.stringify(m));
};


function connect() {
	s = new WebSocket("ws://" + location.host + "/s");
	s.onopen = function(ev) {
		setStatus("reloading...");
		for(var id in ins)
			ins[id].reload();
	};
	
	s.onmessage = function(ev) {
		alert(JSON.stringify(ev));
		
	};
	
	s.onclose = function(ev) {
		setStatus("Closed");
	};
}


function RoomBrowser() {
	var stage = document.body;
	this.node = stage.appendChild(document.createElement("DIV"));
	this.node.className = "roomBrowser";

	this.peoplePanel = this.node.appendChild(document.createElement("DIV"));
	this.peoplePanel.className = "people";

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
	
	this.conversationPanel = this.chatPanel.appendChild(
		document.createElement("DIV"));
	this.conversationPanel.className = "conversation";

	this.tribunePanel = this.chatPanel.appendChild(document.createElement("DIV"));
	this.tribunePanel.className = "send";
	
	this.sayInp = this.tribunePanel.appendChild(document.createElement("INPUT"));
	this.sayInp.type = "text";
	this.sayBtn = this.tribunePanel.appendChild(document.createElement("BUTTON"));
	this.sayBtn.innerHTML = "Say";

	var b = this;
	this.sayBtn.onclick = function() {};
}

room = new In("room");
connect();
new RoomBrowser();	

