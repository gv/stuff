/*
	Mobile interface.

	No tabs/resizers here.
*/


function browse(urlPrefix, l) {
	dojo.addOnLoad(function() {
			var w  = new sometimes.World(urlPrefix);
			var b = new sometimes.SmallWorldBrowser({world: w});
		});
}


//dojo.addOnLoad(function() {


sometimes.SmallWorldBrowser = function(opts, l) {
	this.world = opts.world;
	this.node = this.domNode = l || dojo.create('DIV', {
			className: 'smalWorldBrowser'
		}, document.body);
	this.playerPane = dojo.create('DIV', {
			className: 'playerPane'
		}, this.node);
	this.chatPane = dojo.create('DIV', {
			className: 'chatPane'
		}, this.node);
	this.listPane  = dojo.create('DIV', { 
			className: 'listPane'
		}, this.node);
						
	this.world.players.connect(this);

	this.world.login().addCallback(dojo.hitch(this, function(player) {
				this.playerBrowser = new sometimes.SmallPlayerBrowser(player, 
					this.playerPane);
			}));
	this.report('Adding a player...');
};
		
sometimes.SmallWorldBrowser.prototype = new Widg;

sometimes.SmallWorldBrowser.prototype.update = function(state, players) {
	// update players list
	for(var i in state.players) {
		
	// update chat
	this.chatPane.innerHTML = '';
	for(var i in players.chatLog) {
		this.handleHeard(players.chatLog[i]);
	}
};

sometimes.SmallWorldBrowser.prototype.handleHeard = function(m, players) {
	// Someone said something!
	var author = players.stat(m.author), node = this.chatPane;
	if(author) {
		var nameBody = '<b>' + author.name + '</b>';
	} else {
		// Player's gone
		var nameBody = '*<b>' + m.author + '</b>*';
	}
			
	var phraseNode = dojo.create('DIV', {
			innerHTML: nameBody + ': ' + m.phrase
		}, node);
	// Don't display more then maxPhraseCnt phrases.
	var nodes = dojo.query('DIV', node);
	if(nodes.length > this.maxPhraseCnt) 
		nodes.slice(0, nodes.length - this.maxPhraseCnt).orphan();
};
			
sometimes.SmallPlayerBrowser = function(player, l) {
	this.node = l;
	this.player = player;
	this.addPane('headPane');
	this.nameInp = dojo.create('INPUT', {
		type: 'text',
		value: 'Name goes here'
		}, this.headPane);
	this.addPane('invitationsPane');
	this.addPane('gamesPane');
	player.connect(this);
};

sometimes.SmallPlayerBrowser.prototype = new Widg();

sometimes.SmallPlayerBrowser.prototype.update = function(state, player) {
	this.headerPane.innerHTML = state.name;
	

};
	
	
			
			
			
			
			
