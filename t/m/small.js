/*
	Mobile interface.

	No tabs/resizers here.
*/

dojo.require('anxiety');

function browse(urlPrefix, l) {
	dojo.addOnLoad(function() {
			var w  = new anxiety.World(urlPrefix);
			var b = new anxiety.SmallWorldBrowser({world: w}, l);
		});
}

//dojo.declare('anxiety.SmallWorldBrowser', [], {

dojo.addOnLoad(function() {
		var Widg = function() {};
		Widg.prototype = {
			connect: function(sourceObj, sourceName, destName) {
				return dojo.connect(sourceObj, sourceName, this, destName);
			}
		};
			
		anxiety.SmallWorldBrowser = function(opts, l) {
			this.world = opts.world;
			this.node = l || dojo.create('DIV', {className: 'smalWorldBrowser'}, 
				document.body);
			this.playersFetch.addCallback(dojo.hitch(this, function(players) {
						this.chatPane = dojo.create('DIV', {
								className: 'chatPane'
							}, this.node);
						
						players.passStateToWidget(this);
					}));
		};
		
		anxiety.SmallWorldBrowser.prototype = new Widg;

		anxiety.SmallWorldBrowser.prototype.handleHeard = function(m, players) {
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
					
			// TODO Scroll
		};
			
		anxiety.SmallWorldBrowser.prototype.update = function(state, players) {
			this.chatPane.innerHTML = '';
			for(var i in players.chatLog) {
				this.handleHeard(players.chatLog[i]);
			}
		};

	});
			
			
			
			
			
