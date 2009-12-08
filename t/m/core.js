/*
	Anxiety
	-------

	This is a client for a multiplayer card game using Dojo framework.

	EVENT MODEL / POLICY
	
	If user initiates an operation, we're interested in status of that. This status 
	should be shown on screen, and then when we get a result, it should be associated
	with that particular operation. This operation should be represented as a 
	Deferred. 
	An exeception is the state watching/updateing process, which should not be 
	reported, because it runs all the time.

	If some event is not result of an user-requested operation, we should present it
	as a signal (dojo.connect()).

	TODO
	 - don't load "Tabs" module until we log in 
	 - make "Player" and "PlayerBrowser" a separate module and load them when we 
       log in
	 - get games to their own js files an don't load them until we must use them

	IDEAS
 	 - maybe we'd better use regular <button>s to save cycles and they look nicer too
	 - would be really cool if I made a 'loading...' progress bar, like in GMail
*/

/*	Utils
		-----
*/
function nop() {} 

function DBG(x) {
	if(typeof console != 'undefined')
		console.log(x);
	return x;
}

function cfirst(s) {
	return s.charAt(0).toUpperCase() + s.substring(1);
}

function checkProps(o /*, ...*/) {
	for(var i = arguments.length - 1; i > 0; i--) 
		if(!o[arguments[i]])
			throw 'Property ' + arguments[i] + ' expected';
}

Widg = function() {};
Widg.prototype = {
	connect: function(sourceObj, sourceName, destName) {
		return dojo.connect(sourceObj, sourceName, this, destName);
	}

	,report: function(str, deferred) {
		if(!this.statusBarNode) {
			this.statusBarNode = dojo.create('SPAN', {
					className: 'statusBar'
				}, this.domNode || document.body);
		}
					
		if(deferred) {
			var node = dojo.create('SPAN', {}, this.statusBarNode);
			node.innerHTML = str;
						
			// Clear after done.
			return deferred.addBoth(function() {
					dojo.destroy(node)
						});
		} else {
			if(!this.simpleStatusBarNode)
				this.simpleStatusBarNode = dojo.create('SPAN', {}, this.statusBarNode);
			this.simpleStatusBarNode.innerHTML = str;
			dojo.style(this.simpleStatusBarNode, {
					visibility: str ? 'visible' : 'hidden'});
		}
	}

	,addPane: function(name) {
		this[name] = dojo.create('DIV', {className: name}, this.domNode || this.node);
	}
};

Widg.prototype.report('Loading browser...');
			
sometimes = {};

/*
	Servers
	-------
	
	Base for Game and World classes.
*/
sometimes.Server = function(urlPrefix) {
	this.urlPrefix = urlPrefix;
};

sometimes.Server.prototype.send = function(what, whatElse) {
	whatElse = whatElse || {};
	whatElse.what = what;
	DBG('Posting to ' + this.urlPrefix + ':');
	DBG(whatElse);
	return dojo.xhrPost({url: this.urlPrefix,
				content: whatElse, 
				handleAs: 'json', // OK (2**) answer will be handled as JSON.
				}).addErrback(function(er) {
						// Try to get some information we can use
						if(er.responseText) {
							if('{' == er.responseText.charAt(0)) {
								var data = dojo.fromJson(er.responseText);
								er = data.err;
							}	else
								er.msg = er.responseText;
						} else 
							er.msg = er + '';
						return er;
					})
	/*  It seems when load: function throws an exception,
			error: is called. That's not what I want though.
	*/
};
				


/*
	World object
	````` ``````
*/
sometimes.World = function(urlPrefix) {
	sometimes.Server.apply(this, arguments);
	WD = this; // debug
	// Do handshakes and stuff
	dojox.cometd.init(urlPrefix + 'cometd');
	this.players = new sometimes.PlayerList(this);
};

sometimes.World.prototype.login = function() {
	/**	Initiates a login request. 
			Returns a Deferred.
	*/
	var rv = this.send('needClient', {}).
	addCallback(dojo.hitch(this, function(rsp) {
				return new sometimes.Player(this, rsp.id, rsp.priv);
			}));
	return rv;
};
		
/*
	Client core
	------ ----
	
	This class calls handleXxx methods.
*/

sometimes.MsgDriven = function() {
	this.listeners = [];
};

sometimes.MsgDriven.prototype = {
	processMsg: function(msg) {
		if(!msg.what) {
			DBG('No selector field:' + msg);
			return;
		}
								
		var methodName = 'handle' + cfirst(msg.what);
		if(this[methodName]) {
			var validatingMethodName = 'check' + cfirst(msg.what);
			this[validatingMethodName] && this[validatingMethodName](msg);
			var obs = this, i = 0;
			do {
				if(obs[methodName]) {
					try {
						obs[methodName](msg, this);
					} catch(e) {
						DBG('In ' + methodName + ':');
						DBG(e);
					}
				} else {
					obs.update(this.state, this);
				}
			} while(obs = this.listeners[i++]);
		} else {
			// Slobby branch for debugging purposes
			DBG('No handling method for ' + msg.what);
			this.update();
		}
	}
};

sometimes.MsgDriven.prototype.connect = function(widget) {
	this.listeners.push(widget);
	if(this.state)
		widget.update(this.state, this);
};
				
sometimes.Client = function(world, id, priv) {
	sometimes.MsgDriven.apply(this, arguments);
	this.id = id;
	this.priv = priv;
	this.world = world;
	this.urlPrefix = world.urlPrefix;
	this.revision = -1;
			
	// Subscribe to events.
	// TODO Authenticate.
	dojox.cometd.subscribe('/' + id, this, function(fullMsg) {
			var msg = fullMsg.data;
			DBG('Message from cometd:');
			DBG(msg);
			// any output we can't handle will be silently ignored
			if(!('revision' in msg)) {
				DBG('No revision number:' + msg);
				return;
			}

			if(msg.revision > this.revision) {
				// Got a message from the future, must go there
				this.reload();
			} else if(msg.revision == this.revision) {
				// Looks like something we can handle
				this.processMsg(msg);
			} 
			// If msg.revision < this.revision , then our rev is 
			// older, this is a message from past => do nothing
		});

};


sometimes.Client.prototype.destroy = function() {
	dojox.cometd.unsubscribe('/' + this.id);
	this.listeners = [];
};

sometimes.Client.prototype.reload = function() {
	/**  Fetch and update the whole client state.
	 */ 
	if(this._reloading) // Don't do more than one requests at a time.
		return;
	this._reloading = dojo.xhrGet({
			url: this.urlPrefix + 'clients/' + id,
			handleAs: 'json',
			preventCache: true,
			content: {priv: priv},
			
			error: dojo.hitch(this, function(e) {
					// TODO if server tells us id or password are invalid,
					// we must tell observers this client is broken and then destroy it
					DBG('Failed reloading client ' + id + ':');
					DBG(e);
					var er = mkErr(e);
					
				}),
					
			load: dojo.hitch(this, function(state) {
					this._reloading = false;
					if(!('revision' in state))
						throw 'No revision in state!';
					this.state = state;
					this.update && this.update(state, this); 
					this.revision = state.revision;
					this.updateObservers();
				});
		});
};
						
sometimes.Client.prototype.updateObservers = function() {
	for(var i in this.listeners)
		this.listeners[i].update(state, this);
};
							
/*
	PlayerList
	``````````
*/

sometimes.PlayerList = function(world) {
	sometimes.Client.apply(this, [world, 'players']);
};
		
var _Client = function() {};
_Client.prototype = sometimes.Client.prototype;
sometimes.PlayerList.prototype = new _Client;  


//   Command handlers
//   ``````` ````````

sometimes.PlayerList.prototype.update = function(state) {
	this.chatLog = state.chatLog;
	// We own this object, if anyone else wants to use it, make copy!
	this.players = state.players;
};
				
sometimes.PlayerList.prototype.checkAddPlayer = function(m) {
	checkProps(m, 'id', 'name');
};
					
sometimes.PlayerList.prototype.handleAddPlayer = function(msg) {
	this.players.push(msg);
};

sometimes.PlayerList.prototype.checkRmPlayer = function(m) {
	checkProps(m, 'who');
};
		
sometimes.PlayerList.prototype.handleRmPlayer = function(msg) {
	for(var i in this.players)
		if(this.players[i].id == msg.who) {
			this.players.splice(i, 1);
			break;
		}
};

/* 
	 Player
	 ``````
	 
*/
sometimes.Player = function(world, id, priv) {
	sometimes.Client.apply(this, arguments);
	this.games = {};
};

sometimes.Player.prototype = new _Client;

sometimes.Player.prototype.processMsg = function(m) {
	if(m.gameId) {
		var gm = this.games[m.gameId];
		if(!gm) {
			DBG('Got message for nonexistent game ' + m.gameId);
			return;
		}
		gm.processMsg(m, this);
	} else {
		sometimes.Client.prototype.processMsg.apply(this, [m]);
			}
};

//   APIs
//	 ````
					
sometimes.Player.prototype.say = function(phrase) {
	return this.send('chat', {
			phrase: phrase
		});
};

sometimes.Player.prototype.logOut = function() {
	// Regardless of what server says, we're out of here
	var rv = this.send('iQuit');
	// In fact, we don't have to wait for server's response
	// Tell everyone
	this.state = {
		broken: {msg: 'Logged out'}
	};
	this.updateObservers();
	// still needs to be destroy()ed
	return rv;
};

sometimes.Player.prototype.invite = function(gameType, playerIds) {
	return this.send('invite', {
			gameType: gameType,
			target: playerIds.join(' ')
		});
};

sometimes.Player.prototype.explainWhyWeCantStart = function(
	gameType, partnerIds) {
	var mod = sometimes.games[gameType];
	if(!mod) 
		return 'Game "' + gameType + '" is not supported!';
	return mod.explainWhyWeCantStart(partnerIds);
};

sometimes.Player.prototype.startGame = function(gameType, partnerIds) {
	/** Returns a Deferred for caller to know when operation is over
	 */
	var excuse = this.explainWhyWeCantStart(gameType, partnerIds);
	if(excuse) {
		var er = {msg: excuse};
		return (new dojo.Deferred()).errback(er);
	} else {
		return this.send('startGame', {
				gameType: gameType,
				'with': partnerIds.join(' ')
			});
	}
};

//   Utils
//   `````

sometimes.Player.prototype.send = function(what, whatElse) {
	whatElse = whatElse || {}; 
	whatElse.who = this.id;
	whatElse.priv = this.priv;
	return this.world.send(what, whatElse);
};

//   Client message handlers
//   `````` ``````` ````````

sometimes.Player.prototype.handleInvited = function(m) {
	checkProps(m, 'who', 'gameType');
};

sometimes.Player.prototype.checkUninvited = function(m) {
	checkProps(m, 'who', 'gameType');
};

sometimes.Player.prototype.handleAddGame = function(m) {
	checkProps(m, 'game');
	checkProps(m.game, 'gameType', 'id');
	// Construct an appropriate game object.
	var mod = anxiety.games[m.game.gameType];
	if(!mod) 
		throw 'Game "' + m.game.gameType + '" is not supported';
	checkProps(mod, 'mkGame');
	var game = mod.mkGame(m.game);
	game.mod = mod;
	this.games[game.id] = game;
};

sometimes.Player.prototype.update = function(state) {
	/* Update games */
	for(var id in state.games) {
		
	}
};

/*	Imports
		-------
*/
dojo.require('dojo.cookie');
// TODO We sure need to use cookies to relogin,
// but in the same time I want to allow multiple sessions in 
// different browser windows/tabs

dojo.require('dojox.cometd');

/*
	Browser code
	------- ----

	Cross-domain script loading is async and who knows what else is
	So we need to define classes on load.
*/
dojo.addOnLoad(function() {


		/*  
				Games
				-----
				
				Each game type is a module named anxiety.games[gameType]
		*/
		dojo.declare('anxiety.Game', sometimes.MsgDriven, {
				constructor: function(state) {
					this.id = state.id;
					this.gameType = state.gameType;
				}

			});

		/*dojo.declare('anxiety.GameBrowser', [dijit.layout.ContentPane], {
				constructor: function(opts) {

				}
				
				});*/

		dojo.declare('anxiety.CardWidget', null, {
				constructor: function() {
					this.cardNodes = {};
				}

				,getCardNode: function(card) {
					if(!this.cardNodes[card]) {
						var cn = dojo.create('IMG', {
								src: dojo.moduleUrl('anxiety', 'cards/' + card + '.gif')
							});
						this.cardNodes[card] = cn;
					}
					return this.cardNodes[card];
				}

				,putCard: function(card, parentNode) {
					dojo.place(this.getCardNode(card), parentNode);
				}

				,unputCard: function(card) {
					dojo.orphan(this.getCardNode(card));
				}

				,cardClicked: nop
			});
					


		/*  
			  Thousand
				--------

				TODO Make a module
		*/
		dojo.declare('anxiety.games.thousand.GameFor3', anxiety.Game, {
				constructor: function(s) {
					// This will be called any time we update a whole state
					this.score = s.score;
					this.round = s.round;
				}

				//   Message handlers
				//   ``````` ````````

				,checkSetRound: function(m) {
					checkProps(m, 'round');
					checkProps(m.round, 'hand', 'table');
				}

				,handleSetRound: function(m) {

				}

				,checkCards: function(m) {
					checkProps(m, 'cards');
				}

				,handleCards: function(m) {
					this.round.hand = m.cards.concat(this.round.hand);
					this.tookCards(m.cards);
				}

				,handleBid: function(m) {

				}

				,handlePass: function(m) {


				}
				
				,tookCards: nop, PASSED: -1
			});
			
		dojo.mixin(anxiety.games.thousand, {
				getFriendlyName: function() {
					return 'thousand';
				}
					
				,explainWhyWeCantStart: function(partnerIds) {
					return (2 != partnerIds.length) && "Need exactly 2 partners";
				}
				
				,mkGame: function(state) {
					return new anxiety.games.thousand.GameFor3(state);
				}

				,browse: function(game, player) {
					return new anxiety.games.thousand.BrowserFor3({game: game, 
								player: player});
				}
			});

		var bs = [anxiety.GameBrowser, anxiety.CardWidget];
		dojo.declare('anxiety.games.thousand.BrowserFor3', bs, {
				postCreate: function() {
					/* Buid a display for non round variables */
					this.gameScore = dojo.create('DIV', {className: 'gameScore'}, 
						this.domNode);
					
					this.browse(this.game);
				}

				,browse: function(game) {
					if(this.game) {
						// TODO Cleanup connections.
					}
					
					this.connect(game, 'handleSetRound', 'browseRound');
					this.connect(game, 'tookCards', 'takeCards');
					this.game = game;
				}

				,takeCards: function(cards) {
					if(!this.mySector) 
						throw "Can't take cards, probably didn't receive setRound";
				

					for(var i in cards) 
						this.putCard(cards[i], this.mySector.hand);
				}

				,browseRound: function(m) {
					var round = m.round;
					
					var tableToken = '';
					for(var i in round.table) 
						tableToken += round.table[i].playerId;

					if(this.tableToken != tableToken) {
						/* Erase current table DOM */

						this.table = [];
						/* Set up a table */
						// round.table array is arranged in moving order
						for(var i in round.table) {
							var part = round.table[i];
							var stat = this.game.world.players.stat(part.playerId);
							if(!stat) {
								DBG('No stat for player ' + part.playerId);
								continue;
							}

							var sector = {
								node: dojo.create('DIV', {className: 'sector'})
							};
							
							dojo.create('h1', {innerHTML: stat.name}, sector.node);
							sector.card = dojo.create('DIV', {className: 'card'}, sector.node);
							sector.bid = dojo.create('DIV', {className: 'bid'}, sector.node);
							if(part.id == this.player.id) {
								this.mySector = sector;
								sector.hand = dojo.create('DIV', {className: 'hand'}, sector.node);

							}
							
							this.table.push(sector);
						}

						this.tableToken = tableToken;
					}
					
					/* Display sector states */
					for(var i in round.table) {
						var part = round.table[i], sector = this.table[i];
									
						if(part.card)
							this.putCard(part.card, sector.card);
						
						if(part.bid)
							sector.bid.innerHTML = (part.bid == this.game.PASSED) ?
								'PASSED' :
								part.bid;
					}

					/* Display the hand */
					for(var i in round.hand) {
						this.putCard(round.hand[i], this.mySector.hand);
					}

					/* Display the trump suit */

				}
			});
				

		
	});



