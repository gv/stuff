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

/*	Imports
		-------
*/
dojo.require('dojo.cookie');
// TODO We sure need to use cookies to relogin,
// but in the same time I want to allow multiple sessions in 
// different browser windows/tabs

dojo.require('dojox.cometd');

//dojo.require('dijit.form.Button');

function mkBtn(opts, parent) {
	opts.innerHTML = opts.label;
	var b = dojo.create('BUTTON', opts, parent);
	b.onclick = function(e) {
		return this.onClick && this.onClick(e);
	};
	return b;
}

/*
	Browser code
	------- ----

	Cross-domain script loading is async and who knows what else is
	So we need to define classes on load.
*/
dojo.addOnLoad(function() {
		/*
			Client core
			------ ----

			This class calls handleXxx methods.
		*/
		dojo.declare('anxiety.MsgDriven', null, {
				processMsg: function(msg) {
					if(!msg.what) {
						DBG('No selector field:' + msg);
						return;
					}
								
					var validatingMethodName = 'check' + cfirst(msg.what);
					var methodName = 'handle' + cfirst(msg.what);
					if(this[methodName]) {
						try {
							this[validatingMethodName] && this[validatingMethodName](msg);
							this[methodName](msg);
							this.revision++;
						} catch(e) {
							DBG('In ' + methodName + ':');
							DBG(e);
						}
					} else {
						DBG('No handling method for ' + msg.what);
						this.reload();
					}
				}
			});

		/*
			This class runs a sync mechanism with server.
		*/
		dojo.declare('anxiety.Client', anxiety.MsgDriven, {
				
				//   API
				//   ```
				constructor: function(world, id, priv) {
					this.id = id;
					this.priv = priv;
					this.world = world;
					this.urlPrefix = world.urlPrefix;
					this.revision = -1;
					//this.listeners = [];

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

				}
					
				,passStateToWidget: function(widget, methodName) {
					methodName = methodName || 'update';
					// This way circular refs will be torn in widget.destroy()
					// Also, that's why you call this only in widget.postCreate()
					widget.connect(this, 'updateState', methodName);
					widget[methodName](this.state, this);

					// So, I guess it's finally time to get some use of all this
					// 'dynamic' stuff.
					var namePn = new RegExp('^handle');
					for(var propName in this) 
						if(namePn.test(propName) && widget[propName])
							widget.connect(this, propName, propName);
				}					

				,close: function() {
					return dojox.cometd.unsubscribe('/' + this.id);
				}

				,reload: function() {
					/**  Fetch and update the whole client state.
					 */ 
					if(this._reloading) // Don't do more than one requests at a time.
						return;
					this.world.loadState(this.id, this.priv).addCallback(
						dojo.hitch(this, '_updateState'));
				}

				//   Utils
				//   `````
				
				,_updateState: function(state) {
					this._reloading = false;
					if(!('revision' in state))
						throw 'No revision in state!';
					this.state = state;
					this.updateState(state, this); // for connected widgets

					this.revision = state.revision;
					//this.updated();
				}

				//   Overridables
				//	 ````````````

				,updateState: function(state) {
					// 1. override this to handle whole state loads
					// 2. connect() to this in browsers
					DBG(state);
				}

				//   Signals
				//   ```````
				// ,updated: nop
					 });

		/*
			All kinds of clients
			--- ----- -- -------

			This classes:
			  - check validity of messages
				- expose APIs to use data from their states and manipulate server-side 
				  objects
		*/

		dojo.declare('anxiety.PlayerList', anxiety.Client, {
				
				//   API
				//   ```
				constructor: function() { PL = this; },

				stat: function(id) {
					for(var i in this.players)
						if(this.players[i].id == id)
							return this.players[i];
				}

				//   Command handlers
				//   ``````` ````````

				,updateState: function(state) {
					this.chatLog = state.chatLog;
					// We own this object, if anyone else wants to use it, make copy!
					this.players = state.players;
					this.playersChanged();
				}
				
				,checkAddPlayer: function(m) {
					checkProps(m, 'id', 'name');
				}
					
				,handleAddPlayer: function(msg) {
					this.players.push(msg);
					this.playersChanged();
				}

				,checkRmPlayer: function(m) {
					checkProps(m, 'who');
				}
				
				,handleRmPlayer: function(msg) {
					// XXX
					
					this.playersChanged();
				}

				,handleChat: function(m) {
					this.heard(m);
				}

				/* Signals */
				, heard: nop, playersChanged: nop		});

		/* 
			 Player
			 ``````

		*/
		dojo.declare('anxiety.Player', anxiety.Client, {
				constructor: function(world, id, priv) {
					// Called after Client.constructor
					this.games = {};
				}
					
				,processMsg: function(m) {
					if(m.gameId) {
						var gm = this.games[m.gameId];
						if(!gm) {
							DBG('Got message for nonexistent game ' + m.gameId);
							return;
						}
						gm.processMsg(m, this);
					} else {
						this.inherited(arguments);
					}
				}

				//   APIs
				//	 ````
					
				,say: function(phrase) {
					return this.send('chat', {
							phrase: phrase
								});
				}

				,rm: function() {
					// Regardless of what server says, we're out of here
					var rv = this.send('iQuit');
					// In fact, we don't have to wait for server's response
					// Tell everyone
					this.loggedOut();
					this.world.loggedOut(this);
					// Clean up now
					this.close();
					// TODO Shouldn't we make sure signals don't get called after this
					return rv;
				}
				
				,invite: function(gameType, playerIds) {
					return this.send('invite', {
							gameType: gameType,
								target: playerIds.join(' ')
								});

				}

				,explainWhyWeCantStart: function(gameType, partnerIds) {
					var mod = anxiety.games[gameType];
					if(!mod) 
						return 'Game "' + gameType + '" is not supported!';
					return mod.explainWhyWeCantStart(partnerIds);
				}
					

				,startGame: function(gameType, partnerIds) {
					/** Returns a Deferred for caller to know when operation is over
					 */
					var excuse = this.explainWhyWeCantStart(gameType, partnerIds);
					if(excuse) {
						var er = {msg: excuse};
						this.error(er);
						return (new dojo.Deferred()).errback(er);
					} else {
						return this.send('startGame', {
								gameType: gameType,
									'with': partnerIds.join(' ')
									});
					}
				}

				//   Utils
				//   `````

				,send: function(what, whatElse, okHandler) {
					whatElse = whatElse || {}; 
					whatElse.who = this.id;
					whatElse.priv = this.priv;
					return this.world.send(what, whatElse, okHandler);
				}

				//   Client message handlers
				//   `````` ``````` ````````

				,handleInvited: function(m) {
					checkProps(m, 'who', 'gameType');
					/*
					this.world.players.addCallback(dojo.hitch(this, function(ps) {
					m.senderStat = ps.stat(m.who);
					*/
					m.senderStat = this.world.players.stat(m.who);
					if(!m.senderStat) 
						throw 'Player #' + m.who + ' not found';
					//this.invited(m);
				}

				,checkUninvited: function(m) {
					checkProps(m, 'who', 'gameType');
				}

				,handleAddGame: function(m) {
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
					this.gameStarted(game);
				}

				,updateState: function(s) {
					/* Update invitations */
					this.invitations = [];
					for(var i in s.invitations) {
						var inv = s.invitations[i];
						inv.senderStat = this.world.players.stat(inv.who);
						if(!inv.senderStat)
							continue; // The player who sent this has gone.
						this.invitations.push(inv);
					}
					
					/* Update games */



				}

				/* Signals */
				,invited: nop // this goes only from world
					 ,gameStarted: nop // this too
					 ,loggedOut: nop // world can log you out
					 ,error: nop	
					 });

		/*
			Servers
			-------

			Base for Game and World classes.
		*/
		dojo.declare('anxiety.Server', null, {
				constructor: function(urlPrefix) {
					this.urlPrefix = urlPrefix;
				}

				,send: function(what, whatElse, okHandler) {
					whatElse = whatElse || {};
					whatElse.what = what;
					DBG('Posting to ' + this.urlPrefix + ':');
					DBG(whatElse);
					return dojo.xhrPost({url: this.urlPrefix,
								content: whatElse, 
								handleAs: 'json', // OK (2**) answer will be handled as JSON.
								load: okHandler && dojo.hitch(this, okHandler),
								error: dojo.hitch(this, function(er) {
										// Try to get some information we can use
										if(er.responseText) {
											if('{' == er.responseText.charAt(0)) {
												var data = dojo.fromJson(er.responseText);
												er = data.err;
											}	else
												er.msg = er.responseText;
										} else 
											er.msg = er + '';
										this.error(er);
									})
								/*  It seems when load: function throws an exception,
										error: is called. That's not what I want though.
								*/
								});
				}
				
				,error: nop	});

		/*
			World object
			````` ``````
		*/
		
		dojo.declare('anxiety.World', anxiety.Server, {

				//   APIs
				//   ````

				constructor: function(urlPrefix) {
					WD = this; // debug
					// Do handshakes and stuff
					dojox.cometd.init(urlPrefix + 'cometd');
					this.playersFetch = this.getClient(anxiety.PlayerList, 'players');/*.
						addCallback(dojo.hitch(this, function(pl) {
									this.players = pl;
									}));*/
				}
				
				,login: function(name) {
					/**	Initiates a login request. 
							If successful, calls this.loggedIn(player), where player
							is newly created Player instance.
							If not, calls this.denied(rsp).
							Returns a Deferred.
					*/
					var pwd = dojo.cookie('p.' + name), id = dojo.cookie('i.' + name);
					if(pwd && id) {
						var rv = this.getClient(anxiety.Player, id, pwd);
					} else {
						var rv = this.send('needClient', {name: name}).
							addCallback(dojo.hitch(this, function(rsp) {
									// this way it gets passed to the next callback
									return this.getClient(anxiety.Player, rsp.id, rsp.priv);
									})).
							addCallback(function(c) {
									c.name = name;
									dojo.cookie('p.' + name, rsp.priv);
									dojo.cookie('i.' + name, rsp.id);
								});
						
					}
					rv.addCallback(dojo.hitch(this, 'loggedIn')); // probably not needed
					return rv;
				}

				,loadState: function(id, priv) {
					return dojo.xhrGet({
							url: this.urlPrefix + 'clients/' + id,
								handleAs: 'json',
								preventCache: true,
								error: dojo.hitch(this, function(e) {
										DBG('Failed reloading client ' + id + ':');
										DBG(e);
									}),
								content: {
								priv: priv
									}//,
							//load: dojo.hitch(this, '_updateState')
						});
				}

				,getClient: function(_Class, id, priv) {
					var rv = 	this.loadState(id, priv).addCallback(
						dojo.hitch(this, function(state) {
								var client = new _Class(this, id, priv);
								client._updateState(state);
								return client;
							}));
					return rv;
				}				
			
				
				/* Signals */
				,loggedIn: nop // maybe get rid of this because it clearly
					 // should be a Deferred
					 ,loggedOut: nop // this is rather a Player signal
					 ,heard: nop 
					 });

		/*  
				Games
				-----
				
				Each game type is a module named anxiety.games[gameType]
		*/
		dojo.declare('anxiety.Game', anxiety.MsgDriven, {
				constructor: function(state) {
					this.id = state.id;
					this.gameType = state.gameType;
				}

			});

		dojo.declare('anxiety.GameBrowser', [dijit.layout.ContentPane], {
				constructor: function(opts) {

				}
				
			});

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



