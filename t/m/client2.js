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

function browse(urlPrefix, l) {
	dojo.addOnLoad(function() {
			var w = new anxiety.World(urlPrefix);
			var br = new anxiety.WorldBrowser({world: w}, l);
			//br.startup();
			DBG('Before require...');
			dojo.require('dijit.layout.TabContainer');
			DBG('After require');
		});
}

function checkProps(o /*, ...*/) {
	for(var i = arguments.length - 1; i > 0; i--) 
		if(!o[arguments[i]])
			throw 'Property ' + arguments[i] + ' expected';
}

/*	Imports
		-------
*/
dojo.require('dojo.data.ItemFileWriteStore');
dojo.require('dojo.cookie');
// TODO We sure need to use cookies to relogin,
// but in the same time I want to allow multiple sessions in 
// different browser windows/tabs

dojo.require('dijit._Widget');
//dojo.require('dijit._Templated');
dojo.require('dijit.layout.BorderContainer');
dojo.require('dijit.layout.ContentPane');
dojo.require('dijit.form.CheckBox');
//dojo.require('dijit.form.TextBox');
//dojo.require('dijit.Toolbar');

dojo.require('dojox.grid.DataGrid');
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
				}					

				,connectToWidget: function(widget, methodName) {
					return widget.connect(this, methodName, methodName);
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
					this.invited(m);
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
				

		/*  
				Browser widgets
				------- -------
				
		*/
			
		dojo.declare('anxiety.Reporting', null, {
				report: function(str, deferred) {
					if(!this.statusBarNode) {
						this.statusBarNode = dojo.create('SPAN', {
								className: 'statusBar'
							}, this.domNode);
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
						dojo.style(this.simpleStatusBarNode, {visibility: str ? 'visible' : 'hidden'});
					}
				}
			});

		/*
			ChatBrowser
			```````````
			A window with chat messages in it. We see it in WorldBrowser, until we log in,
			and then we see it in a PlayerBrowser as a part of a more complex setup.
		*/
		dojo.declare('anxiety.ChatBrowser', [dijit.layout.ContentPane], {   
				constructor: function(opts) {
					this.maxPhraseCnt = 50;
				}
										 
				// baseClass: 'chatBrowser',
										 
				,postCreate: function() {
					/** This is called after constructor and buildRendering.
							this.connect machinery was not initialized until now.
					*/
					this.inherited(arguments);
					this.players.passStateToWidget(this);
					this.players.connectToWidget(this, 'handleHeard')
				}

				,update: function() {
					this.domNode.innerHTML = '';
					for(var i in this.players.chatLog) {
						this.handleHeard(this.players.chatLog[i]);
					}
				}

				
				,handleHeard: function(m) {
					// Someone said something!
					var author = this.players.stat(m.author);
					if(author) {
						var nameBody = '<b>' + author.name + '</b>';
					} else {
						// Player's gone
						var nameBody = '*<b>' + m.author + '</b>*';
					}

					var phraseNode = dojo.create('DIV', 
						{innerHTML: nameBody + ': ' + m.phrase}, 
						this.domNode);
					// Don't display more then maxPhraseCnt phrases.
					var nodes = dojo.query('DIV', this.domNode);
					if(nodes.length > this.maxPhraseCnt) 
						nodes.slice(0, nodes.length - this.maxPhraseCnt).orphan();
					
					// TODO Scroll
				}
			});


		// To keep line short
		var wbBase = [dijit.layout.BorderContainer, anxiety.Reporting];
		dojo.declare('anxiety.ListBrowser', wbBase, {
				constructor: function() {
					// opts: players
				}
				
				,postCreate: function() {
					DBG('Postcreating ListBrowser...');

					// init BorderContainer
					this.attr({
							splitter: true,
								region: 'trailing',
								style: {
								width: '250px'
								  }
						});
					
					// Layout
					this.playerListView = new dojox.grid.DataGrid({
							structure: {
								layout: [{
										field: 'cb',
										name: '',
										width: '10%'
									}, {
										field: 'name',
										name: 'Name',
										width: '90%'
									}]}});
									
					this.addChild(this.playerListView);
					this.playerListView.startup();
					this.toolbar = new dijit.layout.ContentPane({
							region: 'bottom'
						});
					this.addChild(this.toolbar);
							
					this.connect(this.players, 'handleAddPlayer', 
						function(m) {
							this.playerStore.newItem(m);
						});

					this.players.passStateToWidget(this);

					this.connect(this.players, 'handleRmPlayer', 
						function(m) {
							// m.who is identifier of the removed player
							this.playerStore.fetch({
									query: {id: m.who},
										scope: this.playerStore,
										onItem: function(it) {
										this.deleteItem(it)
											}
								});
						});
				}

				,update: function(state) {
					// Update the whole store with state.players
					// state.players array is used by Player class, so we make copy of it
					var items = dojo.clone(state.players);
					DBG('Initializing a store with following items:');
					DBG(items);
					this.playerStore = new dojo.data.ItemFileWriteStore({
							data: {
								identifer: 'id',
								label: 'name',
								items : items
							}
						});
					DBG('Calling setStore');
					this.playerListView.setStore(this.playerStore);
				}
			});
									 
		/*
			WorldBrowser
			````````````
		*/
		// declare messes up 'bases' argument
		var wbBase = [dijit.layout.BorderContainer, anxiety.Reporting];
		dojo.declare('anxiety.WorldBrowser', wbBase, {  
				constructor: function() {
					DBG("Constructing WorldBrowser...");
					WB = this;
				}

				,postCreate: function() {
					DBG('postcreating WorldBrowser...');
					this.inherited(arguments);

					// Layout

					this.mainLayout = new dijit.layout.BorderContainer({
							style: {
								width: '100%',
								height: '100%'
							},
							design: 'sidebar',
							gutters: true,
							liveSplitters: true,
							className: 'worldBrowser'
						});
					this.mainLayout.placeAt(this.domNode);

					// Login pane
					var loginPane = new dijit.layout.ContentPane({
							region: 'top',
							className: 'loginPane'
						});
					this.mainLayout.addChild(loginPane);
					this.statusBarNode = dojo.create('DIV', 
						{className: 'statusBar'},
						loginPane.domNode);
					this.report('Fetching user list...', this.world.playersFetch);
					this.usernameBox = dojo.create('INPUT', {
							type: 'text',
						}, loginPane.domNode);
					this.connect(this.usernameBox, 'onkeydown', 
						function(ev) {
							if(13 == ev.keyCode)  // ENTER
								return this.loginBtnPressed();
						});
					this.loginBtn = mkBtn({
							label: 'Log in'
						}, loginPane.domNode);
					this.connect(this.loginBtn, 'onClick', 'loginBtnPressed');
					this.logoutBtn = mkBtn({
							label: "Log out"
						}, loginPane.domNode);
					this.connect(this.logoutBtn, 'onClick', function() {
							this.report('Logging out...', this.playerBrowser.player.rm());
						});

					// 
					this.world.playersFetch.addCallback(dojo.hitch(this, function(ps) {
								DBG('PlayerList object arrived:');
								DBG(ps);
								var listBrowser = new anxiety.ListBrowser({
										players: ps
									});
								listBrowser.attr('region', 'trailing');
								this.mainLayout.addChild(listBrowser);
							}));


					this.loggedOut();

					this.connect(this.world, 'loggedIn',  'loggedIn');
					this.connect(this.world, 'loggedOut', 'loggedOut');
					/* FIXME Why do i always need to say it twice? */
					this.connect(this.world, 'error', function(er) {
							alert(er.msg);
						});

					this.mainLayout.resize(); // Layout doesn't break with this
				}

				//   API
				//   ```

				,getSelectedPlayerIds: function() {
					var rv = [], items = this.playerListView.selection.getSelected();
					for(var i in items) 
						rv.push(items[i].id);
					return rv;
				}
				

				//   World & playerList event handlers  
				//   ````` ` `````````` ````` ````````  
											 
				,loggedIn: function(player) {
					this.chatBrowser.destroy();
					delete this.chatBrowser;

					this.playerBrowser = new anxiety.PlayerBrowser({
							player: player, 
							worldBrowser: this});
					this.moveIntoClientWindow(this.playerBrowser);
					dojo.attr(this.logoutBtn, 'disabled', false);
					dojo.attr(this.usernameBox, 'title', 'Id is ' + player.id);
				}

				,loggedOut: function(player) {
					if(this.playerBrowser) {
						this.playerBrowser.destroy();
						delete this.playerBrowser;
					}

					dojo.attr(this.loginBtn, 'disabled', false);
					dojo.attr(this.logoutBtn, 'disabled', true);
					dojo.attr(this.usernameBox, 'disabled', false);

					if(!this.chatBrowser) {
						this.world.playersFetch.addCallback(dojo.hitch(this, function(ps) {
									// Setup a chat message view.
									this.chatBrowser = new anxiety.ChatBrowser({
											players: ps
										});
									this.moveIntoClientWindow(this.chatBrowser);
								}));
					}
				}

				//   Utils
				//   `````
											 
				,moveIntoClientWindow: function(wdg) {
					// TODO remove whatever is in the client window now
					wdg.attr('region', 'center');
					this.mainLayout.addChild(wdg);
				}

				,loginBtnPressed:	function() {
					var loginName = dojo.attr(this.usernameBox, 'value');
					if(!loginName.length) {
						alert('Enter name to log in');
						return;
					}
					
					dojo.attr(this.loginBtn, 'disabled', true);
					dojo.attr(this.usernameBox, 'disabled', true);
					this.report('Logging in...', this.world.login(loginName)).
						addErrback(dojo.hitch(this, 'loggedOut'));
				}


			});
		
		/*
			PlayerBrowser
			`````````````
			On design of this thing, it would be nice to have a chat eveywhere. 
			So we'll keep a send bar always typeable at the bottom,
			and I'm planning to have some notifications on messages that will be visible 
			whereever in the interface you are.
			Upside, there should be tabs for invitations and games.
			
		*/			
		dojo.declare('anxiety.PlayerBrowser', 
			[dijit.layout.BorderContainer, anxiety.Reporting],	
			{   
				constructor: function(opts) {
					this.invs = {};
					this.inviteBtns = [];
					this.gameViews = {};

					/* Setup BorderContainer */
					this.attr('gutters', true);
				}
					
				,destroy: function() {
					this.inherited(arguments);
					for(var i in this.inviteBtns) {
						this.inviteBtns[i].destroy();
					}
				}

				,postCreate: function() {
					this.inherited(arguments);

					/* Make the 'Invite' buttons */
					for(var gameType in anxiety.games) {
						var gmMod = anxiety.games[gameType];
						var btn = mkBtn({
								label: 'Invite to play "' + gameType + '"'
							});
						dojo.placeAt(btn, this.worldBrowser.toolbar.domNode);
						this.connect(btn, 'onClick', 'invBtnPressed');
						this.inviteBtns.push(btn);
					}
							
					/* Make a status bar */
					dojo.style(this.domNode, {position: 'relative'});
					this.statusBarNode = dojo.create('DIV', 
						{className: 'statusBar'}, 
						this.domNode);

					/* Make the always visible 'say' box */
					this.talkBar = new dijit.layout.ContentPane({region: 'bottom'});
					this.phraseBox = (new dijit.form.TextBox()).placeAt(this.talkBar.domNode);
					this.connect(this.phraseBox, 'onKeyDown', 'sayThings');

					this.sayBtn = mkBtn({
								label: 'Say'
						});
					dojo.placeAt(this.sayBtn, this.talkBar.domNode);
					this.connect(this.sayBtn, 'onClick', 'sayThings');
					this.addChild(this.talkBar);
					
					/* Make tabs. */
					this.tabs = new dijit.layout.TabContainer({region: 'center'});
					this.addChild(this.tabs);
					
					/* Set up our own chat browser */
					this.chat = new anxiety.ChatBrowser({
							title: 'Invitations & chat',
							region: 'center',
							world: this.player.world
						});
					this.invAndChatLo = new dijit.layout.BorderContainer({
							title: 'Invitations & chat'
						});
					this.invPane = new dijit.layout.ContentPane({
							title: 'Invitations',
							region: 'leading',
							splitter: true,
							style: {
								width: '25%'
							}
						});
					this.invAndChatLo.addChild(this.invPane);
					this.invAndChatLo.addChild(this.chat);
					this.tabs.addChild(this.invAndChatLo);

					this.connect(this.player, 'updated', 'updateState');
					this.connect(this.player, 'invited', 'addInv');
					this.connect(this.player, 'handleUninvited', 'rmInv');
					this.connect(this.player, 'gameStarted', 'updateGame');

				}

				,updateState: function() {
					/* Redraw invitations */
					dojo.empty(this.invPane.containerNode);
					this.invViews = {};

					for(var i in this.player.invitations) {
						this.addInv(this.player.invitations[i]);
					}

					/* Update games */
					for(var whatever in this.player.games) {
						this.updateGame(this.player.games[whatever]);
					}
					/* Remove browsers for games which aren't there */
					for(var id in this.gameViews)
						if(!(id in this.player.games))
							this.removeGameView(id);
				}

				,removeGameView: function(id) {


				}


				,updateGame: function(gm) {
					if(!this.gameViews[gm.id]) {
						/* Make a game browser */
						var wdg = gm.mod.browse(gm);
						wdg.attr({title: gm.id});
						this.tabs.addChild(wdg);
						this.gameViews[gm.id] = wdg;
					} else 
						this.gameViews[gm.id].browse(gm);
				}

				,addInv: function(inv) {
					/** Displays a checkbox with a name of invitation sender in a specific 
							gameType section. If section doesn't exist, it is created
					*/
					if(!this.invViews[inv.gameType]) {
						var node = dojo.create('DIV', {
								className: 'gameType'
							}, this.invPane.containerNode);
						var listNode = dojo.create('DIV', {
								className: 'list'
							}, node);
						//var startBtnNode = dojo.create('BUTTON', {
						var startBtn = mkBtn({
								label: 'Start "' + inv.gameType + '" game'
							});
						dojo.placeAt(startBtn, node);
						
						// Hack
						startBtn.anxiety = {
							playerBrowser: this,
							gameType: inv.gameType
						};
						startBtn.onClick = function() {
							var pb = this.anxiety.playerBrowser, gameType = this.anxiety.gameType;
							// Get selected ids
							var view = pb.invViews[gameType], ids = [];
							for(var i in view.entries) {
								var en = view.entries[i];
								if(en.cbNode.checked) 
									ids.push(en.inviterId);
							}
							if(ids.length) {
								dojo.attr(this, 'disabled', true);
								pb.report('Starting game...', pb.player.startGame(gameType, ids))
								.addBoth(dojo.hitch(this, function() {
											dojo.attr(this, 'disabled', false);
										}));
								// XXX 'uninvited' message could come first and delete a button
								// so there will be nothing to enable
							} else 
								DBG('startGame: no partners selected!');
						};
								
						
						this.invViews[inv.gameType] = {
							node: node,
							listNode: listNode,
							startBtn: startBtn,
							entries: []
						};
					}

					var typeView = this.invViews[inv.gameType];
					var entry = {
						node: dojo.create('DIV', {
								className: 'entry'
							}, typeView.listNode),
						inviterId: inv.who
					};

					entry.cbNode = dojo.create('INPUT', {
							type: 'checkbox',
							name: inv.who
						}, entry.node);
					entry.labelNode = dojo.create('LABEL', {
							innerHTML: inv.senderStat.name
						}, entry.node);
					typeView.entries.push(entry);
				}
					
				,rmInv: function(inv) {
					var section = this.invViews[inv.gameType];
					if(section) {
						for(var i in section.entries) {
							var en = section.entries[i];
							if(en.inviterId == inv.who) {
								dojo.destroy(en.node);
								section.entries.splice(i, 1);
								if(!section.entries.length) { 
									DBG('Removing empty section');
									delete this.invViews[inv.gameType];
									dojo.destroy(section.node);
								}
								return;
							}
						}
						DBG('rmInv: found nothing from ' + inv.who);
					} else {
						DBG('rmInv: no invitations for ' + inv.gameType);
					}
				}

				,sayThings: function(ev) {
					if(13 != ev.keyCode) 
						if('click' != ev.type)
							return;

					var phrase = this.phraseBox.attr('value');
					if(phrase)
						this.report('Sending...', this.player.say(phrase));
					this.phraseBox.attr('value', '');
				}
				
				,invBtnPressed: function(evt) {
					// XXX which button?
					var ids = this.worldBrowser.getSelectedPlayerIds();
					this.player.invite('thousand', ids);
				}
			});

		
	});



