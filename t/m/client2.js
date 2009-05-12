/*
	Anxiety
	-------

	This is a client for a multiplayer card game using Dojo framework.

	TODO
	 - don't load "Tabs" module until we log in 
	 - make "Player" and "PlayerBrowser" separate modules and load them when we log in
	 - get games to their own js files an don't load them until we use them

	IDEAS
 	 - maybe we'd better use regular <button>s to save cycles
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
			br.startup();
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

dojo.require('dijit._Widget');
dojo.require('dijit._Templated');
dojo.require('dijit.layout.BorderContainer');
dojo.require('dijit.layout.TabContainer');
dojo.require('dijit.layout.ContentPane');
dojo.require('dijit.form.Button');
dojo.require('dijit.form.CheckBox');
dojo.require('dijit.form.TextBox');
//dojo.require('dijit.Toolbar');

dojo.require('dojox.grid.DataGrid');
dojo.require('dojox.cometd');

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
					}
				}
			});

		/*
			This class runs a sync mechanism with server.
		*/
		dojo.declare('anxiety.Client', anxiety.MsgDriven, {
				constructor: function(world, id, priv) {
					this.id = id;
					this.priv = priv;
					this.world = world;
					this.urlPrefix = world.urlPrefix;
					this.revision = -1;

					// Subscribe to events.
					// TODO Authenticate.
					dojox.cometd.subscribe('/' + id, this, function(fullMsg) {
							DBG(fullMsg);
							var msg = fullMsg.data;
							// processMessage
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
							// If msg.revision < this.revision , then
							// Our rev is older, this is a message from past
							// so we should do nothing
						});

					this.reload();
				}
					
				,reload: function() {
					/**  Fetch and update the whole client state.
					 */ 
					if(this._reloading) // Don't do more than one requests at a time.
						return;
					dojo.xhrGet({
							url: this.urlPrefix + 'clients/' + this.id,
								handleAs: 'json',
								error: dojo.hitch(this, function(e) {
										DBG('Failed reloading client ' + this.id + ':');
										DBG(e);
									}),
								content: {
								priv: this.priv
									},
								load: dojo.hitch(this, '_updateState')
								});
						this._updating = true;
				}

				,_updateState: function(state) {
					this._reloading = false;
					if(!('revision' in state))
						throw 'No revision in state!';
					this.revision = state.revision;
					this.updateState(state);
					this.updated();
				}

				,close: function() {
					return dojox.cometd.unsubscribe('/' + this.id);
				}
				
				//   Overridables
				//	 ````````````

				,updateState: function(state) {
					DBG(state);
				}
				
				,updated: nop
					 });

		/*
			All kinds of clients
			--- ----- -- -------

			This classes:
			  - check validity of messages
				- expose APIs to use data from their states and manipulate server-side objects
		*/

		dojo.declare('anxiety.PlayerList', anxiety.Client, {
				
				//   API
				//   ```
				stat: function(id) {
					for(var i in this.players)
						if(this.players[i].id == id)
							return this.players[i];
				}

				//   Command handlers
				//   ``````` ````````
				,updateState: function(state) {
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
					var excuse = this.explainWhyWeCantStart(gameType, partnerIds);
					if(excuse) {
						this.error({msg: excuse});
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
					m.senderStat = this.world.players.stat(m.who);
					if(!m.senderStat) 
						throw 'Player #' + m.who + ' not found';
					this.invited(m);
				}

				,handleAddGame: function(m) {
					checkProps(m, 'gameType', 'id');
					// Construct an appropriate game object.
					var mod = anxiety.games[m.gameType];
					if(!mod) 
						throw 'Game "' + m.gameType + '" is not supported';
					checkProps(mod, 'mkGame');
					var game = mod.mkGame(m);
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
				, invited: nop, gameStarted: nop, loggedOut: nop	});

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
					return dojo.xhrPost({url: this.urlPrefix,
								content: whatElse, 
								handleAs: 'json', // OK (2**) answer will be handled as JSON.
								load: dojo.hitch(this, okHandler),
								error: dojo.hitch(this, function(er) {
										// Try to get some information we can use
										if(er.responseText) {
											if('{' == er.responseText.charAt(0))
												er = dojo.fromJson(er.responseText);
											else
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
				constructor: function(urlPrefix) {
					// Do handshakes and stuff
					dojox.cometd.init(urlPrefix + 'cometd');
					this.players = new anxiety.PlayerList(this, 'players');
				}
				

				//   APIs
				//   ````
				
				,login: function(name) {
					/**	Initiates a login request. 
							If successful, calls this.loggedIn(player), where player
							is newly created Player instance.
							If not, calls this.denied(rsp).
							Returns a Deferred.
					*/
					return this.send('needClient', {name: name}, function(rsp) {
							var p = new anxiety.Player(this, rsp.id, rsp.priv);
							this.loggedIn(p);
						});
				}
			
				
				/* Signals */
				,loggedIn: nop, loggedOut: nop,  heard: nop });

		/*  
				Games
				-----
				
				Each game type is a module named anxiety.games[gameType]
		*/
		dojo.declare('anxiety.Game', anxiety.MsgDriven, {
				constructor: function(s) {

				}

			});

		dojo.declare('anxiety.GameBrowser', [dijit.layout.ContentPane], {
				constructor: function(opts) {

				}
				
			});


		/*  
			  Thousand
				--------

				TODO Make a module
		*/
		dojo.declare('anxiety.games.thousand.GameFor3', anxiety.Game, {
				handleCards: function(m) {

				}
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

				,browse: function(game) {
					return new anxiety.games.thousand.BrowserFor3({game: game});
				}
			});
			
		dojo.declare('anxiety.games.thousand.BrowserFor3', anxiety.GameBrowser, {
				postCreate: function() {
					browse(this.game);
				}

				,browse: function(game) {
					if(this.game) {
						// TODO Cleanup connections.
					}
					
					this.game = game;
				}
			});
				

		/*  
				Browser widgets
				------- -------
				
		*/
			
		dojo.declare('anxiety.Reporting', null, {
				report: function(str, deferred) {
					this.statusBarNode.innerHTML = str;
					dojo.style(this.statusBarNode, {visibility: str ? 'visible' : 'hidden'});
					// Clear after done.
					return deferred && deferred.addBoth(dojo.hitch(this, 'report', '', 0));
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
					//this.world = opts.world;
					this.maxPhraseCnt = 50;
				}
										 
				// baseClass: 'chatBrowser',
										 
				,postCreate: function() {
					/** This is called after constructor and buildRendering.
							this.connect machinery was not initialized until now.
					*/
					this.connect(this.world.players, 'heard', 'heard');
					this.inherited(arguments);
				}

				
				,heard: function(m) {
					// Someone said something!
					var author = this.world.players.stat(m.author);
					if(!author)
						return DBG('Got a chat message from nonexistent player ' + m.author);

					var phraseNode = dojo.create('DIV', {innerHTML: 
																							 '<b>' + author.name + 
																							 '</b>: ' + m.phrase}, 
						this.domNode);
					// Don't display more then maxPhraseCnt phrases.
					var nodes = dojo.query('DIV', this.domNode);
					if(nodes.length > this.maxPhraseCnt) 
						nodes.slice(0, nodes.length - this.maxPhraseCnt).orphan();
				}
			});


		/*
			WorldBrowser
			````````````
			A widget that shows us client list, allows to login and spawns a PlayerBrowser
			when we do.
		*/
		// To keep line short
		var wbBase = [dijit._Widget, dijit._Templated, anxiety.Reporting];
		dojo.declare('anxiety.WorldBrowser', wbBase, {  
				//   API
				//   ```
				
				getSelectedPlayerIds: function() {
					var rv = [], items = this.playerListView.selection.getSelected();
					for(var i in items) 
						rv.push(items[i].id);
					return rv;
				}
				
				,postCreate: function() {
					this.inherited(arguments);
					this.loggedOut();

					this.connect(this.world, 'loggedIn',  'loggedIn');
					this.connect(this.world, 'loggedOut', 'loggedOut');
					/* FIXME Why do i always need to say it twice? */
					this.connect(this.world, 'error', function(er) {
							alert(er.msg);
						});

					/* Put in current values. */
					this.updatePlayers(this.world.players);

					/*   Connect PlayerList client command handlers to a datastore
					 */
					this.connect(this.world.players, 'updateState', 'updatePlayers');
					this.connect(this.world.players, 'handleAddPlayer', 
											 function(m) {
												 this.playerStore.newItem(m);
											 });
					this.connect(this.world.players, 'handleRmPlayer', 
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
					/* This thing is very important! */
					this.mainLayout.resize();
					/* inb4 worthless comment )) */
				}


				//   World & playerList event handlers  
				//   ````` ` `````````` ````` ````````  
											 
				,updatePlayers: function(state) {
					// Update the whole store with state.players
					// state.players array is used by Player class, so we make copy of it
					this.playerStore = new dojo.data.ItemFileWriteStore({
							data: {
								identifer: 'id',
								label: 'name',
								items : dojo.clone(state.players)
							}
						});
					this.playerListView.setStore(this.playerStore);
				}																	
									 
				,loggedIn: function(player) {
					this.chatBrowser.destroy();
					delete this.chatBrowser;

					this.playerBrowser = new anxiety.PlayerBrowser({player: player, worldBrowser: this});
					this.moveIntoClientWindow(this.playerBrowser);
					this.logoutBtn.attr('disabled', false);
					this.usernameBox.attr('title', 'Id is ' + player.id);
				}

				,loggedOut: function(player) {
					if(this.playerBrowser) {
						this.playerBrowser.destroy();
						delete this.playerBrowser;
					}

					this.loginBtn.attr('disabled', false);
					this.usernameBox.attr('disabled', false);
					this.logoutBtn.attr('disabled', true);

					if(!this.chatBrowser) {
						// Setup a chat message view.
						this.chatBrowser = new anxiety.ChatBrowser({
								world: this.world
							});
						this.moveIntoClientWindow(this.chatBrowser);
					}
				}

				//   Utility methods
				//   ``````` ```````
											 
				,moveIntoClientWindow: function(wdg) {
					// TODO remove whatever is in the client window now
					wdg.attr('region', 'center');
					this.mainLayout.addChild(wdg);
				}

				
				//   UI event handlers
				//	 `` ````` ````````
										 
				,loginBtnPressed: function() {
					var loginName = this.usernameBox.attr('value');
					if(!loginName.length) {
						alert('Enter name to log in');
						return;
					}
										 
					this.loginBtn.attr('disabled', true);
					this.usernameBox.attr('disabled', true);
					this.report('Logging in...', this.world.login(loginName)).
						addErrback(dojo.hitch(this, 'loggedOut'));
				}

				,logoutBtnPressed: function() {
					this.report('Logging out...', this.playerBrowser.player.rm());
				}
										 

				/*  Variables  */
				,templateString: null
					 /* templatePath must be in the same domain for now */
					 ,templatePath: '/templates/worldbrowser.xml'
					 ,widgetsInTemplate: true
					 
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
		dojo.declare('anxiety.PlayerBrowser', [dijit.layout.BorderContainer, anxiety.Reporting],	{   
				/*templateString: null,
					templatePath: '/templates/playerbrowser.xml',
					widgetsInTemplate: true,*/
				constructor: function(opts) {
					/*this.player = opts.player;
						this.worldBrowser = opts.worldBrowser;*/
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
						var btn = new dijit.form.Button({
								label: 'Invite to play "' + gameType + '"'
							});
						btn.placeAt(this.worldBrowser.toolbar.domNode);
						this.connect(btn, 'onClick', 'invBtnPressed');
						this.inviteBtns.push(btn);
					}
							
					/* Make a status bar */
					dojo.style(this.domNode, {position: 'relative'});
					this.statusBarNode = dojo.create('DIV', {className: 'statusBar'}, 
																					 this.domNode);

					/* Make the always visible 'say' box */
					this.talkBar = new dijit.layout.ContentPane({region: 'bottom'});
					this.phraseBox = (new dijit.form.TextBox()).placeAt(this.talkBar.domNode);
					this.sayBtn = (new dijit.form.Button({
								label: 'Send',
								onClick: dojo.hitch(this, 'sayThings')
							})).placeAt(this.talkBar.domNode);
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
					if(!this.invViews[inv.gameType]) {
						var node = dojo.create('DIV', {
								className: 'gameType'
							}, this.invPane.containerNode);
						var listNode = dojo.create('DIV', {
								className: 'list'
							}, node);
						//var startBtnNode = dojo.create('BUTTON', {
						var startBtn = new dijit.form.Button({
								label: 'Start "' + inv.gameType + '" game'
							});
						startBtn.placeAt(node);
						
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
								this.attr('disabled', true);
								pb.report('Starting game...', pb.player.startGame(gameType, ids))
								.addBoth(dojo.hitch(this, function() {
											this.attr('disabled', false);
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


				}

				,sayThings: function() {
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



