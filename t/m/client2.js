/*
	Anxiety
	-------

	This is a client for a multiplayer card game using Dojo framework.

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
dojo.require('dijit.Toolbar');

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

			This class runs a sync mechanism with server.
		*/
		dojo.declare('anxiety.Client', null, {
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

							if(msg.revision < this.revision) {
								// Our rev is older, this is a message from past
								return;
							}

							if(msg.revision == this.revision) {
								// Looks like something we can handle
								if(!msg.what) {
									DBG('No selector field:' + msg);
									return;
								}
								
								var methodName = 'handle' + cfirst(msg.what);
								if(this[methodName]) {
									// XXX game plugging code here
									this[methodName](msg);
									this.revision++;
								} else {
									DBG('No handling method for ' + msg.what);
								}
								return;
							}

							if(msg.revision > this.revision) {
								// Got message from the future, must go there
								this.reload();
							}
						});
					this.reload();

				}
					
				,reload: function() {
					/**  Fetch and update the whole client state.
					 */ 
					if(this._reloading) // Don't do more than one requests at a time.
						return;
					// XXX handle error
					dojo.xhrGet({
							url: this.urlPrefix + 'clients/' + this.id,
								handleAs: 'json',
								// error:
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
				}
					
				
				//   Overridables
				//	 ````````````

				,updateState: function(state) {
					DBG(state);
				}
					
			});

		/*
			All kinds of clients
			--- ----- -- -------
			
		*/

		dojo.declare('anxiety.PlayerList', anxiety.Client, {
				stat: function(id) {
					for(var i in this.players)
						if(this.players[i].id == id)
							return this.players[i];
				}

				//   Command handlers
				//   ``````` ````````
				,updateState: function(state) {
					this.players = state.players;
					this.playersChanged();
				}
					
				,handleAddPlayer: function(msg) {
					this.players.push(msg);
					this.playersChanged();
				}
				
				,handleRmPlayer: function(msg) {
					// XXX
					
					this.playersChanged();
				}

				,handleChat: function(m) {
					this.heard(m);
				}

				, heard: nop, playersChanged: nop
			});

		
		dojo.declare('anxiety.Player', anxiety.Client, {
				constructor: function(world, id, priv) {
					// Called after Client.constructor
					this.games = {};
				}
					
				//   APIs
				//	 ````
					
				,say: function(phrase) {
					return this.send('chat', {
							phrase: phrase
								});
				}

				,rm: function() {
					return this.send('iQuit');
				}

				,invite: function(kindOfGame, playerIds) {
					return this.send('invite', {

						});

				}

				,startGame: function(partnerIds) {
					return this.send('startGame', {


						});
				}

				//   Utils
				//   `````

				,send: function(what, whatElse, okHandler) {
					whatElse = whatElse || {}; 
					whatElse.who = this.id;
					whatElse.priv = this.priv;
					return this.world.send(what, whatElse, okHandler);
				}

				/* Signals */
				, invited: nop, gameStarted: nop
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
					return dojo.xhrPost({url: this.urlPrefix,
								content: whatElse, 
								handleAs: 'json', // OK (2**) answer will be handled as JSON.
								load: dojo.hitch(this, okHandler),
								error: dojo.hitch(this, function(er) {
										// Try to get some information we can use
										if(er.responseText) {
											if('{' == er.responseText.charAt(0))
												er = dojo.fromJson(er);
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
			Browser widgets
			------- -------

			ChatBrowser
			```````````
			A window with chat messages in it. We see it in WorldBrowser, until we log in,
			and then we see it in a PlayerBrowser as a part of a more complex setup.
		*/
		dojo.declare('anxiety.ChatBrowser', [dijit.layout.ContentPane], {   
				constructor: function(opts) {
					this.world = opts.world;
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
																							 '<b>' + author.name + '</b>: ' + m.phrase}, 
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
		dojo.declare('anxiety.WorldBrowser', [dijit._Widget, dijit._Templated],	 {   
				templateString: null,
					/* templatePath must be in the same domain for now */
					templatePath: '/templates/worldbrowser.xml',
					widgetsInTemplate: true,
										 
					constructor: function(opts) {
					this.world = opts.world;
										 
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
												 // m.id is identifier of the removed player
												 var itm = this.playerStore._getItemByIdentity(m.id);
												 this.playerStore.deleteItem(itm);
											 });
					/* This thing is very important! */
					this.mainLayout.resize();
					/* inb4 worthless comment )) */
				}


				//   Utility methods
				//   ``````` ```````
											 
				,moveIntoClientWindow: function(wdg) {
					// TODO remove whatever is in the client window now
					wdg.attr('region', 'center');
					this.mainLayout.addChild(wdg);
				}

				,report: function(str, deferred) {
					this.statusBarNode.innerHTML = str;
					dojo.style(this.statusBarNode, {visibility: str ? 'visible' : 'hidden'});
					// Clear after done.
					return deferred && deferred.addBoth(dojo.hitch(this, 'report', '', 0));
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
										 

				//   World & playerList event handlers  
				//   ````` ` `````````` ````` ````````  
											 
				,updatePlayers: function(state) {
					// Update the whole store with state.players
					this.playerStore = new dojo.data.ItemFileWriteStore({
							data: {
								identifer: 'id',
								label: 'name',
								items : state.players
							}
						});
					this.playerListView.setStore(this.playerStore);
				}																	
									 
				,loggedIn: function(player) {
					this.report(); // clear status message
					this.chatBrowser.destroy();
					delete this.chatBrowser;

					this.playerBrowser = new anxiety.PlayerBrowser({player: player});
					this.moveIntoClientWindow(this.playerBrowser);
					this.logoutBtn.attr('disabled', false);
				}

				,loggedOut: function(player) {
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
		dojo.declare('anxiety.PlayerBrowser', [dijit.layout.BorderContainer],	{   
				/*templateString: null,
					templatePath: '/templates/playerbrowser.xml',
					widgetsInTemplate: true,*/
				constructor: function(opts) {
					this.player = opts.player;
					this.attr('gutters', true);
				},
					
					postCreate: function() {
					this.inherited(arguments);

					this.talkBar = new dijit.layout.ContentPane({region: 'bottom'});
					this.phraseBox = (new dijit.form.TextBox()).placeAt(this.talkBar.domNode);
					this.sayBtn = (new dijit.form.Button({
								label: 'Send',
								onClick: dojo.hitch(this, 'sayThings')
							})).
						placeAt(this.talkBar.domNode);
					this.addChild(this.talkBar);
					
					/* Make tabs. */
					this.tabs = new dijit.layout.TabContainer({region: 'center'});
					this.addChild(this.tabs);
					
					/* Set up our own chat browser */
					this.chat = new anxiety.ChatBrowser({
							title: 'Invitations & chat',
							world: this.player.world
											 });
					this.tabs.addChild(this.chat);
				}

				,sayThings: function() {
					var phrase = this.phraseBox.attr('value');
					if(phrase)
						/*this.report('Sending...', */this.player.say(phrase)/*)*/;
				}
				
			});

		
	});



