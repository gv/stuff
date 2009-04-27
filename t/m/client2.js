/*
	This is a client for a multiplayer card game using Dojo
*/

/*
	Utils
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

/*
	Imports
	-------
*/
dojo.require('dijit._Widget');
dojo.require('dijit._Templated');
dojo.require('dijit.layout.BorderContainer');
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
					this.urlPrefix = world.urlPrefix;
					this.revision = -1;

					// Subscribe to events.
					// TODO Authenticate.
					dojox.cometd.subscribe('/' + id, this, function(msg) {
							// processMessage
							// any output we can't handle will be silently ignored
							if(!('revision' in msg)) {
								DBG('No revision number:' + msg.toSource());
								return;
							}

							if(msg.revision < this.revision) {
								// Our rev is older, this is a message from past
								return;
							}

							if(msg.revision == this.revision) {
								// Looks like something we can handle
								if(!msg.what) {
									DBG('No selector field:' + msg.toSource());
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

				},
					
					// fetch and update the whole client state
					reload: function() {
					// don't do second request
					if(this._updating)
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
				},

					_updateState: function(state) {
					this._updating = false;
					this.updateState(state);
				},
					
				
					/*  Overridables
							------------
					*/
					updateState: function(state) {
					DBG(state.toSource());
				}
					
			});

		/*
			All kinds of clients
			--- ----- -- -------
			
		*/

		dojo.declare('anxiety.PlayerList', anxiety.Client, {
				// connect(...) or subclass???
				playersChanged: nop,
					
					_updateState: function(state) {
					this.players = state.players;
					this.playersChanged();
				},
					
					handleAddPlayer: function(msg) {
					this.players.push(msg);
					this.playersChanged();
				},

					handleRmPlayer: function(msg) {
					// XXX
				
					this.playersChanged();
				}
			});

		
		dojo.declare('anxiety.Player', anxiety.Client, {
				constructor: function(world, id, priv) {
					// Called after Client.constructor
					this.games = {};
				}

			});

		/*
			World object
			````` ``````
		*/
				
		dojo.declare('anxiety.World', null, {
				constructor: function(urlPrefix) {
					this.urlPrefix = urlPrefix;

					// do handshakes and stuff
					dojox.cometd.init(urlPrefix + 'cometd');

					this.players = new anxiety.PlayerList(this, 'players');
				},

					/* Signals */
					loggedIn: nop,
					denied: nop,
					heard: nop,
					
					login: function(name) {
					/*
						Initiates a login request. 
						If successful, calls this.loggedIn(player), where player
						is newly created Player instance.
						If not, calls this.denied(rsp).
					*/
					dojo.xhrPost({
							url: this.urlPrefix,
								handleAs: 'json',
								content: {
								what: 'needClient',
									name: name
									},

								error: dojo.hitch(this, function(e) {
										console.log(e);
										this.denied(e);
									}),

								load: dojo.hitch(this, function(rsp) {
										var p = new anxiety.Player(this, rsp.id, rsp.priv);
										this.loggedIn(p);
									})
								});
				}
			});


		/*
			Browser widgets
			------- -------

			ChatBrowser
			```````````
			A window with chat messages in it. We see it in WorldBrowser, until we log in,
			and then we see it in a PlayerBrowser as a part of a more complex setup.
		*/
		dojo.declare('anxiety.ChatBrowser', [dijit._Widget],
								 {   constructor: function(opts) {
										 this.world = opts.world;
										 this.maxPhraseCnt = 50;
									 },
										 
										 buildRendering: function() {
										 this.domNode = dojo.create('DIV');
									 },

										 postCreate: function() {
										 /*
											 This is called after constructor and buildRendering.
											 this.connect machinery was not initialized until now.
										 */
										 this.connect(this.world, 'heard', 'heard');
									 },


										 heard: function(phrase) {
										 // Someone said something
										 var phraseNode = dojo.create('DIV', {innerHTML: phrase.text}, this.domNode);
										 // Keep displayed phrase count < maxPhraseCnt
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
		dojo.declare('anxiety.WorldBrowser', [dijit._Widget, dijit._Templated],
								 {   templateString: null,
										 templatePath: '/templates/worldbrowser.xml',
										 widgetsInTemplate: true,
										 
										 constructor: function(opts) {
										 this.world = opts.world;
										 
										 this.beLoggedOut();
									 },

										 postCreate: function() {
										 this.connect(this.world, 'loggedIn',  'loggedIn');
										 this.connect(this.world, 'denied', 'denied');
										 /*
											 Make a datastore for a player list
										 */


										 /*
											 Connect PlayerList client command handlers to a datastore
										 */


									 },

										 beLoggedOut: function() {
										 // Setup a chat message view.
										 this.chatBrowser = new anxiety.ChatBrowser({
												 world: this.world
											 }, this.viewport);
									 },
										 
										 
										 loginBtnClicked: function() {
										 var loginName = this.usernameBox.attr('value');
										 if(!loginName.length) {
											 alert('Enter name to log in');
											 return;
										 }
										 
										 this.loginBtn.attr('disabled', true);
										 this.world.login(loginName);
									 },

										 denied: function(why) {
										 DBG(why);
										 this.loginBtn.attr('disabled', false);
									 },

										 loggedIn: function(player) {
										 this.chatBrowser.destroy();
										 this.playerBrowser = new anxiety.PlayerBrowser({player: player}, 
																																		this.viewport);
									 }
										 
								 });
		
		/*
			PlayerBrowser
			`````````````
			A tab container.
			Tab 1  - chat view and send box.
			Tab 2  - invitations view.
			Other tabs show games.

			XXX Where do we put 'Log out' button?
		*/			
		dojo.declare('anxiety.PlayerBrowser',
								 [dijit._Widget/*, dijit._Templated*/],
								 {   /*templateString: null,
										 templatePath: '/templates/playerbrowser.xml',
										 widgetsInTemplate: true,*/
										 constructor: function(opts) {
										 
											 
										 
									 }
								 });

		
	});



