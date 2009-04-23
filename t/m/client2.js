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
				onplayerschanged: nop,
					
					_updateState: function(state) {
					this.players = state.players;
					this.onplayerschanged();
				},
					
					handleAddPlayer: function(msg) {
					this.players.push(msg);
					this.onplayerschanged();
				},

					handleRmPlayer: function(msg) {
					// XXX
				
					this.onplayerschanged();
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
					
					login: function(name) {
					dojo.xhrPost({
							url: this.urlPrefix,
								handleAs: 'json',
								content: {
								what: 'needClient',
									name: name
									},

								error: dojo.hitch(this, function(e) {
										console.log(e);
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

			WorldBrowser
			````````````
			A widget that shows us client list, allows to login and spawns a PlayerBrowser
			when we do.
		*/
		/*
			Would be nice to do 

			var tplUrl = dojo.moduleUrl('anxiety', 'WorldBrowser.html');
			DBG('Template URL is ' + tplUrl);
			
			but templatePath must be XHR reachable so i will use templateString for a while
			until i figure how to do better
		*/

		dojo.declare('anxiety.WorldBrowser', [dijit._Widget, dijit._Templated],
								 { 
									 templateString: 
									 '<div>' + 
										 ('<div dojoType="dijit.layout.BorderContainer" style="width: 100%; height: 100%" design="sidebar" gutters="true" liveSplitters="true">' + 
											('<div dojoType="dijit.layout.BorderContainer" region="center" splitter="true">' +
											 ('<div dojoType="dijit.layout.ContentPane" region="top">' +
												('<input type="text" dojoType="dijit.form.TextBox" dojoAttachPoint="usernameBox" />' +
												 '<button dojoType="dijit.form.Button">Login</button>') +
												'</div>') +
											 '</div>' + 
											 '<div dojoType="dijit.layout.ContentPane" splitter="true" region="trailing" style="width: 200px;">' + 
											 ('<div dojoAttachPoint="lList">fill me</div>' +
												'<div dojoType="dijit.Toolbar" style="position:absolute; bottom:0; padding:4px;">' +
												('<div dojoType="dijit.form.Button">Reload</div>') +
												'</div>') +
											 '</div>') + 
											'</div>') + 
										 '</div>',
										 
										 templatePath: null, //tplUrl,
										 widgetsInTemplate: true,

										 constructor: function(opts) {
										 this.world = opts.world;


									 }
								 });

			dojo.declare('anxiety.PlayerBrowser',
									 [dijit._Widget, dijit._Templated],
									 { templateString: null,
										 templatePath: dojo.moduleUrl('anxiety', 'PlayerBrowser.html'),
											 widgetsInTemplate: true,
											 constructor: function(opts) {
											 
											 

									 }
								 });
	});



