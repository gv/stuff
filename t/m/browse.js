/*
 * Big browser. 
 */

dojo.require('anxiety');

dojo.require('dojo.data.ItemFileWriteStore');
dojo.require('dijit._Widget');
//dojo.require('dijit._Templated');
dojo.require('dijit.layout.BorderContainer');
dojo.require('dijit.layout.ContentPane');
dojo.require('dijit.form.CheckBox');
//dojo.require('dijit.form.TextBox');
//dojo.require('dijit.Toolbar');

dojo.require('dojox.grid.DataGrid');

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



dojo.addOnLoad(function() {
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
						dojo.style(this.simpleStatusBarNode, {
								visibility: str ? 'visible' : 'hidden'});
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
					this.attr({region: 'center'});
					this.players.passStateToWidget(this);
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
					this.head = new dijit.layout.ContentPane({
							region: 'top',
							content: 'Users'
						});
					this.addChild(this.head);
					this.playerListView = new dojox.grid.DataGrid({
							region: 'center',
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
					//this.playerListView.startup();
					this.toolbar = new dijit.layout.ContentPane({
							region: 'bottom'
						});
					this.addChild(this.toolbar);
							
					this.players.passStateToWidget(this);

				}

				,handleRmPlayer: function(m) {
					// m.who is identifier of the removed player
					this.playerStore.fetch({
							query: {id: m.who},
								scope: this.playerStore,
								onItem: function(it) {
								this.deleteItem(it)
									}
						});
				}
				
				,handleAddPlayer: function(m) {
					this.playerStore.newItem(m);
				};


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
								this.report('Creating player list browser...', this.world.playersFetch);
								var listBrowser = LB = new anxiety.ListBrowser({
										players: ps
									});
								listBrowser.attr('region', 'trailing');
								this.mainLayout.addChild(listBrowser);
								listBrowser.startup();
							}));


					this.loggedOut();

					this.connect(this.world, 'loggedIn',  'loggedIn');
					this.connect(this.world, 'loggedOut', 'loggedOut');
					/* FIXME Why do i always need to say it twice? */
					this.connect(this.world, 'error', function(er) {
							alert(er.msg);
						});

					this.mainLayout.startup();
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
									this.report('Creating chat browser...');
									CB = this.chatBrowser = new anxiety.ChatBrowser({
											players: ps
										});
									this.moveIntoClientWindow(this.chatBrowser);
									this.chatBrowser.startup();
									this.report('');
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
					
					this.player.passStateToWidget(this);
					this.connect(this.player, 'invited', 'addInv');
					this.connect(this.player, 'gameStarted', 'updateGame');
				}

				,update: function() {
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
					
				,handleUninvited: function(inv) {
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
