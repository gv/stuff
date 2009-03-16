/*
	This is a browser client for multiplayer card game.
*/

// let's use this
//dojo.require('dojo.cookie');

var mkWorld = function(url) {
	var r = {
		prefix: url
	};
	return r;
};

var browse = function(worldUrl, container) {
	var world = mkWorld(worldUrl), l = L(container);

	// Build a home.
	var mainPanel = stick(l, 'DIV', 'mainPanel');

	var loginScreen = stick(mainPanel, 'DIV', 'loginScreen');
	stick(loginScreen, 'DIV', 'label', 'Name');
	var nameInp = stick(loginScreen, 'INPUT', '');
	nameInp.type = 'text';
	var loginBtn = stick(loginScreen, 'BUTTON', '', 'Login');
	stick(loginScreen, 'DIV', 'clear');

	var playerScreen = stick(mainPanel, 'DIV', 'playerScreen');

	// Parallel branch 1.
	browseList(stick(l, 'DIV', 'listBrowser'), world);
	
	// Parallel branch 2.
	var getName = defer(function() {
			nameInp.disabled = false;
			var readName = defer(function() {
					nameInp.disabled = true;
					return nameInp.value;
				});
			return readName(getEvt(loginBtn));
		});

	var login = defer(function(name) {
			if(name) {
				return send(world.prefix, {what: 'needClient', name: name});
			} else {
				alert('ENTER NAME');
				return login(getName());
			}
		});

	var getPlayer = defer(function(ans) {
			if(ans) {
				if(ans.err) {
					alert(ans.err.msg);
				} else {
					ans.world = world;
					return ans;
				}
			}
			return getPlayer(login(getName()));
		});

	var go = defer(function() {
			var player = getPlayer();
			var loggedOut = browsePlayer(playerScreen, player);
			go(loggedOut);
		});
	
	go();
};

var browsePlayer = defer(function(l, player) {
		// build a player browser widget
		l.innerHTML = '';

		// build a panel
		var panel = stick(l, 'DIV', 'panel');
		var nameInd = stick(panel, 'DIV', 'playerName', player.name);
		var logOutBtn = stick(panel, 'BUTTON', 'logout', 'Log out');
		var reloadBtn = stick(panel, 'BUTTON', 'reload', 'Reload');

		// XXX invitation list should have it's own container
		// Also, we should handle invitaion messages in parallel message handling cycle.
		var invitationPanel = stick(panel, 'DIV', 'invitations');
		var invitationList = stick(invitationPanel, 'DIV', 'invitationList');
		var invitationItems = [];

		clearFloats(panel);

		// XXX need more gamescreens
		var gameScreen  = stick(l, 'DIV', 'gameScreen');

		var refresh = defer(function(playerState) {
				// update invitations
				if(playerState.invitations) {
					updateControls(playerState.invitations,
												 invitationItems,
												 function() {
													 var lIt = stick(invitationList, 'DIV', 'invitation');
													 var it = {
														 l: lIt,
														 lCbx: stick(lIt, 'INPUT'),
														 lLabel: stick(lIt, 'DIV')
													 };
													 it.lCbx.type = 'checkbox';
													 return it;
												 },

												 function(it, inv) {
													 if(it.who == inv.who && inv.whatGame == it.whatGame) 
														 return;

													 it.who = inv.who;
													 it.whatGame = inv.whatGame;
													 
													 /* If our version of list does not yet contain player
															who sent an invite, we need to wait for it to update
													 */
													 var getPlayerName = defer(function() {
															 var p = first(filter(player.world.players, 
																									 function(p) {
																										 p.id == inv.who;
																									 }));
															 return p && p.name;
															 /*return p ? 
															 p.name : 
															 getPlayerName(getEvt(player.world, 'updateplayers'));*/
														 });

													 inv.playerName = getPlayerName() || 
													 getPlayerName(getEvt(player.world, 
																								'updateplayers'));
													 
													 indicate(it.lLabel, inv.playerName);
													 /* XXX cancel this indication if this element is 
															used for another invitation */
												 });
				}
					
				var gameState = first(playerState.games);
				if(!gameState) {
					return handleBtn(race(getEvt(logOutBtn), getEvt(reloadBtn)));
				}
				
				gameState.player = player;

				return race(browseGame(gameScreen, gameState),
										handleBtn(race(getEvt(logOutBtn),
																	 getEvt(reloadBtn))));
			});

		var start = defer(function() {
				return refresh(getPlayerState(player));
			});

		var handleBtn = defer(function(evt) {
				switch(evt.target) {
				case logOutBtn:
				return true;
				case reloadBtn:
				return start();
				}
			});

		return start();
	});

var getPlayerState = function(player) {
	return askServer(player.world.prefix + 'clients/' + player.id, {priv: player.priv});
};

var sendFromPlayer = defer(function(player, data, suffix) {
		data.from = player.id;
		data.priv = player.priv;
		return send(player.world.prefix + (suffix || ""), data);
	});


var browseGame = defer(function(l, game) {
		/* game object remains the same in this function for clarity.
			 We store game state in state
		*/
		l.innerHTML = '';

		var player = game.player;

    var lRound = stick(l, 'DIV', 'round');
		var lScore = stick(l, 'DIV', 'score');
		
		var getGameState = defer(function(playerState) {
				return playerState.games &&	playerState.games[game.id];
			});

		var handleCommand = defer(function(cmd) {
				/* Here we handle an asynchronous command from server
				 */
				

			});

		var display = defer(function(state) {
				if(state.over) 
					return true;

				if(state.round) {
					var browsedRound = browseRound(lRound, state.round, game);
					/* To get a next round, we must ask the whole clients state again. */
					return go(browsedRound);
				}

				// XXX What does that mean if game is not over and stil we got no round?
				// Do we need to wait or something?			
				return true;
			});				
		
		var go = defer(function(gameState) {
				return display(getGameState(getPlayerState(player))); 
			});
				
		return display(game);
	});

/*
	Returns an asynchronous message from server.
*/
var getMsg = defer(function(player) {


	});

var browseRound = defer(function(l, round, game) {
		/* The main game message processing must happen here.
		*/
		l.innerHTML = '';
		
		var lHand = stick(l, 'DIV', 'hand');
		var lHandCards = [], lTableCards = [];

		var handleMsg = defer(function(msg, state) {
				switch(msg.what) {
				case 'cards': // we were dealt some cards


				break;
				}
			});

		var go = defer(function(state) {



			});

		return go(round);
	});
	
    
var browseList = defer(function(l, world) {
		// housewarm
		var usersDisplay = stick(l, 'UL', 'users'), lUsers = [];
		
		//var invitationsDisplay = stick(l, 'UL', 'invitations');
		var panel = stick(l, 'DIV', 'panel');
		var inviteBtn = stick(panel, 'BUTTON', 'invite', 'Invite');
		// manual reload
		var reloadBtn = stick(panel, 'BUTTON', 'reload', 'Reload');

		var refresh = defer(function(data) {
				if(data.err) {
					alert(data.err.msg);
				}

				if(data.players) {
					updateControls(data.players,
												 lUsers,
												 function() {
													 var lIt = stick(usersDisplay, 'DIV', 'user');
													 var it = {
														 lCbx: stick(lIt, 'INPUT'),
														 lLabel: stick(lIt, 'DIV'),
													 };
													 it.lCbx.type = 'checkbox';
													 return it;
												 },
												 function(it, usr) {
													 if(it.id == usr.id) return;
													 it.id = usr.id;
													 it.name = usr.name;
													 it.lLabel.innerHTML = usr.name;
													 it.lLabel.title = usr.id;
												 });

					world.players = players;
					world.onupdateplayers && world.onupdateplayers();
				}

				go();
			});

		var getSelectedIds = function() {
			return filter(map(lUsers, function(w) {
						return w.lCbx.checked && w.id;
					}), identity);
		};

		var go = defer(function(what) {
				if(what instanceof Event) {
					//alert(what);
					switch(what.target) {
					case reloadBtn:
					return reload();

					case inviteBtn:
					var ids = getSelectedIds();

					if(!ids.length) {
						alert('Select some players');
						break;
					}

					refresh(sendFromPlayer(player, {
								what: 'invite', 
								whatGame: 'thousand',
								who: ids.join(',')
							}));
					
					break;
					}
				} 

				var evts = map(map(lUsers, 
													 prop('lCbx')),
											 getEvt);
				
				if(getSelectedIds().length)
					evts.push(getEvt(inviteBtn));

				go(race(getEvt(reloadBtn), 
								race.apply(null, evts)));
			});												
			
		var reload = function() {
			refresh(askServer(world.prefix + 'clients'));
		};
		
		reload();
	});
			
