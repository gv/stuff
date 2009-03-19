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
	var listBrowser = mkListBrowser();
	browseList(listBrowser, stick(l, 'DIV', 'listBrowser'), world);
	
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
			setPlayer(listBrowser, null);
			var player = getPlayer();
			setPlayer(listBrowser, player);
			var loggedOut = browsePlayer(playerScreen, player, listBrowser);
			go(loggedOut);
		});
	
	go();
};

var browsePlayer = defer(function(l, player, listBrowser) {
		// build a player browser widget
		l.innerHTML = '';

		// build a panel
		var panel = stick(l, 'DIV', 'panel');
		var nameInd = stick(panel, 'DIV', 'playerName', player.name);
		var logOutBtn = stick(panel, 'BUTTON', 'logout', 'Log out');
		var reloadBtn = stick(panel, 'BUTTON', 'reload', 'Reload');
		clearFloats(panel);

		// Also, we should handle invitation messages in parallel message handling cycle.
		var invitationPanel = listBrowser.invitationPanel;
		invitationPanel.innerHTML = '';
		var invitationList = stick(invitationPanel, 'DIV', 'invitationList');
		var invitationItems = [];

		var listExtPanel = listBrowser.extPanel;
		listExtPanel.innerHTML = '';
		var inviteBtn = stick(listExtPanel, 'BUTTON', 'invite', 'Invite');

		// XXX need more gamescreens
		var gameScreen  = stick(l, 'DIV', 'gameScreen');

		var state = {};
		var refresh = defer(function(playerState) {
				// update invitations
				state = playerState;
				if(state.invitations) {
					updateControls(state.invitations,
												 invitationItems,
												 function() {
													 var lIt = stick(invitationList, 'DIV', 'invitation');
													 var it = {
														 l: lIt,
														 lCbx: stick(lIt, 'INPUT'),
														 lLabel: stick(lIt, 'DIV', 'label')
													 };
													 it.lCbx.type = 'checkbox';
													 clearFloats(lIt);
													 return it;
												 },

												 function(it, inv) {
													 if(it.who == inv.who && inv.whatGame == it.whatGame) 
														 return;

													 it.who = inv.who;
													 it.whatGame = inv.whatGame;

													 it.l.title = it.who + '(' + it.whatGame + ')';
													 
													 /* If our version of list does not yet contain player
															who sent an invite, we need to wait for it to update
													 */
													 var getPlayerName = defer(function() {
															 var p = first(filter(player.world.players, 
																									 function(p) {
																											return p.id == inv.who;
																									 }));
															 return p && p.name;
															 /*return p ? 
															 p.name : 
															 getPlayerName(getEvt(player.world, 
															 'updateplayers'));*/
														 });

													 /* XXX cancel this indication if this element is 
															used for another invitation */
													 // abandon(inv.playerName);
													 inv.playerName = getPlayerName() || 
													 // XXX Here we can call getEvt more than once in a row
													 // That doesn't work now
													 getPlayerName(getEvt(player.world, 
																								'updateplayers'));
													 
													 indicate(it.lLabel, inv.playerName);
												 });
				}
					
				return go();
			});

		var go = defer(function(answer) {
				if(answer) {
					if(answer.err) {
						alert(answer.err.msg);
					}
				}

				var common = race(getEvt(logOutBtn),
													 start(getEvt(reloadBtn)));
				
				var gameState = first(state.games);
				if(gameState) {
					gameState.player = player;
					return race(common, 
											go(browseGame(gameScreen, gameState)));
				} else {
					return race(common,
											go(invitePeople()));
				}
			});

		var start = defer(function() {
				return refresh(getPlayerState(player));
			});
		
		var invitePeople = defer(function(/*selChange*/) {
				var next = invitePeople(getEvt(listBrowser, 'playersselectionchange'));
				/*if(!selChange)
					return next;*/
				var selPlayers = getSelectedPlayers(listBrowser);
				if(!selPlayers.length)
					return next;
				return race(sendInvitations(selPlayers, 
																		getEvt(inviteBtn)),
										next);
			});

		var sendInvitations = defer(function(players) {
					return sendFromPlayer(player, {
							what: 'invite', 
							whatGame: 'thousand',
							who: map(players, prop('id')).join(',')
						});
			});				
				
		return start();
	});

var getPlayerState = function(player) {
	return askServer(player.world.prefix + 'clients/' + player.id, 
{priv: player.priv}
									 );
	
};

var sendFromPlayer = defer(function(player, data, suffix) {
		data.from = player.id;
		data.priv = player.priv;
		return send(player.world.prefix + (suffix || ""), data);
	});


var browseGame = defer(function(l, game) {
		/* "game" object remains the same in this function for clarity.
			 We store game state in "state"
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

/*
	List Browser
	---- -------
	This browser works in two modes:
	
	A. Not logged in: shows a list of players' names.
	B. Logged in: shows a list of players' names and controls to invite 
	some of them to play a game
*/

var mkListBrowser = defer(function() {
		return {};
	});

var setPlayer = defer(function(listBrowser, player) {
		listBrowser.player = player;
		listBrowser.onloggedinstatechange && listBrowser.onloggedinstatechange(player);
	});

var getSelectedPlayers = defer(function(listBrowser) {
		return filter(listBrowser.lUsers, function(w) {
				return w.lCbx.checked;
			});
	});

/*
var getPlayersSelectionChangeEvt = defer(function(listBrowser) {
		// This call enables checkboxes

	});
*/

var browseList = defer(function(listBrowser, l, world) {
		l.innerHTML = '';

		// Housewarm
		var usersDisplay = stick(l, 'DIV', 'users'), lUsers = [];
		listBrowser.lUsers = lUsers;
		stick(usersDisplay, 'H3', '', 'Users');

		// We put invitation panel in a middle of the window.
		// XXX Control it from browsePlayer and make invisible when not logged in.
		var midPanel = stick(l, 'DIV', 'midPanel');
		stick(midPanel, 'H3', '', 'Invitations');
		listBrowser.invitationPanel = stick(midPanel, 'DIV', 'invitations');
		var playBtn = stick(midPanel, 'BUTTON', 'play', 'Play');

		var panel = stick(l, 'DIV', 'panel');
		// manual reload
		var reloadBtn = stick(panel, 'BUTTON', 'reload', 'Reload');
		listBrowser.extPanel = stick(panel, 'DIV', 'extra'); // for browsePlayer

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
														 lLabel: stick(lIt, 'DIV')
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

					world.players = data.players;
					world.onupdateplayers && world.onupdateplayers();
				}

				return go();
			});

		var go = defer(function(what) {
				/* When someone calls getEvt(listBrowser, 'playersselectionchange'
					 we need to enable checkboxes.
				*/
				var common = race(reload(getEvt(reloadBtn)), 
													go(getEvt(listBrowser, 'prepareevt')));

				if(!listBrowser.onplayersselectionchange) 
					return common;
				
				var evts = map(map(lUsers, 
													 prop('lCbx')),
											 getEvt);
				return race(common, 
										go(defer(listBrowser.onplayersselectionchange)
											 (race.apply(null, evts))));
			});
			
		var reload = defer(function() {
				return refresh(askServer(world.prefix + 'clients'));
			});
		
		reload();
		// XXX this result is still linked
	});
			
