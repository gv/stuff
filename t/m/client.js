/*
	This is a browser client for multiplayer card game.
*/

// let's use this
//dojo.require('dojo.cookie');

// some generic accessors
var get = defer(function(object, key) {	return object[key];    });
var getContainer  = defer(function(widget) {	return widget.l;    });

// ----
				 
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

var browseList = defer(function(l, world) {
		// housewarm
		var usersDisplay = stick(l, 'UL', 'users');
		var invitationsDisplay = stick(l, 'UL', 'invitations');
		var panel = stick(l, 'DIV', 'panel');
		var inviteBtn = stick(panel, 'BUTTON', 'invite', 'Invite');
		// manual reload
		var reloadBtn = stick(panel, 'BUTTON', 'reload', 'Reload');

		//var reload; // remove this and see what happens

		var iter = defer(function(what) {
				if(what instanceof Event) {
					//alert(what);
					switch(what.target) {
					case reloadBtn:
					return reload();

					case inviteBtn:
					alert('invite');
					break;
					}
				} else { // answer
					alert(urlEncode(what));
				}



				iter(race(getEvt(reloadBtn), getEvt(inviteBtn)));
			});
		
		reload = function() {
			iter(askServer(world.prefix + 'clients'));
		};
		reload();
	});;
			
var browsePlayer = defer(function(l, player, world) {
		// build a player browser widget
		l.innerHTML = '';

		// build a panel
		var panel = stick(l, 'DIV', 'panel');
		var nameInd = stick(panel, 'DIV', 'playerName', player.name);
		var logOutBtn = stick(panel, 'BUTTON', 'logout', 'Log out');
		var reloadBtn = stick(panel, 'BUTTON', 'reload', 'Reload');

		var invitationPanel = stick(panel, 'DIV', 'invitations');
		var invitationList = stick(invitationPanel, 'DIV', 'invitationList');
		var invitationItems = [];
		

		clearFloats(panel);

		// XXX need more gamescreens
		var gameScreen  = stick(l, 'DIV', 'gameScreen');

		var refresh = defer(function(data) {
				// update invitations
				if(data.invitations) {
					for(var i = 0; i < data.invitations.length; i++) {
						var inv = data.invitations[i], it = invitationItems[i];
						if(it && it.who == inv.who && inv.whatGame == it.whatGame) 
							continue;
						
							
							
						
						
						
					
				var game;
				if(data.games) {
					for(var i in data.games) {
						game = data.games[i];
						break;
					}
				}
				
				if(!game) {
					return handleBtn(race(getEvt(logOutBtn), getEvt(reloadBtn)));
				}

				return race(browseGame(gameScreen, data),
										handleBtn(race(getEvt(logOutBtn)
																	 getEvt(reloadBtn))));
			});

		var start = defer(function() {
				return refresh(askServer(world.prefix + 'clients/' + player.id));
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

var browseGame = defer(function(container, game) {
	
    
	});
	
    
