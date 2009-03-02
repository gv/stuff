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

// if no id is given, asks user for a name
var getPlayer = function(world, id) {
	var r = {
		world: world
	};

	// ask 
	
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
			
// returns a player browser struct
var browsePlayer = defer(function(l, player) {
	// build a player browser widget
		l.innerHTML = '';
		var logOutBtn = stick(l, 'BUTTON', 'logout', 'Log out');
		var gameScreen  = stick(l, 'DIV', 'gameScreen');
		
		indicate(gameScreen, urlEncode(player));
		return getEvt(logOutBtn);
	});

var browseGame = function(game, container) {
    
};
	
    
