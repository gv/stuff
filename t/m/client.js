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


var getList = function(world) {
	var url = world.prefix + 'clients';
	var r = {
		world: world,
		data: askServer(url)
	};

	return r;
};

var browse = function(worldUrl, container) {
	var world = mkWorld(worldUrl), l = L(container);

	// Parallel branch 1.
	browseList(stick(l, 'DIV', 'listBrowser'));


	// Parallel branch 2.
	var iter = defer(function() {
			var player = getPlayer(world);
			return iter(browsePlayer(result));
		});

		
	/*
		playerBrowser: browsePlayer(getPlayer(world, getCookie('tplayerid')),
		addEl(container, 'DIV', 'playerBrowser')),
	*/
};

var browseList = defer(function(l) {
		// housewarm
		var usersDisplay = stick(l, 'UL', 'users');
		var invitationsDisplay = stick(l, 'UL', 'invitations');
		var panel = stick(l, 'DIV', 'panel');
		var inviteBtn = stick(panel, 'BUTTON', 'invite', 'Invite');
		// manual reload
		var reloadBtn = stick(panel, 'BUTTON', 'reload', 'Reload');

		var iter = defer(function(what) {
				if(what instanceof Event) {
					alert(what);
					switch(what.target) {
					case reloadBtn:
						alert('reload');
						break;

					case inviteBtn:
						alert('invite');
						break;
					}
				}


				/*return */iter(race(getEvt(reloadBtn), getEvt(inviteBtn)));
			});

		iter();
	});;
			
// returns a player browser struct
var browsePlayer = function(l) {
	var r = {
		l: L(container),
		player: player,
		gameBrowser: browseGame( )
	};
	
	// build a player browser widget
	
	
	
	return r;
};

var browseGame = function(game, container) {
    
};
	
    
