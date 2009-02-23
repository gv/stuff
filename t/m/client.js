// let's use this
dojo.require('dojo.cookie');

// some generic accessors
var get = dfr(function(object, key) {	return object[key];    });
var getContainer  = dfr(function(widget) {	return widget.l;    });

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
	var world = mkWorld(worldUrl);
	var r = {
		l: L(container),
		world: world
	};

	// build world browser widget
	browseList(getList(world), 
						 addEl(container, 'DIV', 'listBrowser'));

	// sequence is
	// get username
	// do stuff until logout
	var iter = defer(function(lastBrowsingResult) {
			var player = getPlayer(world);
			return iter(browsePlayer(result));
		});

		
	/*
		playerBrowser: browsePlayer(getPlayer(world, getCookie('tplayerid')),
		addEl(container, 'DIV', 'playerBrowser')),
	*/
	return r;
};

var browseList = function(list, container) {
	var r = {
		l: L(container)
	};
	
	
	
	//return r;
};

// returns a player browser struct
var browsePlayer = function(player, container) {
	var r = {
		l: L(container),
		player: player,
		gameBrowser: browseGame( )
	};
	
	// build a player browser widget
	
	
	
	return r;
};

var browseGame = function(game, container) {
	var r = 
    
};
	
    
