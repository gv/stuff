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

var getPlayer = function(world, id) {
	var r = {
		world: world
	};
	return r;
};


var getList = function(world) {
    var url = world.prefix + 'what=list';
    var r = {
	world: world,
	data: askServer(url)
    };

    return r;
};

// returns world browser data
var browse = function(worldUrl, container) {
    var world = mkWorld(worldUrl);
	var r = {
		l: L(container),
		world: world,
		playerBrowser: browsePlayer(getPlayer(world, getCookie('tplayerid')),
					    addEl(container, 'DIV', 'playerBrowser')),
		listBrowser: browseList(getList(world), 
					addEl(container, 'DIV', 'listBrowser'))
	};

	// build world browser widget
	

	return r;
};

var browseList = function(list, container) {
    var r = {
	l: L(container)
    };
    return r;
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
	
    
