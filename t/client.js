function L(id) {
	return document.getElementById(id);
}


function addEl(parent, tagName) {
	return L(parent).appendChild(document.createElement(tagName));
}



var mkWorld = function(url) {
	var r = {
		url: url
	};
	return r;
};

var getPlayer = function(world, id) {
	var r = {
		world: world
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
																addEl(container, 'DIV')),
																
	};

	// build world browser widget
	

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
	
    
