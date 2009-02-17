function l(id) {
    return document.getElementById(id);
}

var mkWorld = function(url) {
    var r = {
	url: url
    };
    return r;
};

var getPlayer = function(world, id) {
    

};

// returns world browser data
var browse = function(worldUrl, container) {
    var world = mkWorld(worldUrl);
    var r = {
	l: l(container),
	world: world,
	playerBrowser: browsePlayer( )
    };

    // build world browser widget



    return r;
};

// returns a player browser struct
var browsePlayer = function(world, player, container) {
    var r = {
	l: l(container),
	player: player,
	gameBrowser: browseGame( )
    };

    // build a player browser widget



    return r;
};

var browseGame = function(world, player, game, container) {
    
};
	
    