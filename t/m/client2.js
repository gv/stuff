/*
	This is a client for a multiplayer card game using Dojo
*/

function nop() {} 

/*
	This class runs a sync mechanism with server.
*/
function World(urlPrefix) {
	this.urlPrefix = urlPrefix;
	
	// fetch a client list
	dojo.xhrGet({
			url: this.urlPrefix + 'clients',
				handleAs: 'json',
				load: dojo.hitch(this, 'updateClients')
				});
};


dojo.require('dijit.layout.SplitContainer');
dojo.require('dijit._Templated');


/*
	A widget that shows us client list, allows to login and spawns a PlayerBrowser
	when we do.
*/
dojo.declare('anxiety.WorldBrowser',
						 [
							dijit.layout.SplitContainer,
						 //dijit._Templated,
						 ],
						 
						 //templatePath: dojo.moduleUrl('anxiety', 'WorldBrowser.html'),

						 constructor: function(opts) {
							 


						 });

dojo.declare('anxiety.PlayerBrowser',
						 [dijit._Templated],
						 
						 templatePath: dojo.moduleUrl('anxiety', 'PlayerBrowser.html'),

						 constructor: function() {
							 


						 });

function browse(urlPrefix, l) {
	var w = new World(urlPrefix);
	new anxiety.WorldBrowser({world: w}, l);
}

