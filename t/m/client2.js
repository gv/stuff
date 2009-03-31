/*
	This is a client for a multiplayer card game using Dojo
*/

function nop() {} 

function DBG(x) {
	if(typeof console != 'undefined')
		console.log(x);
	return x;
}

/*
	Entry
*/
function browse(urlPrefix, l) {
	dojo.addOnLoad(function() {
			var w = new anxiety.World(urlPrefix);
			var br = new anxiety.WorldBrowser({world: w}, l);
			br.startup();
		});
}


dojo.require('dijit._Widget');
dojo.require('dijit._Templated');
dojo.require('dijit.layout.BorderContainer');
dojo.require('dijit.layout.ContentPane');
dojo.require('dijit.form.Button');
dojo.require('dijit.form.CheckBox');
dojo.require('dijit.Toolbar');

/*
	Cross-domain script loading is async and who know what else is
	So we need to define classes on load.
*/
dojo.addOnLoad(function() {
		/*
			This class runs a sync mechanism with server.
		*/
		dojo.declare('anxiety.World', null, {
				constructor: function(urlPrefix) {
					this.urlPrefix = urlPrefix;
					this.reload();
				},

					reload: function() {
					// fetch a client list
					dojo.xhrGet({
							url: this.urlPrefix + 'clients',
								handleAs: 'json',
								load: dojo.hitch(this, '_updateClients')
								});
				},	

					_updateClients: function(resp) {
					this.clients = resp.clients;
					this.refreshPlayers();
				},
					
					refreshPlayers: nop
			});


		/*
			A widget that shows us client list, allows to login and spawns a PlayerBrowser
			when we do.
		*/
		/*var tplUrl = dojo.moduleUrl('anxiety', 'WorldBrowser.html');
			DBG('Template URL is ' + tplUrl);*/

		dojo.declare('anxiety.WorldBrowser', [dijit._Widget, dijit._Templated],
								 { 
									 /*
										 templatePath must be XHR reachable so i will use templateString for a while
										 until i figure how to do better
									 */
									 templateString: 
									 '<div>' + 
										 ('<div dojoType="dijit.layout.BorderContainer" ' + 
											'style="width: 100%; height: 100%" ' + 
											'design="sidebar" gutters="true" liveSplitters="true">' + 
											('<div dojoType="dijit.layout.ContentPane" splitter="true" ' + 
											 'region="center">Hi, Im center</div>') + 
											('<div dojoType="dijit.layout.ContentPane" splitter="true" ' + 
										 'region="trailing" style="width: 200px;">' + 
											 '<div dojoAttachPoint="lList">fill me</div>' +
											 ('<div dojoType="dijit.Toolbar" ' + 
												'style="position:absolute; bottom:0; padding:10px;">' +
												'<div dojoType="dijit.form.Button">Reload</div>' +
												'</div>') +
											 '</div>') + 
											'</div>') + 
										 '</div>',
										 
										 templatePath: null, //tplUrl,
										 widgetsInTemplate: true,

										 constructor: function(opts) {
										 this._world = opts.world;


									 }
								 });

		dojo.declare('anxiety.PlayerBrowser',
								 [dijit._Widget, dijit._Templated],
								 { templateString: null,
										 templatePath: dojo.moduleUrl('anxiety', 'PlayerBrowser.html'),
										 widgetsInTemplate: true,
										 constructor: function(opts) {
							 


									 }
								 });
	});



