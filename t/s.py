#
#			A networked card game server
#			- --------- ---- ---- ------
#
#			This program will not use any template system, because all the 
#			data will be transmitted in JSON form by some kind
#			of AJAX.

print "started" 
# So we know how long does it take for our python to load

# Servers setup
# ------- -----

#STATIC_PREFIX = "http://93.92.203.153:8008/"
#STATIC_CLIENT_CODE_PREFIX = STATIC_PREFIX + "t/"
STATIC_PREFIX = "/"
PORT_NUMBER = 2222
PORT_NUMBER = 6080 # for bishop


# Imports
# -------

import os, sys
import cjson
from twisted.web2 import http, server, static, http_headers
print "core libraries loaded"

# our libs
from world import In, Resource, cfirst, getArg, Game, MessageDriven, Client, Player
import world

#print "all libraries loaded"

# utils
DEBUG = False
DEBUG = True



				
class DictResource(Resource):
		def __init__(self, dct):
				self.dct = dct

		def childFactory(self, req, name):
				return self.dct.get(name)

		def render(self, request):
				return http.Response(200, stream='not this')
				

#
#		 Players
#		 -------
#
#		 This part defines two kinds of clients, which are Player and PlayerList
#

# The players list will be accessible at http://hostname/clients/players
class PlayerList(Client):
		def __init__(self):
				Client.__init__(self, 'players')
				self.chatLog = []

		def getState(self):
				return {
						'players': [dict(id = p.id, name = p.name) 
												for p in Client.table.values()
												if isinstance(p, Player)],
						'chatLog': self.chatLog
						}

		def handleRmPlayer(self, who, **k):
				del Client.table[who]

		def handleAddPlayer(self, **k):
				"""	 It's a message for remote, actual adding is already done.
				"""
				pass

		def handleChat(self, author, phrase,**k):
				""" 
				"""
				self.chatLog += [{'author': author,
													'phrase': phrase}]
				self.chatLog = self.chatLog[-10:]
				

# Set up a 'players' client
PlayerList()

# Now we can load games

import thousand, tictactoe

# TODO how do we register a gameType


#						 
#			Entry point
#			----- -----
#
#			This is a root resource. It routes GET and POST requests to following 
#			resources:
#
#			/					(in)	->	 Client manager input. Post to it if you need a client id.
#													 Also, post to it if you have a client id but don't 
#													 need it anymore.
#			/					(out) ->	 Also if you GET it, you will retrieve browser client 
#													 HTML page.
#			/clients	(out) ->	 Client manager. Reports clients' states and player list.
#			/games		(in)	->	 Game Manager. Routes POST requests from remotes to Game 
#													 objects,	 which handle them according to specific game 
#													 rules.
#			/cometd					->	 Async message server.
#			/templates			->	 A static directory for dojo widget templates. Must be 
#													 XHR reachable from 'world' domain, so we put it here and 
#													 not on a static server.
#			/dlib						->	 Debug version of Dojo library.
#
class Entry(In):
		def __init__(self):
				Resource.__init__(self)
				self.putChild('clients', DictResource(Client))
				self.putChild('games', DictResource(Game))
				self.putChild('cometd', world.bus) 
				self.putChild('templates', static.File(os.path.abspath("templates")))
				self.putChild('dlib', static.File('/opt/share/www/dojod'))
				if not STATIC_PREFIX.startswith('http'): # got no static server
						mediaPath = os.path.join(os.path.dirname(world.__file__), 'm')
						self.child_m = static.File(mediaPath)
				self.child_comettest = static.File(os.path.join(os.path.dirname(world.__file__), 
																												'comettest.html'))
				Client.get('players').postMessage(what = 'chat',
																					author = 'World',
																					phrase = 'Started working...')
																					
						

		def _getPlayerById(self, id):
				rv = Client.get(id)
				if isinstance(rv, Player): return rv
				raise ValueError("No player %s" % id)


		# Input handlers.
		# ----- ---------
		#
		# Public message
		#
		def handleNeedClient(self, request):
				# player must have a name, so we can show him in a list
				name = getArg(request, 'name')
				if Player.getByName(name):
						raise ValueError('There already is a player named ' + name)
				player = Player(name)
				return dict(id = player.id, 
										priv = player.priv, 
										name = player.name)

		#
		# Private messages (need authentication)
		#
		def handleIQuit(self, req):
				player = self.getPlayer(req)
				player.close()
				return {}

		def handleChat(self, req):
				# It must support auth
				player = self.getPlayer(req)
				Client.get('players').postMessage(
						what = 'chat',
						phrase = getArg(req, 'phrase'),
						author = player.id
						)
				return {}
								

		def handleInvite(self, req):
				""" Any player can send 'invite' with list of target ids
				Then each of the targets gets and keeps an invitation
				"""
				inviter = self.getPlayer(req)
				ids = getArg(req, 'target').split(' ')
				errMsgs = []
				for id in ids:
						if id == inviter.id:
								errMsgs += ["You can't invite yourself"]
								continue
						who = Client.get(id)
						if not who:
								errMsgs += ["No client %s" % id]
						# TODO Don't send if he already got equivalent invitation.
						who.postMessage(what = 'invited',
														who = inviter.id,
														gameType = getArg(req, 'gameType'))
				rv  = {}
				if errMsgs != []:
						# it's partial success so we don't raise exception
						rv['err'] = {'msg': "\n".join(errMsgs)}
				return rv

		def handleStartGame(self, req):
				""" When player has enough invitations, he can send a 'startGame'
				"""
				starter = self.getPlayer(req)
				# check everything
				ids = getArg(req, 'with').split(' ')
				partners = [starter]
				errMsgs = []
				for id in ids:
						player = Client.get(id)
						if not player:
								errMsgs += ["No player %s" % id]
								continue
						partners += [player]
						# remove invitation
						starter.postMessage(what = 'uninvited',
																gameType = getArg(req, 'gameType'),
																who = player.id)
				# Now let's start
				# FIXME Account for other kinds of games here
				thousand.Game(partners)
				rv  = {}
				if errMsgs != []:
						# it's partial success so we don't raise exception
						rv['err'] = {'msg': "\n".join(errMsgs)}
				return rv



		# Browser HTML code.
		# ------- ---- -----

		def render(self, request):
				if 'POST' == request.method:
						return In.render(self, request)
				worldPrefix = '/'
				# registerModulePath must be before inclusion of client2.js
				if DEBUG:
						dojoLoaderUrl = "/dlib/dojo/dojo.js"
				else:
						dojoLoaderUrl = STATIC_PREFIX + 'dlib/dojo/dojo.xd.js'

						
				if STATIC_PREFIX.startswith('http'): 
						mediaUrlPrefix = STATIC_PREFIX + 't/'
				else:
						mediaUrlPrefix = '/m/'

				hdrs = http_headers.Headers()
				hdrs.addRawHeader("Content-Type", "text/html; charset=utf-8")
				return http.Response(200, headers=hdrs, stream=("""<html><head>
<link rel=stylesheet href="%(static)scss.css" />
<SCRIPT TYPE="text/javascript" SRC="%(dldr)s" djConfig="isDebug: true"></SCRIPT>
<SCRIPT>
dojo.registerModulePath('anxiety', '%(static)s');
</SCRIPT>
<script src="%(static)sclient2.js"></script>
</head>
<body class="tundra">
<div id="worldBrowser"></div>
<script>
browse("%(world)s", "worldBrowser");
</script>
</html>
""") % { 'static' : mediaUrlPrefix, 
				 'world': worldPrefix, 
				 'dldr': dojoLoaderUrl})


				

if __name__ == '__main__':
		# serve all this
		print 'Starting server...'
		from twisted.web2 import server, channel
		from twisted.internet import reactor

		site = server.Site(Entry())
		reactor.listenTCP(PORT_NUMBER, channel.HTTPFactory(site))

		# For twisted.web it was
		# reactor.listenTCP(PORT_NUMBER, site)
		
		# Complicated stuff
		#from twisted.application import service , strports
		#app = service.Application('worldd')
		#svc = strports.service('tcp:%d' % PORT_NUMBER, channel.HTTPFactory(site))
		#svc.setServiceParent(app)
		
		print "Running reactor..."
		reactor.run()



