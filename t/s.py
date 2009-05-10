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
from base64 import urlsafe_b64encode, urlsafe_b64decode
from random import randint, shuffle
import cjson
from twisted.web2 import http, server, static
print "core libraries loaded"

# cometds setup.py is broken so i checked in entire cometd tree
# Who knows what else is broken there
# It also lacks a server side messaging API
# So we will use twisted-cometd-client.py from there,
# which i renamed to twcocl.py
# ---
# so
# first cometd is directory name
# second cometd is filename
from cometd import cometd, twcocl

# our libs
from world import In, Resource, cfirst, getArg, Game
import world
import thousand

print "all libraries loaded"

# utils
DEBUG = False
DEBUG = True


def randomBinString(length):
		s = ""
		while len(s) < length: s += chr(randint(0, 255))
		return s


				
class DictResource(Resource):
		def __init__(self, dct):
				self.dct = dct

		def childFactory(self, req, name):
				return self.dct.get(name)

		def render(self, request):
				return http.Response(200, stream='not this')
				

#		 
#		 Clients (outs)
#		 ------- ------
#
#		 Every client is an 'out' port of the world.
#		 (When I say 'out' think stdout: we can write to them from this program)
#		 Client state is completely stored in the world.
#		 All remote clients are just browsers. They can:
#			- request the whole client's state
#			- subscribe to get messages that control the client
#

"""
bus = twcocl.CometdClient(port=PORT_NUMBER)
bus.init()
"""

from twisted.python import log
log.startLogging(sys.stderr)
bus = cometd.cometd()
bus.verbose = True
# Get needed client id
cometd.BayeuxServer.cometClients.registerConnect(
		client_id = 'world',
		auth_type = 'none',
		connection_type = 'long-polling' # make 'em happy
)
				


class MessageDriven:
		"""	 A utility base class to control some objects by JSON-serializable messages
		"""
		def postMessage(self, msg=None, **kw):
				msg = msg or kw
				self.processMessage(msg)
				
		def processMessage(self, msg):
				what = msg['what']
				handlingMethod = getattr(self, 'handle' + cfirst(what))
				handlingMethod(**msg)

		def getState(self):
				"""	 If browser asks, we must give him something
				"""
				raise "Implement getState!"


class Client(MessageDriven, Resource):
		""" Client object is a resource containing a state of a 
		remote browsable/trackable entity.

		Each clients state is stored on the server, so user 
		can close her browser and open it again and nothing wil be lost.
		
		This means a client object must be a JSON-serializable data structure,
		controllable by JSON-serializable messages.
		"""

		table = {}

		@classmethod 
		def get(cls, id):
				return cls.table.get(id)

		def __init__(self, id = None):
				Resource.__init__(self)
				self.id = id or urlsafe_b64encode(randomBinString(8))
				self.revision = 0
				Client.table[self.id] = self

		def postMessage(self, msg=None, **kw):
				msg = msg or kw
				# Image with rev X can handle message with rev X.
				msg['revision'] = self.revision
				self.processMessage(msg)
				self.revision = msg['revision'] + 1

				# Post to cometd

				bxMsg = {
						'channel': '/' + self.id,
						'clientId': 'world',
						'data': msg
						}
				cometd.BayeuxServer.Publish(bxMsg, None)
				bus.route(None, bxMsg)
				
				

		# Resource interface
		# -------- ---------

		def render(self, request):
				if self.authenticate(request):
						state = self.getState()
						state['revision'] = self.revision
						return http.Response(200, stream=cjson.encode(state))
				else:
						return self.rejectNoAuth()

		# Overridables
		# ------------
				
		def rejectNoAuth(self):
				return http.Response(
						403, stream=cjson.encode({'err':{'msg':'Not authentificated'}})
						)

		def authenticate(self, request):
				return True

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

		def getState(self):
				return {
						'players': [dict(id = p.id, name = p.name) 
												for p in Client.table.values()
												if isinstance(p, Player)]
						}

		def handleRmPlayer(self, who, **k):
				del Client.table[who]

		def handleAddPlayer(self, **k):
				"""	 It's a message for remote, actual adding is already done.
				"""
				pass

		def handleChat(self, **k):
				""" Remote is free to store talk history, and we won't.
				"""
				pass


class Player(Client):
		"""	 This objects getState() and message queue are the points
		where remote clients get their data.
		The player objects hosts different games of differtent
		classes. Each game must be a JSON-serializable object,
		that can be controlled by JSON-serializable messages.
		"""
		
		@classmethod
		def getByName(cls, name):
				for cl in Client.table.values():
						if isinstance(cl, Player) and cl.name == name:
								return cl
								

		def __init__(self, name):
				Client.__init__(self)
				self.priv = urlsafe_b64encode(randomBinString(8))
				self.name = name
				# This will store game views, actually. But from players point of view,
				# those *are* games.
				self.games = {}
				self.invitations = []
				Client.get('players').postMessage({
								'what': 'addPlayer',
								'name': self.name,
								'id': self.id
								})

		def close(self):
				# TODO do something about my games
				Client.get('players').postMessage({
								'what': 'rmPlayer',
								'who': self.id
								})
								
				
		# Client interface
		# ------ ---------
 
		def processMessage(self, msg):
				"""	 This method is modified so it can see if message is for
				some game plugin and pass it to this plugin.
				"""
				gameId = msg.get('gameId')
				if gameId:
						self.games[gameId].processMessage(self, msg)
				else:
						Client.processMessage(self, msg)

		def getState(self):
				return {
						'name': self.name,
						'games': [v.getState() for v in self.games.values()],
						'invitations': [i.getState() for i in self.invitations]
						}

		# Output handlers.
		# ------ ---------

		def handleAddGame(self, game):
				""" Remote should reconstruct a game object from it's state
				We can't we're kind of too lame for that
				"""
				pass
				

		def handleRmGame(self, gameId):
				"""	 Removes a game view from client when a game is over
				"""
				del self.games[gameId]

		def handleInvited(self, who, gameType, **k):
				""" 
				"""
				class Invitation:
						def __init__(self, who, gameType):
								self.who = who
								self.gameType = gameType
						
						def __eq__(self, other):
								return (self.who == other.who and 
												self.gameType == other.gameType)

						def getState(self):
								return dict(who = self.who,
														gameType = self.gameType)

				inv = Invitation(who, gameType)
				if not (inv in self.invitations):
						self.invitations += [inv]

		# Interface
		# ---------

		def addGame(self, view):
				"""	 Adds a new game view to a client.
				"""
				self.games[view.id] = view
				# This messages will pass a view state to remote.
				# Remote client should construct game object from this state.
				self.postMessage(what = 'addGame',
												 game = view.getState())


# Set up a 'players' client
PlayerList()


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
				self.putChild('cometd', bus) #cometd.cometd())
				self.putChild('templates', static.File(os.path.abspath("templates")))
				self.putChild('dlib', static.File('/opt/share/www/dojod'))
				if not STATIC_PREFIX.startswith('http'): # got no static server
						mediaPath = os.path.join(os.path.dirname(world.__file__), 'm')
						self.child_m = static.File(mediaPath)
				self.child_comettest = static.File(os.path.join(os.path.dirname(world.__file__), 
																												'comettest.html'))
						

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
				if errMsgs != []:
						raise ValueError("\n".join(errMsgs))
				return {}

		def handleStartGame(self, req):
				""" When player has enough invitations, he can send a 'startGame'
				"""
				player = self.getPlayer(req)
				# check everything
				ids = getArg(req, 'with').split(',')
				for id in ids:
						return
						#start a game 

						# remove invitations
						
						
												


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

				return http.Response(200, stream=("""<html><head>
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



