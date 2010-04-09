# 
# This module contains stuff that's used by both the server and games(plugins)
#

import cjson
from twisted.web2 import resource, http
from base64 import urlsafe_b64encode, urlsafe_b64decode
from random import randint, shuffle

# utils
DEBUG = False
DEBUG = True


def cfirst(s):
		return s[0].capitalize() + s[1:]

def getArg(req, argName):
		try: return unicode(req.args[argName][0], 'utf-8')
		except: raise ValueError('"%s" parameter expected' % argName)

def randomBinString(length):
		s = ""
		while len(s) < length: s += chr(randint(0, 255))
		return s


class Resource(resource.PostableResource):
		def child_(self, request):
				if len(request.postpath) == 1:
						return self



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

#
#		 Servers (ins)
#		 ------- -----
#
#		 A server object is an "in" port of a world.
#		 "In" means it's like stdin: we read data from them.
#		 Although, we can send back a response for every received message.
#		 It does following things:
#		 - handles POST requests
#		 - returns JSON encoded responces just to know if request was accepted
#
class In(Resource):
		def _handleReq(self, request):
				# There must be a selector field
				what = getArg(request, 'what')
				handlingMethod = getattr(self, 'handle' + cfirst(what))
				#except AttributeError: raise ValueError("Don't know what '%s' is" % what)
				return handlingMethod(request)


		def render(self, request):
				""" For now let's just handle all remote messages here.
				"""
				#if not self.authenticate(req):
				#		 return cjson.encode({'err': {'msg': "Not authenticated"}})

				# maybe opposite
				if DEBUG:
						# Use twisteds exception handling mechanism
						# so we can see a stack trace
						result = self._handleReq(request)
						return http.Response(200, stream=cjson.encode(result))
				else:
						# Pass error to browser
						try:
								result = self._handleReq(request)
								return http.Response(200, stream=cjson.encode(result))
						except Exception, e:
								return http.Response(500, stream=cjson.encode({'err': {'msg': repr(e)}}))

		def getPlayer(self, req):
				""" Returns a Player objects for a player who sent this message.
				Also raises an exception in case we cant be sure. """
				who = getArg(req, 'who')
				priv = getArg(req, 'priv')
				p = self._getPlayerById(who)
				if p.priv != priv: raise ValueError('password mismatch')
				return p

		# Overridables
		# ------------
		
		def _getPlayerById(self, id):
				raise ValueError('Implement _getPlayerById !')
				

class Game(In):
		""" Every message posted to this input is a move in a game.
		Each move is initiated by a specific player.
		Parameter name for this players' id is 'who'.
		All this means we also must authenticate everyone who sends us stuff.
		"""
		table = {}
		gameType = 'override_this'

		@classmethod
		def get(cls, id):
				return Game.table.get(id)

		def __init__(self, players):
				players.sort()
				self.id = self.gameType + ''.join([p.id for p in players])
				self.players = players
				Game.table[self.id] = self

		def _getPlayerById(self, id):
				for p in self.players:
						if p.id == who: return p
				raise ValueError("No player %s in game %s" % (id, self.id))
				

		def getView(self, player):
				return player.games[self.id]

class GameView(MessageDriven):
		def __init__(self, player, game):
				self.id = game.id
				self.gameType = game.gameType
				self.init(player, game)
				# Send it away in a first message related to this game
				player.addGame(self)

		def mkState(self, **k):
				k['id'] = self.id
				k['gameType'] = self.gameType
				return k


#   Message Bus
#   ------- ---
#
# first cometd is directory name
# second cometd is filename
from cometd import cometd
import sys
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
				# cometd.BayeuxServer.Publish(bxMsg, None)
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

		# API
		# ---------

		def addGame(self, view):
				"""	 Adds a new game view to a client.
				"""
				self.games[view.id] = view
				# This messages will pass a view state to remote.
				# Remote client should construct game object from this state.
				self.postMessage(what = 'addGame',
												 game = view.getState())

		def close(self):
				# TODO do something about my games and invitations which i sent
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
						msg['player'] = self
						self.games[gameId].processMessage(msg)
						del msg['player']
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

		def handleAddGame(self, game, **k):
				""" Remote should reconstruct a game object from it's state
				We can't we're kind of too lame for that
				"""
				pass
				

		def handleRmGame(self, gameId, **k):
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

		def handleUninvited(self, who, gameType, **whatever):
				self.invitations = [inv for inv in self.invitations	if(
								inv.who != who or inv.gameType != gameType)]
						


