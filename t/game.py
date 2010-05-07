import sys
sys.path = ['ws', '/opt/ws'] + sys.path

print "Loading libraries..."

from random import randint

from twisted.web import server, resource, static, websocket
from twisted.internet import reactor
try:
		import cjson
except ImportError:
		import simplejson as cjson
		cjson.encode = cjson.dumps
		cjson.decode = cjson.loads

#
#    MESSAGE EXCHANGE
#    ``````` ````````
#    Simple rules:
#    'dn' property always refers to client id, to whom the message is routed to
#

outs = {}

class Out:
		def __init__(self, id):
				self.id = id
				self.remotes = set()
				self.version = 0
				outs[id] = self

		def connect(self, remote):
				self.remotes.add(remote)
				s = self.getState()
				s['what'] = 'state'
				s['dn'] = self.id
				s['version'] = self.version
				remote.transport.write(cjson.encode(s))
				
		def postMessage(self, m=None, **kw):
				m = m or kw
				# Let remotes process, then process locally
				m['version'] = self.version
				m['dn'] = self.id
				for r in self.remotes:
						r.transport.write(cjson.encode(m))
				self.version = self.version + 1
				self.processMessage(m)

		#
		#  OVERRIDE THESE
		#

		def processMessage(self, m):
				pass

		def getState(self):
				return {}

class Room(Out):
		def __init__(self):
				Out.__init__(self, "room")
				self.conversation = []

		def processMessage(self, m):
				what = m['what']
				if what == "createComment":
						self.conversation = (self.conversation + [m])[-10:]
				elif what == "createPerson":
						pass # list is updated already here
				elif what == "updatePerson":
						person = outs[m['id']]
						person.name = m['name']
						

		def getState(self):
				people = [{
								"id": p.id,
								"name": p.name
								} for p in outs.values() 
									if isinstance(p, Person)]
				return {
						'conversation': self.conversation,
						'people': people
						}

room = Room()
room.postMessage(what="createComment", author="Server", text="Started...");

import random

def randomString():
		return ''.join(random.choice("poiuytrewqasdfghjklmnbvcxz") for _ in range(16))
		

class Person(Out):
		def __init__(self):
				id = randomString()
				Out.__init__(self, id)
				self.name = ""
				self.password = randomString()
				self.invitations = []
				room.postMessage(what="createPerson", id=self.id)

		def processMessage(self, m):
				what = m['what']
				if what == 'updatePerson':
						self.name = m['name']
				elif what == 'createInvitation':
						self.invitations.append(m)

		def getState(self):
				return {'invitations': self.invitations}
				

class BrowserConnection(websocket.WebSocketHandler):
		#lost = False
		owner = None
		
		def __init__(self, transport):
				websocket.WebSocketHandler.__init__(self, transport)
				
		def connectionOpened(self):
				pass

		def send(self, m=None, **kw):
				m = m or kw
				self.transport.write(cjson.encode(m))

		def frameReceived(self, frame):
				try:
						m = cjson.decode(frame)
						print m

						gameId = m.get('game')
						if gameId:
								game = self.owner.games[gameId]
								game.processMessage(m)

						what = m['what']
						if what in ['createPerson', 'openPerson']:
								self.disown()
								p = None
								if what == 'openPerson':
										p = outs.get(m['id'])
										if p and p.password != m['password']:
												raise ValueError("Bad password")
								
								if not p:
										p = Person()
								self.send(what="person", id=p.id, password=p.password)
								room.connect(self)
								p.connect(self)
								self.owner = p
						elif what == 'createComment':
								m['author'] = self.owner.id
								room.postMessage(m)
						elif what == 'updatePerson':
								m['id'] = self.owner.id
								room.postMessage(m)
						elif what == 'createInvitation':
								target = m['target']
								if(target == self.owner.id):
										raise ValueError("It's sort of depressing to invite yourself")
								target = outs[target]
								m['src'] = self.owner.id
								target.postMessage(m)
						elif what == "createGame":
								players = m['players']
								for p in players:
										if not self.owner.hasInvitation(p):
												raise ValueError("Invitation from %s not found!" % p)
								
								players.append(self.owner.id)
								Game(players)

				except Exception, e:
						errMsg = "ERROR: %s" % str(e)
						self.transport.write(errMsg)
								

		def connectionLost(self, reason):
				#print "lost: ", reason
				self.lost = True
				self.disown()

		def disown(self):
				if self.owner:
						self.owner.remotes.discard(self)
						
def getPerson(id):
		p = outs.get(id)
		if p and isinstance(p, Person):
				return p
		raise ValueError("No person %s" % id)



games = {}

class Game:
		def __init__(self, playerIds):
				self.id = randomString()
				self.players = [outs[p] for p in playerIds]
				games[self.id] = self

				self.turnIndex = 0
				self.pieces = []

				for p in self.players:
						p.games[self.id] = self
						p.postMessage(what="createGame", id=self.id)
		
		def getState(self):
				return {
						'id': self.id, 
						'whoseTurn': self.players[self.turnIndex].id,
						'pieces': self.pieces
						}

		def processMessage(m):
				what = m['what']
				if what == 'createPiece':
						self.pieces.append(m)
						# TODO check for victory
						for p in self.players:
								p.postMessage(m)
				






if __name__ == "__main__":
		print 'Starting server...'

		root = resource.Resource()
		root.putChild("", static.File("index.html"))
		root.putChild("m", static.File("m"))

		site = websocket.WebSocketSite(root)
		site.addHandler("/s", BrowserConnection)
		reactor.listenTCP(8888, site)

		try:
				from twisted.internet import ssl
				sslContext = ssl.DefaultOpenSSLContextFactory(
						'privkey.pem', 
						'cacert.pem',
						)
				reactor.listenSSL(443, site, sslContext)
		except ImportError, e:
				print "No SSL, ", e

				print "Running reactor..."
				reactor.run()
				
