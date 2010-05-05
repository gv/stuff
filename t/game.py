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

print 'Starting server...'

root = resource.Resource()
root.putChild("", static.File("index.html"))
root.putChild("m", static.File("m"))

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
				s['id'] = self.id
				s['version'] = self.version
				remote.transport.write(cjson.encode(s))
				
		def postMessage(self, m=None, **kw):
				m = m or kw
				# Let remotes process, then process locally
				m['version'] = self.version
				m['id'] = self.id
				for r in self.remotes:
						#if r.lost:
						#		self.remotes.discard(r)
						#else:
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
				self.chatHistory = []

		def processMessage(self, m):
				what = m['what']
				if what == "iSay":
						self.chatHistory = (self.chatHistory + [m])[-10:]
				elif what == "newPerson":
						pass # list is updated already here

		def getState(self):
				people = [{
								"id": p.id,
								"name": p.name
								} for p in outs.values() 
									if isinstance(p, Person)]
				return {
						'chat': self.chatHistory,
						'people': people
						}

room = Room()
room.postMessage(what="iSay", speaker="Server", speech="Server started...");


def randomString():
		s = ""
		while len(s) < 16:
				s += chr(randint(48, 123))
		return s
		

class Person(Out):
		def __init__(self):
				id = randomString()
				Out.__init__(self, id)
				self.name = ""
				room.postMessage(what="newPerson", newPersonId=self.id)

		def processMessage(self, m):
				what = m['what']
				if what == 'newName':
						self.name = m['name']
				

class BrowserConnection(websocket.WebSocketHandler):
		lost = False
		owner = None
		
		def __init__(self, transport):
				websocket.WebSocketHandler.__init__(self, transport)
				
		def connectionOpened(self):
				pass

		def frameReceived(self, frame):
				m = cjson.decode(frame)
				print m
				what = m['what']
				if what == 'newHere':
						self.disown()
						p = Person()
						room.connect(self)
						p.connect(self)
						self.owner = p
				elif what == 'iSay':
						room.postMessage(what='iSay', 
														 speaker=self.owner.id, speech=m['speech'])
								

		def connectionLost(self, reason):
				print "lost: ", reason
				self.lost = True
				self.disown()

		def disown(self):
				if self.owner:
						self.owner.remotes.discard(self)
						

site = websocket.WebSocketSite(root)
site.addHandler("/s", BrowserConnection)
reactor.listenTCP(8888, site)
#reactor.listenSSL(443, site)

print "Running reactor..."
reactor.run()
				
