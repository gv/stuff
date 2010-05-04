import sys
sys.path = ['ws', '/opt/ws'] + sys.path

print "Loading libraries..."

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

		def connect(self, remote, v):
				self.remotes.add(remote)
				
				
		def postMessage(self, m=None, **kw):
				m = m or kw
				# Let remotes process, then process locally
				m['version'] = self.version
				m['id'] = self.id
				for r in self.remotes:
						r.write(cjson.encode(m))
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
				if what == "chat":
						self.chatHistory = (self.chatHistory + [m])[-10:]

		def getState(self):
				return {
						'chat': self.chatHistory,
						'people': []
						}


def randomString():
		s = ""
		while len(s) < 16:
				s += chr(randint(32, 127))
		return s
		

class Person(Out):
		def __init__(self):
				id = randomString()
				Out.__init__(self, id)
				self.name = ""

		def processMessage(self, m):
				what = m['what']
				if what == 'newName':
						self.name = m['name']
				

class BrowserConnection(websocket.WebSocketHandler):
		lost = False
		
		def __init__(self, transport):
				websocket.WebSocketHandler.__init__(self, transport)
				

		def connectionOpened(self):
				#self.transport.write("yeah well")
				pass

		def frameReceived(self, frame):
				m = cjson.decode(frame)
				print m
				what = m['what']
				if what == 'open':
						pass
				else:
						id = m['id']
						out = outs[id]
						if what == 'reload':
								s = out.getState()
								s['version'] = out.version
								s['what'] = 'state'
								s['id'] = out.id
								self.transport.write(cjson.encode(s))
						elif what == '':
								

		def connectionLost(self, reason):
				print "lost: ", reason
				self.lost = True

site = websocket.WebSocketSite(root)
site.addHandler("/s", BrowserConnection)
reactor.listenTCP(8888, site)
#reactor.listenSSL(443, site)

print "Running reactor..."
reactor.run()
				
