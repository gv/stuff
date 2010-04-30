import sys
sys.path = ['ws', '/opt/ws'] + sys.path

from twisted.web.server import Site
from twisted.web.resource import Resource
from twisted.internet import reactor
from twisted.web.static import File, Data

from twisted.web import websocket

print 'Starting server...'
root = Resource()
root.putChild("", File("wsdemo.html"))
#root.putChild("qwe", Data("Just some text", "text"))

class WsHandler(websocket.WebSocketHandler):
		def __init__(self, transport):
				websocket.WebSocketHandler.__init__(self, transport)
				print 456

		def connectionOpened(self):
				self.transport.write("yeah well")

site = websocket.WebSocketSite(root)
site.addHandler("/clk", WsHandler)
reactor.listenTCP(8888, site)
#reactor.listenSSL(443, site)

print "Running reactor..."
reactor.run()
				
