import sys
sys.path = ['ws', '/opt/ws'] + sys.path

from twisted.web.server import Site
from twisted.web.resource import Resource
from twisted.internet import reactor
from twisted.web.static import File, Data

print 'Starting server...'
root = Resource()
root.putChild("", File("wsdemo.html"))
root.putChild("clk", Data("Just some text", "text"))

site = Site(root)
reactor.listenTCP(8888, site)
reactor.listenSSL(443, site)

print "Running reactor..."
reactor.run()
				
