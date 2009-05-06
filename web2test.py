# don't segfault
from twisted.web2 import resource, http

PORT_NUMBER = 2222

#class Entry(resource.PostableResource):
		#def child_(self, request):
		#		if len(request.postpath) == 1:
		#				return self

		#def render(self, req):
		#		return http.Response(200, stream="ok")

from twisted.web2 import server, channel
from twisted.internet import reactor

site = server.Site(resource.PostableResource()) #Entry())
reactor.listenTCP(PORT_NUMBER, channel.HTTPFactory(site))

print "Running reactor..."
reactor.run()
