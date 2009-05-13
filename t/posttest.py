from twisted.web2 import http
from world import In, getArg

class Entry(In):
		def render(self, req):
				m = ''
				if 'POST' == req.method:
						m = getArg(req, 'm')
				return http.Response(200, stream="""
<html>
<body>
%s
<form>
<input type=text name="m" />
<input type="submit" />
</form>
</body>
</html>
""" % m)
				
				
						

if __name__ == '__main__':
		# serve all this
		print 'Starting server...'
		from twisted.web2 import server, channel
		from twisted.internet import reactor

		site = server.Site(Entry())
		reactor.listenTCP(PORT_NUMBER, channel.HTTPFactory(site))

		print "Running reactor..."
		reactor.run()

