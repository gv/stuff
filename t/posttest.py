from twisted.web2 import http, http_headers
from world import In, getArg
import cjson

class Entry(In):
		def render(self, req):
				m = ''
				if 'POST' == req.method:
						m = unicode(getArg(req, 'm'), 'utf-8')
						print 'Got ' + m
						print [ord(c) for c in m]
				hs = http_headers.Headers()
				hs.addRawHeader('Content-Type', 'text/html;charset=utf-8')
				return http.Response(200, headers=hs, stream="""
<html>
<body>
<div>%(str)s</div>
<div>%(js)s</div>
<form method="post">
<input type=text name="m" />
<input type="submit" />
</form>
</body>
</html>
""" % { 'str': m.encode('utf-8'), 'js': cjson.encode({'x': m})})
				
				
						

if __name__ == '__main__':
		# serve all this
		print 'Starting server...'
		from twisted.web2 import server, channel
		from twisted.internet import reactor

		site = server.Site(Entry())
		reactor.listenTCP(2222, channel.HTTPFactory(site))

		print "Running reactor..."
		reactor.run()

