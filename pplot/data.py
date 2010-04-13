import time

from google.appengine.ext import webapp
from google.appengine.ext.webapp import template
from google.appengine.ext.webapp import util
from google.appengine.ext import db
from django.utils import simplejson

import pplot


class MainPage(webapp.RequestHandler):
    """ Renders the main template."""
    def get(self):
        template_values = { 'title':'AJAX Add (via GET)', }
        path = os.path.join(os.path.dirname(__file__), "index.html")
        self.response.out.write(template.render(path, template_values))

class RPCHandler(webapp.RequestHandler):
    """ Will handle the RPC requests."""
    def get(self):
				q = db.GqlQuery("SELECT * FROM Price")
				#rs = q.fetch(10)
				lines = {}
				for p in q:
						offerKey = str(p.offer.key())
						if not offerKey in lines:
								lines[offerKey] = {
										'points': [],
										'title': p.offer.title,
										'url': p.offer.url
										}
						lines[offerKey]['points'].append({
										'value': p.value, 
										'unit': p.unit,
										'date': time.mktime(p.date.timetuple())
										})
				r = {'lines': lines}					
				self.response.out.write(
						simplejson.dumps(r, sort_keys=True, indent=4, ensure_ascii=False))

def main():
    app = webapp.WSGIApplication([
        ('/', MainPage),
        ('/data', RPCHandler),
        ], debug=True)
    util.run_wsgi_app(app)

if __name__ == '__main__':
    main()
