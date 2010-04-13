from google.appengine.ext import db

class Offer(db.Model):
		url = db.StringProperty()
		title = db.StringProperty()

class Price(db.Model):
		value = db.IntegerProperty()
		date = db.DateTimeProperty()
		offer = db.ReferenceProperty(Offer)
		unit = db.StringProperty()

