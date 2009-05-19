# 
# This is a thousand game implementation for our "World" framework
#

from random import shuffle
import world
from cards import *

deck = [Card(s, r) for 
				s in [SPADES, CLUBS, DIAMONDS, HEARTS] for
				r in [9, JACK, QUEEN, KING, 10, ACE]]

class Round:
		""" Round object holds state of a single round of a thousand game.
		It's actually a set of methods which control Game object.
		"""

		PASSED = -1

		def __init__(self, game):
				self.currentIndex = 0
				self.trump = None
				self.declarer = None

				# XXX Do we need this link?
				self.views = []
				for index, player in enumerate(game.players):
						self.views += [self.View(self, game, index)]
						
				# Deal
				shuffle(deck)
				
				for player in game.players:
						player.postMessage(gameId = game.id,
															 what = 'cards',
															 cards = deck[index*7 : index*7+7])
				self.hidden = deck[21:]

				# First bid
				self.bid(game, game.players[0], 100)

		def _checkCurrent(self, game, player):
				if game.players[self.currentIndex] != player:
						raise ValueError("Is't not %s's turn to do what he does!" % player.id)

		def _checkDeclaration(self, game, mustHave = True):
				if mustHave and not self.declarer:
						raise ValueError("This can't be done until nobody declared!")
				elif self.declarer and not mustHave:
						raise ValueError("This can't be done when %s already has declared!" % self.declarer)
						

		def bid(self, game, bidder, amount):
				self._checkCurrent(game, bidder)

				# Check this player's view to see if we can afford this amount

				# Post messages to views
				for p in game.players:
						p.postMessage(what = 'bid',
													gameId = game.id,
													who = bidder.id,
													amount = amount)

		def pass_(self, game, passer):
				self._checkCurrent(game, passer)

				for p in game.players:
						p.postMessage(what = 'pass',
													gameId = game.id,
													who = passer.id)
				
				# Check if we have a declarer, because we need to send 
				# her the hidden cards
				bidderIndexes = [i for i,v in enumerate(game.views) 
												 if v.round.bid != Round.PASSED]
				if len(bidderIndexes) != 1:
						return
				
				# Yep we do
				self.declarer = game.players[bidderIndexes[0]]
				self.declarer.postMessage(what = 'cards',
																	gameId = game.id,
																	cards	 = self.hidden)

		def move(self, game, mover, card, announce=False):
				pass
				
				
				
						
						

				

				

				


		class View:#(MessageDriven):
				""" Round view is a part of round which can be browsed by user. 
				"""
				def __init__(self, round, game, index):
						# We also must see images of other players.
						# Seems right we reference them by ids here 
						# (another circular link issue)
						# Second option is using weakrefs

						player = game.players[index]
						gameView = game.getView(player)

						class Sector:
								def __init__(self, player):
										self.id = player.id
										self.card = None
										self.bid = None

						self.score = 0
						self.hand = []
						self.bid = None
						self.trump = None
						self.declarerId = None
						self.table = [Sector(p) for p in game.players]
														
						gameView.setRound(self, player)

				def getState(self):
						return dict(
								score = self.score,
								hand = self.hand,
								trump = self.trump,
								bid = self.bid,
								table = [dict(id = pv.id,
															bid = pv.bid,
															card = pv.card) 
												 for pv in self.table]
								)
								

class Game(world.Game):
		"""	 This class is a server plugin. 
		It processes commands from clients, related to any 3 player thousand game.
		"""

		gameType = 'thousand'

		def __init__(self, players):
				if (len(players)) != 3:
						raise AttributeError("Can't play round with %n players" % 
																 len(players))
				
				world.Game.__init__(self, players)

				# first event is "we start a game"
				# send appropriate commands by initializing a view
				self.views = [self.View(p, self) for p in players]
				self.startRound()

		def startRound(self):
				self.round = Round(self)

		# Browser request handlers. 

		def handleBid(self, req):
				bidder = self.getPlayer(req)
				self.round.bid(self, bidder, amount)
				return {}

		def handlePass(self, req):
				passer = self.getPlayer(req)
				self.round.pass_(self, passer)
				return {}

		def handleMove(self, req):
				mover = self.getPlayer(req)
				self.round.move(self, mover, card, announce)
				return {}
				

		class View(world.GameView):
				""" The game view maintains game data, specific for a player. 

				This class is a Player plugin.
				This object sits inside clients state and processes client messages, such as
				"this player has received cards" or "this player sees a card is put into a trick".
				"""
				def init(self, player, game):
						# State we initialize here will be sent to browser
						self.score = 0
						self.round = None

				def getState(self):
						"""	 
						This data will go to a game browser.

						We actually must keep all the data in this object and not ever ask 'Game' 
						because then it would be impossible to track changes and assign rev numbers.
						"""
						return self.mkState(
								score = self.score,
								round = self.round and self.round.getState()
								)

				def setRound(self, round, player):
						# passing an object
						self.round = round
						player.postMessage(what = 'newRound',
															 gameId = self.id,
															 round = round.getState())
						
				def handleNewRound(self, **something):
						" Already done "
						pass

				def handleCards(self, cards, player, **kw):
						self.round.hand += cards

				def handleBid(self, who, amount, **k):
						pass

				def handlePass(self, who, **k):
						pass

#		API
#		---

