# This program will not use any template system, because any 
# data will be transmitted in JSON form by some kind
# of AJAX.

# static files url prefix
print "started"
STATIC_PREFIX = "http://93.92.203.153:8008/t/"

# imports
import cjson
from base64 import urlsafe_b64encode, urlsafe_b64decode
from random import randint, shuffle
from twisted.web import server, resource
from twisted.internet import reactor

print "libraries imported"

# utils

def randomBinString(length):
    s = ""
    while len(s) < length: s += chr(randint(0, 255))
    return s

def cfirst(s):
    return s[0].capitalize() + s[1:]
    

# Every client state modification is done via client.postMessage(msg) 
# method, which will call processMessage(msg), implemented by concrete
# client class, and will post message to remote client replications
# (maybe by storing it in a queue so remote can retrieve them any time later)
# 
# Every client will store it's own revision number and message buffer for
# synchronization, so for every client we will essentially need apersistent 
# TCP connection. Which means we shouldn't create many clients.
#
class MessageDriven:
    """
    A utility class to control some objects by JSON-serializable messages
    """
    def postMessage(self, msg=None, **kw):
        msg = msg or kw
        self.processMessage(msg)
        
    def processMessage(self, msg):
        what = msg['what']
        handlingMethod = getattr(self, 'handle' + cfirst(what))
        handlingMethod(msg)

    def getState(self):
        """
        If browser asks, we must give him something
        """
        raise "Implement getState!"

class Client(MessageDriven, resource.Resource):
    """
    Each clients state is stored on the server, so user 
    can close her browser and open it again and nothing wil be lost.
    
    This means a client object must be a JSON-serializable data structure,
    controllable by JSON-serializable messages.
    """
    def __init__(self, id = None):
        resource.Resource.__init__(self)
        self.id = id or urlsafe_b64encode(randomBinString(8))

    def getChild(self, path, request):
        if "" == path: return self
        return resource.Resource.getChild(self, path, request)


    def render_GET(self, request):
        return cjson.encode(self.getState())
        



class Player(Client):
    """
    This objects getState() and message queue are the points
    where remote clients get their data.
    The player objects hosts different games of differtent
    classes. Each game must be a JSON-serializable object,
    that can be controlled by JSON-serializable messages.
    """

    def __init__(self, name):
        Client.__init__(self)
        self.name = name
        # This will store game views, actually. But from players point of view,
        # those *are * games.
        self.games = {}
        self.invitations = []

    def processMessage(self, msg):
        """
        This method is modified so it can see if message is for
        some game plugin and pass it to this plugin.
        """
        gameId = msg.get('game')
        if gameId:
            self.games[gameId].processMessage(msg)
        else:
            client.processMessage(self, msg)

    def addGame(self, view):
        """
        Adds a new game view to a client.
        """
        self.games[view.id] = view
        # This messages will pass a view state to remote.
        # Remote client should construct game object from this state.
        self.postMessage(what = 'addGame',
                         game = view.getState())

    def handleAddGame(self, view):
        pass
        

    def handleRmGame(self, gameId):
        """
        Removes a game view from client when a game is over
        """
        del self.views[gameId]

    def getState(self):
        return {
            'name': self.name,
            'games': [v.getState() for v in self.games.values()],
            'invitations': invitations
            }

    
# The players list will be accessible at http://server/clients

class ClientMgr(Client):
    """ This object stores our client table for us,
    so we can reference client by an id anywhere.
    
    XXX Yet we don't. Why is that?
    
    Another thing it does is providing a client list 
    (and client list related messages to remotes)
    """
    def __init__(self):
        Client.__init__(self, 'list')
        self.clients = {}

    def get(self, id):
        if 'list' == id:
            return self
        return self.clients.get(id)

    def handleAddPlayer(self, msg):
        """
        It's a message for remote, actual adding is already done.
        """
        pass

    def handleChat(self, msg):
        """
        That will just go to message queue or UDP
        """
        pass
                         
                         
    def addPlayer(self, client):
        self.clients[client.id] = client
        self.postMessage({
                'what': 'addPlayer',
                'name': client.name,
                'id': client.id
                })
        
    def getState(self):
        return {
            'players': [dict(id = p.id, name = p.name) 
                      for p in self.clients.values()]
            }


class Server(resource.Resource):
    """ Server object is a resource, to which remotes can post
    their messages and get JSON-encoded responses.
    A server does not require a persistent connection, so we can 
    add many servers for games of any kinds. They just have to be 
    available through getChild() call.
    Server is not required to have a state.
    """
    
    def getChild(self, path, request):
        if "" == path: return self
        return resource.Resource.getChild(self, path, request)

    def render_POST(self, request):
        """ For now let's just handle all remote messages here.
        """
        # There must be a selector field
        try: what = request.args['what'][0]
        except: return cjson.encode(
            {'err': 
             {'msg': 'Posted message must have selector field'}
             }
            )
        
        try: handlingMethod = getattr(self, 'handle' + cfirst(what))
        except: return cjson.encode({'err': {'msg': "Don't know what '%s' is" % what}})

        result = handlingMethod(request)
        return cjson.encode(result)

class GameMgr(resource.Resource):
    """ An utility resource to receive post requests to 
    http://server/games/34lj2345h4l urls
    """
    def __init__(self):
        resource.Resource.__init__(self)
        self.games = {}

    def getChild(self, path, request):
        return self.games[path]
        
class Entry(Server):
    """ This is a main resouce, which dispatches requests
    for other resources.
    """

    def __init__(self):
        resource.Resource.__init__(self)
        self.clients = ClientMgr()
        self.putChild('clients', self.clients)
        self.games = GameMgr()
        self.putChild('games', self.games)

    def render_GET(self, request):
        """ Here we serve a single html document which is an entry point
        to our client. 
        The client is some javascript files served statically
        from static server.
        """
        worldPrefix = 'http://%s:2222/' % request.getRequestHostname()
        return """<html>
<head>
<link rel=stylesheet href="%(static)scss.css" />
<script src="%(static)sdefer.js"></script>
<script src="%(static)sclient.js"></script>
</head>
<body>
<center>
<div id="worldBrowser"></div>
</center>

<script>
browse("%(world)s", "worldBrowser");
</script>
</html>
""" % { 'static' : STATIC_PREFIX, 'world': worldPrefix }

    def handleNeedClient(self, request):
        # player must have a name, so we can show him in a list
        try: name = request.args['name']
        except: return {'err': {'msg': "needClient: say your name!"}}
        client = Player(name)
        self.clients.addPlayer(client)
        return dict(id = client.id)

    def handleChat(self, request):
        pass



#class Card:
#    """
#    These are cards we will play some day
#    """
#    def __init__(self, suit, rank):
#        self.suit = suit
#        self.rank = rank

def Card(suit, rank):
    """
    Completely serializable
    """
    return suit + rank

def suit(card):
    return card - card % 20

def rank(card):
    return card % 20

SPADES = 40
CLUBS = 60
DIAMONDS = 80
HEARTS = 100

NINE = 0
JACK = 2
QUEEN = 3
KING = 4
TEN = 10
ACE = 11

deck = [Card(s, r) for 
        s in [SPADES, CLUBS, DIAMONDS, HEARTS] for
        r in [NINE, JACK, QUEEN, KING, TEN, ACE]]

class Round:
    """ Round object holds state of a single round of a thousand game.
    It's actually a set of methods which control Game object.
    """
    def __init__(self, game):
        self.currentIndex = 0
        self.trump = None
        self.declarer = None

        # XXX do we need this link?
        self.views = []
        for index, player in enumerate(game.players):
            self.views += [self.View(self, game, index)]
            
        # deal
        shuffle(deck)
        
        for player in game.players:
            player.postMessage(gameId = game.id,
                               what = 'cards',
                               cards = deck[index*7 : index*7+7])
        self.hidden = deck[21:]

        # first bid
        self.bid(game.players[0], 100)

    def bid(self, game, bidder, amount):
        if game.players[self.currentIndex] != bidder:
            raise ValueError("Is't not %s's turn to bid!" % bidder.id)

        # check this player's view to see if we can afford this amount

        # post messages to views
        

        


    class View:#(MessageDriven):
        """ View is a part of round which can be browsed by user. 
        It processes messages sent to clients as a part of client state,
        but it's not revisioned or has not a  remote accessible mesage history
        """
        def __init__(self, round, game, index):
            # self.id = round.id

            # We also must see images of other players.
            # Seems right we reference them by ids here (another circular link issue)
            class PlayerView:
                def __init__(self, playerId):
                    self.id = playerId
                    self.card = None
                    self.bid = None

            self.score = 0
            self.hand = []
            self.bid = None
            self.trump = None
            self.players = [PlayerView(p.id) for p in 

            gameView.setRound(self, player)

        def getState(self):
            return dict(
                score = self.score,
                hand = self.hand,
                trump = self.trump,
                bid = self.bid,
                
                )
                

class Game(Server):
    """
    This class is a server plugin. 
    It processes commands from clients, related to any thousand game.
    """
    # XXX we should accept any command only from authenticated clients
    def __init__(self, players):
        if (len(players)) != 3:
            raise AttributeError("Can't play round with %n players" % 
                                 len(players))
        
        sort(players)
        self.players = players
        self.id = ''.join([p.id for p in players])

        # first event is "we start a game"
        # send appropriate commands by initializing a view
        self.views = [self.View(p, self) for p in players]
        self.startRound()

    def startRound(self):
        self.round = Round(self)

    def _getPlayer(self, id):
        for p in self.players:
            if p.id = who: return p
        raise AttributeError("No player %s in game %s" % (who, self.id))

    def handleBid(self, req):
        # XXX authenticate 
        bidder = self._getPlayer(who)
        return self.round.bid(self, bidder, amount)

    def handlePass(self, req):
        # XXX authenticate again
        passer = self._getPlayer(who)
        return self.round.pass_(self, passer)

    def handleMove(self, req):
        # XXX authenticate on plugin or class level
        mover = self._getPlayer(who)
        return self.round.move(self, mover, card, announce)
        

    class View: #(MessageDriven):
        """ The game view maintains game data, specific for a player. 

        This class is a Player plugin.
        This object sits inside clients state and processes client messages, such as
        "this player has received cards" or "this player sees a card is put into a trick".
        """
        def __init__(self, player, game):
            # I think this object should not contain 
            # references back to a player
            # self.player = player
            self.id = game.id
            self.score = 0
            player.addGame(self)

        def getState(self):
            """
            This data will go to a game browser
            """
            return dict(
                id = self.id,
                type = 'thousand',
                score = self.score,
                round = self.round.getState()
                )

        def setRound(self, round, player):
            # passing an object
            self.round = round
            player.postMessage(what = 'newRound',
                               gameId = self.id,
                               round = round.getState())
            
        def handleNewRound(self, msg, **k):
            " Already done "
            pass

        def handleCards(self, msg):
            self.round.cards += msg['cards']
        
        

# serve all this
site = server.Site(Entry())
reactor.listenTCP(2222, site)
print "running reactor"
reactor.run()



