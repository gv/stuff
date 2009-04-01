# This program will not use any template system, because any 
# data will be transmitted in JSON form by some kind
# of AJAX.

# static files url prefix
print "started"
STATIC_COMMON_PREFIX = "http://93.92.203.153:8008/"
STATIC_PREFIX = STATIC_COMMON_PREFIX + "t/"

# imports
import cjson
from base64 import urlsafe_b64encode, urlsafe_b64decode
from random import randint, shuffle
from twisted.web import server, resource
from twisted.internet import reactor
from cometd import cometd

print "libraries imported"

# utils

DEBUG = False
#DEBUG = True

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
# synchronization.
#
# Still since we can't afford more than one persistent connection, we will need
# some kind of multiplexor (cometd comes to mind)
#
class MessageDriven:
    """  A utility base class to control some objects by JSON-serializable messages
    """
    def postMessage(self, msg=None, **kw):
        msg = msg or kw
        self.processMessage(msg)
        
    def processMessage(self, msg):
        what = msg['what']
        handlingMethod = getattr(self, 'handle' + cfirst(what))
        handlingMethod(**msg)

    def getState(self):
        """  If browser asks, we must give him something
        """
        raise "Implement getState!"



class Client(MessageDriven, resource.Resource):
    """ Each clients state is stored on the server, so user 
    can close her browser and open it again and nothing wil be lost.
    
    This means a client object must be a JSON-serializable data structure,
    controllable by JSON-serializable messages.
    """

    table = {}

    def get(id):
        return Client.table.get(id)

    def __init__(self, id = None):
        resource.Resource.__init__(self)
        self.id = id or urlsafe_b64encode(randomBinString(8))
        self.revision = 0
        Client.table[self.id] = self

    def postMessage(self, msg=None, **kw):
        msg = msg or kw
        msg['revision'] = self.revision + 1
        self.processMessage(msg)
        self.revision = msg['revision'];
        

    # Resource interface

    def getChild(self, path, request):
        if "" == path: return self
        return resource.Resource.getChild(self, path, request)

    def render_GET(self, request):
        if self.authenticate(request):
            return cjson.encode(self.getState())
        else:
            return self.rejectNoAuth()

    # Overridables
        
    def rejectNoAuth(self):
        return cjson.encode({'err':{'msg':'Not authentificated'}})

    def authenticate(self, request):
        return True



class Player(Client):
    """  This objects getState() and message queue are the points
    where remote clients get their data.
    The player objects hosts different games of differtent
    classes. Each game must be a JSON-serializable object,
    that can be controlled by JSON-serializable messages.
    """

    def __init__(self, name):
        Client.__init__(self)
        self.priv = urlsafe_b64encode(randomBinString(8))
        self.name = name
        # This will store game views, actually. But from players point of view,
        # those *are* games.
        self.games = {}
        self.invitations = []

    def processMessage(self, msg):
        """  This method is modified so it can see if message is for
        some game plugin and pass it to this plugin.
        """
        gameId = msg.get('gameId')
        if gameId:
            self.games[gameId].processMessage(self, msg)
        else:
            Client.processMessage(self, msg)

    def addGame(self, view):
        """  Adds a new game view to a client.
        """
        self.games[view.id] = view
        # This messages will pass a view state to remote.
        # Remote client should construct game object from this state.
        self.postMessage(what = 'addGame',
                         game = view.getState())

    def handleAddGame(self, game):
        """ Remote should reconstruct a game object from it's state
        We can't we're kind of too lame for that
        """
        pass
        

    def handleRmGame(self, gameId):
        """  Removes a game view from client when a game is over
        """
        del self.games[gameId]

    def handleInvited(self, who, whatGame, **k):
        """ 
        """
        class Invitation:
            def __init__(self, who, what):
                self.who = who
                self.what = what
            
            def __eq__(self, other):
                return (self.who == other.who and 
                        self.what == other.what)

            def getState(self):
                return dict(who = self.who,
                            whatGame = self.what)

        inv = Invitation(who, whatGame)
        if not (inv in self.invitations):
            self.invitations += [inv]

    def getState(self):
        return {
            'name': self.name,
            'games': [v.getState() for v in self.games.values()],
            'invitations': [i.getState() for i in self.invitations]
            }

    
# The players list will be accessible at http://hostname/clients

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

    def getChild(self, path, req):
        if "" == path: return self
        return self.clients.get(path)

    def handleAddPlayer(self, **k):
        """  It's a message for remote, actual adding is already done.
        """
        pass

    def handleChat(self, **k):
        """  That will just go to message queue or network
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

        # maybe opposite
        if DEBUG:
            try:
                result = handlingMethod(request)
                return cjson.encode(result)
            except Exception, e:
                return cjson.encode({'err': {'msg': repr(e)}})
        else:
            result = handlingMethod(request)
            return cjson.encode(result)


#            
#     Entry point
#     ----- -----
#
#     This is a root resource. It routes GET and POST requests to following resources:
#
#     /         ->   Client manager input. Post to it if you need a client id.
#                    Also, post to it if you have a client id but don't need it anymore.
#                    Also if you GET it, you will retrieve browser client code.
#     /clients  ->   Client manager. Reports clients' states and player list.
#     /cometd   ->   Async message server.
#     /games    ->   Game Manager. Routes POST requests from remotes to Game objects,
#                    which handle them according to specific game rules.
#
class Entry(Server):
    """ 
    """

    def __init__(self):
        resource.Resource.__init__(self)
        self.clients = ClientMgr()
        self.putChild('clients', self.clients)

        class GameMgr(resource.Resource):
            """ An utility resource to receive post requests to 
            http://server/games/34lj2345h4l urls
            """
            def __init__(self):
                resource.Resource.__init__(self)
                self.games = {}

            def getChild(self, path, request):
                return self.games[path]
        
        self.games = GameMgr()
        self.putChild('games', self.games)

    def render_GET(self, request):
        """ Here we serve a single html document which is an entry point
        to our client. 
        The client is some javascript files served statically
        from static server.
        """
        worldPrefix = 'http://%s:2222/' % request.getRequestHostname()
        return ("""<html>
<head>
<link rel=stylesheet href="%(static)scss.css" />
<link rel=stylesheet href="%(dlib)sdijit/themes/nihilo/nihilo.css" />""" + 
#<script src="%(static)sdefer.js"></script>
#<script src="%(static)sclient.js"></script>
"""<script>
djConfig = { isDebug: true } 
</script>
<SCRIPT TYPE="text/javascript" SRC="%(dlib)sdojo/dojo.xd.js"></SCRIPT>
<SCRIPT>
dojo.registerModulePath('anxiety', '%(static)s');
</SCRIPT>
<script src="%(static)sclient2.js"></script>
</head>
<body class="nihilo">
<div id="worldBrowser"></div>

<script>
browse("%(world)s", "worldBrowser");
</script>
</html>
""") % { 'static' : STATIC_PREFIX, 'world': worldPrefix, 'dlib': STATIC_COMMON_PREFIX + 'dojod/'}
    # fucking <script>s everywhere

    # Remote messages handlers

    def handleNeedClient(self, request):
        # player must have a name, so we can show him in a list
        # XXX check if name is taken
        try: name = request.args['name'][0]
        except: return {'err': {'msg': "needClient: say your name!"}}
        player = Player(name)
        # XXX that should be automatically done by a single global client manager
        self.clients.addPlayer(player)
        return dict(id = player.id, 
                    priv = player.priv, 
                    name = player.name)

    def handleChat(self, request):
        pass

    def handleInvite(self, request):
        # XXX check priv token
        ids = request.args['who'][0].split(',')
        for id in ids:
            who = self.clients.get(id)
            if not who:
                raise ValueError("No client %s" % whoId)
            who.postMessage(what = 'invited',
                            who = request.args['from'][0],
                            whatGame = request.args['whatGame'][0])
        return {}
                        



# ---------- Thousand game ---- XXX make a module ---------------------------------

def Card(suit, rank):
    """  Completely serializable
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
        self.bid(game.players[0], 100)

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
        # her cards
        bidderIndexes = [i for i,v in enumerate(game.views) 
                         if v.round.bid != Round.PASSED]
        if len(bidderIndexes) != 1:
            return
        
        # Yep we do
        self.declarer = game.players[bidderIndexes[0]]
        self.declarer.postMessage(what = 'cards',
                                  gameId = game.id,
                                  cards  = self.hidden)

    def move(self, game, mover, card, announce=False):
        pass
        
        
        
            
            

        

        

        


    class View:#(MessageDriven):
        """ Round view is a part of round which can be browsed by user. 
        It processes messages sent to clients as a part of client state,
        but it's not revisioned or has not a  remote accessible mesage history
        """
        def __init__(self, round, game, index):
            # We also must see images of other players.
            # Seems right we reference them by ids here 
            # (another circular link issue)
            # Second option is using weakrefs
            class PlayerView:
                def __init__(self, player):
                    self.id = player.id
                    self.card = None
                    self.bid = None

            self.score = 0
            self.hand = []
            self.bid = None
            self.trump = None
            self.declarerId = None
            self.table = [PlayerView(p) for p in game.players]
                            
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
                

class Game(Server):
    """  This class is a server plugin. 
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
            if p.id == id: return p
        raise AttributeError("No player %s in game %s" % (id, self.id))

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
            """  This data will go to a game browser
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
        
        

if __name__ == '__main__':
    # serve all this
    site = server.Site(Entry())
    reactor.listenTCP(2222, site)
    print "running reactor"
    reactor.run()



