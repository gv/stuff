# Here we serve a single html document which is an entry point
# to our client. 
# The client is some javascript files served statically
# from static server.

# This program will not use any template system, because any 
# data will be transmitted in JSON form by some kind
# of AJAX.

# static files url prefix
print "started"
STATIC_PREFIX = "http://93.92.203.153:8008/t/"
WORLD_PREFIX = "http://93.92.203.153:2222/"

# imports
import cjson
from base64 import urlsafe_b64encode, urlsafe_b64decode
from random import randint
from twisted.web import server, resource
from twisted.internet import reactor

print "libraries imported"

def randomBinString(length):
    s = ""
    while len(s) < length: s += chr(randint(0, 255))


# Every client state modification is done via client.postMessage(msg) 
# method, which will call processMessage(msg), implemented by concrete
# client class, and will post message to remote client replications
# (maybe by storing it in a queue so remote can retrieve them any time later)
# XXX make it JSON serializable
class Message:
    """
    It seems that Message should be much of a POD structure, cause if we 
    add any virtual methods, we can't pass them to remote, so we will
    still need some king of selector field.
    """
    def __init__(self, what, **opts):
        self.what = what

    def serialize(self):
        pass

class Client(resource.Resource):
    """
    Each clients state is stored on the server, so user 
    can close her browser and open it again and nothing wil be lost.
    """
    def __init__(self, id = None):
        resource.Resource.__init__(self)
        self.id = id or urlsafe_b64encode(randomBinString(8))

    def getChild(self, path, request):
        if "" == path: return self
        return resource.Resource.getChild(self, path, request)


    def render_GET(self, request):
        return cjson.encode(self.getState())
        
    def postMessage(self, msg):
        self.processMessage(msg)
        

    def processMessage(self, msg):
        what = msg['what']
        handlingMethod = getattr(self, 'handle' + what.capitalize())
        handlingMethod(msg)

    def getState(self):
        """
        If browser asks, we must give him something
        """
        raise "Implement getState!"



class Player(Client):
    def __init__(self, name):
        Client.__init__(self)
        self.name = name


# The players list will be accessible at http://server/clients

class ClientMgr(Client):
    def __init__(self):
        Client.__init__(self, 'list')
        # indexed by ids
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

    
    
class Entry(resource.Resource):
    def __init__(self):
        resource.Resource.__init__(self)
        self.clients = ClientMgr()
        self.putChild('clients', self.clients)

    def getChild(self, path, request):
        if "" == path: return self
        return resource.Resource.getChild(self, path, request)

    # Reference javascripts here.
    def render_GET(self, request):
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
""" % { 'static' : STATIC_PREFIX, 'world': WORLD_PREFIX }

    def render_POST(self, request):
        """
        For now let's just handle all remote messages here.
        """
        # There must be a selector field
        try: what = request.args['what'][0]
        except: return cjson.encode({'err': {'msg': 'Posted message must have selector field'}})
        
        try: handlingMethod = getattr(self, 'handle' + what.capitalize())
        except: return cjson.encode({'err': {'msg': "Don't know what '%s' is" % what}})

        result = handlingMethod(request)
        return cjson.encode(result)
                
        
    def handleNeedClient(self, request):
        # player must have a name, so we can show him in a list
        try: name = request.args['name']
        except: return {'err': {'msg': "needClient: say your name!"}}
        client = Player(name)
        self.client
        
        
        

# serve all this
site = server.Site(Entry())
reactor.listenTCP(2222, site)
print "running reactor"
reactor.run()



