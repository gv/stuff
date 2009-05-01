# 
# This module contains stuff that's used by both the server and games(plugins)
#

import cjson
from twisted.web2 import resource, http

# utils
DEBUG = False
DEBUG = True


def cfirst(s):
    return s[0].capitalize() + s[1:]

def getArg(req, argName):
    try: return req.args[argName][0]
    except: raise ValueError('"%s" parameter expected' % argName)

class Resource(resource.PostableResource):
    def child_(self, request):
        if len(request.postpath) == 1:
            return self



#
#    Servers (ins)
#    ------- -----
#
#    A server object is an "in" port of a world.
#    "In" means it's like stdin: we read data from them.
#    Although, we can send back a response for every received message.
#    It does following things:
#    - handles POST requests
#    - returns JSON encoded responces just to know if request was accepted
#
class In(Resource):
    def _handleReq(self, request):
        # There must be a selector field
        what = getArg(request, 'what')
        handlingMethod = getattr(self, 'handle' + cfirst(what))
        #except AttributeError: raise ValueError("Don't know what '%s' is" % what)
        return handlingMethod(request)


    def render(self, request):
        """ For now let's just handle all remote messages here.
        """
        #if not self.authenticate(req):
        #    return cjson.encode({'err': {'msg': "Not authenticated"}})

        # maybe opposite
        if DEBUG:
            # Use twisteds exception handling mechanism
            result = self._handleReq(request)
            return http.Response(200, stream=cjson.encode(result))
        else:
            try:
                result = self._handleReq(request)
                return http.Response(200, stream=cjson.encode(result))
            except Exception, e:
                return http.Response(500, stream=cjson.encode({'err': {'msg': repr(e)}}))

    def getPlayer(self, req):
        """" Returns a Player objects for a player who sent this message.
        Also raises an exception in case we can't be sure. """
        try: who = req.args['who'][0]
        except: raise ValueError('"who" parameter expected')
        try: priv = req.args['priv'][0]
        except: raise ValueError('"priv" parameter expected')
        p = self._getPlayerById(who)
        if p.priv != priv: raise ValueError('priv token mismatch')
        return p

    # Overridables
    # ------------
    
    def _getPlayerById(self, id):
        raise ValueError('Implement _getPlayerById !')
        

class Game(In):
    """ Every message posted to this input is a move in a game.
    Each move is initiated by a specific player.
    Parameter name for this players' id is 'who'.
    All this means we also must authenticate everyone who sends us stuff.
    """
    table = {}

    def get(id):
        return Game.table.get(id)

    def __init__(self, players):
        sort(players)
        self.id = ''.join([p.id for p in players])
        self.players = players
        Game.table[id] = self

    def _getPlayerById(self, id):
        for p in self.players:
            if p.id == who: return p
        raise ValueError("No player %s in game %s" % (id, self.id))
        

