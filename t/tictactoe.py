#
#   This is a tic tac toe implementation
#

import world

class Game(world.Game):
		
		gameType = 'tictactoe'

		def __init__(self, players):
				if (len(players)) != 2:
						raise AttributeError("Can't play tictactoe with %n players" % 
																 len(players))
				world.Game.__init__(self, players)
				
				
				
