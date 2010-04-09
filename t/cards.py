#
#  Card data type for use in all kinds of card games
#

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

JACK = 11
QUEEN = 12
KING = 13
ACE = 1


