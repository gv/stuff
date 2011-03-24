s = [1,3,4,6]

def permute(s, prefix=[]):
		if s == []:
				yield prefix
		else:
				for i in range(len(s)):
						for x in permute(s[1:], prefix + [s[0]]):
								yield x
						s = s[1:] + [s[0]]

def pjoin(s, ops):
		if len(s) == 1:
				yield s[0]
		else:
				for op in ops:
						for suffix in pjoin(s[1:], ops):
								yield "%s%s%s" % (s[0], op, suffix)

def permutePart(s, prefix=[]):
		if s == []:
				yield prefix
		else:
				for i in range(len(s)):
						for x in permute(s[1:], prefix + [s[0]]):
								yield x
						s = s[1:] + [s[0]]


	
for x in permute(s): 
		for y in pjoin(x, "*+-"):
				print "%s = %s" % (y, eval(y))
