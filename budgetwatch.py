# coding=windows-1251
import urllib2, re, locale
locale.setlocale(locale.LC_ALL, "")

class Item:
		def __init__(self, id):
				self.id = id
				self.title = None
				self.children = []
				self.value = -1

		def p(self, indent=0):
				print indent * " " + "%s: %s" % ( 
						locale.format("%.1f", self.value, True),
						self.title
						)
				for c in self.children: c.p(indent + 1)
				

class Budget:
		def __init__(self):
				self.items = {}
				self.root = Item("r")

		def getParent(self, it):
				parts = it.id.split(" ")
				i = len(parts) - 1
				while i >= 0:
						part = "0" * len(parts[i])
						if part != parts[i]:
								parts[i] = part
								p = self.items.get(" ".join(parts))
								if p: return p
						i = i - 1
						

		def add(self, it):
				self.items[it.id] = it
				(self.getParent(it) or self.root).children.append(it)

b = Budget()

tagPat = re.compile("<.+?>")
tdPat = re.compile("<td.*?>(.+?)</td>", re.I | re.S)
codePat = re.compile(r"""([0-9])\s+([0-9]{2})\s+([0-9]{5})\s+([0-9]{2})\s+([0-9]{4})\s+([0-9]{3})""")
terminatorPat = re.compile("ИТОГО ДОХОДОВ")

urls = [
		"http://www.gov.spb.ru/law?doc&nd=8461106&nh=0&ssect=4",
		"http://www.gov.spb.ru/law?doc&nd=8461106&nh=0&ssect=5"
]

for url in urls:
		r = urllib2.urlopen(url)
		r = r.read()
		
		tds = tdPat.finditer(r)
		it = None
		for x in tds:
				s = tagPat.sub("", x.group(1)).strip()
				if terminatorPat.search(s):
						break
				m = codePat.match(s)
				if m:
						it = Item(m.expand(r"\1 \2 \3 \4 \5 \6"))
						continue
				if not it: 
						continue # discard
				if not it.title:
						it.title = s
						continue
				val = re.match("[0-9\\.]+", s)
				if val:
						it.value = float(val.group(0))
						b.add(it)
						it = None


b.root.p()


