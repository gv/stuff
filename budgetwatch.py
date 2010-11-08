import urllib2, re

tagPat = re.compile("<.+?>")

tdPat = re.compile("<td.*?>(.+?)</td>", re.I | re.S)

url = "http://www.gov.spb.ru/law?doc&nd=8461106&nh=0&ssect=4"
r = urllib2.urlopen(url)
r = r.read()

tds = tdPat.finditer(r)
for x in tds:
		print "P: " + tagPat.sub("", x.group(1)).strip()
