import urllib2
import re

def main():
		ulmartPricePattern = re.compile(r"([0-9][0-9 ]+).\.</span>")
		url = "http://www.ulmart.ru/goods/181800/"
		r = urllib2.urlopen(url)
		print(r.info())
		r = r.read().decode('utf-8')
		print(type(r))
		r = ulmartPricePattern.search(r)
		if not r:
				print "No price!"
				return

		print(r.group(1))
		p = int(re.sub("[^0-9]+", "", r.group(1)))
		print p

if __name__ == "__main__":
		main()
