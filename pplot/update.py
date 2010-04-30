import urllib2
import re
import logging
import datetime

"""
GET /goods/181800/?head=1 HTTP/1.1
Host: www.ulmart.ru
Connection: keep-alive
User-Agent: Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US) AppleWebKit/533.2 (KHTML, like Gecko) Chrome/5.0.342.9 Safari/533.2
Referer: http://www.ulmart.ru/goods/181800/
Cache-Control: max-age=0
Accept: application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5
Accept-Encoding: gzip,deflate,sdch
Accept-Language: ru-RU,ru;q=0.8,en-US;q=0.6,en;q=0.4
Accept-Charset: windows-1251,utf-8;q=0.7,*;q=0.3
Cookie: CLTRACK=ble5stvuqh9t6lrgmcfqbcg783; forum_data=a%3A2%3A%7Bs%3A11%3A%22autologinid%22%3Bs%3A0%3A%22%22%3Bs%3A6%3A%22userid%22%3Bi%3A-1%3B%7D; scroll_count=5; preview=-1; currency=rur; __utma=174499942.1612825002036179000.1246281537.1256135615.1257261395.7; anti_bot=1; utmpid=WfkWmksWo/N8rFz9A33mAg==; columns=a%3A0%3A%7B%7D; price_currency=rur; currency_rate=0; orderdir=asc; orderby=retail_price; view_type=0; city=18413; PHPSESSID=7ca29bc0ddc209835c079b2164db9b89
"""

from google.appengine.ext import db

import pplot

titlePattern = re.compile(r"<title>(.*)</title>")
ulmartPricePattern = re.compile(r"([0-9][0-9 ]+).\.</span>")

def main():
		urls = ["http://www.ulmart.ru/goods/181800/",
						"http://www.ulmart.ru/goods/184857/",
						"http://www.ulmart.ru/goods/194881/",
						"http://www.ulmart.ru/goods/194360/",
						"http://www.ulmart.ru/goods/196607/"
						]
		for url in urls:
				try:
						updateUrl(url)
				except Exception, e:
						logging.warn("%s: %s" % (url, str(e)))

def updateUrl(url):
		req = urllib2.Request(url, "", {
						"Cookie": "anti_bot=1; city=18413"
						})
		r = urllib2.urlopen(req)
		#print(r.info())
		r = r.read()
		#open("z.html", "w").write(r)

		text = r.decode('utf-8')
		r = ulmartPricePattern.search(text)
		if not r:
				logging.warn("%s: No price!" % url)
				return

		p = int(re.sub("[^0-9]+", "", r.group(1)))
		
		d = datetime.datetime.now()
		q = db.GqlQuery("SELECT * FROM Offer WHERE url = :1", url)
		r = q.fetch(1)
		if r:
				offerRec = r[0]
		else:
				offerRec = pplot.Offer(url = url)
				offerRec.put()
				logging.warn("Offer recorded for %s" % url)

		if not offerRec.title:
				m = titlePattern.search(text)
				if m:
						offerRec.title = m.group(1)
						offerRec.put()

		rec = pplot.Price(date = d, value = p, offer = offerRec, unit = 'r')
		rec.put()

if __name__ == "__main__":
		main()
