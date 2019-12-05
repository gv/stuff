#!/usr/bin/python
import urllib2
import re
import xml.dom.minidom
import xml.sax
import time
import datetime
import email.utils
import os.path
import os
import subprocess
import argparse
import urlparse
import shutil
import sys

out = os.path.join(os.path.dirname(__file__), "audio")

def getText(n):
		rc = []
		for node in n.childNodes:
				if node.nodeType == node.TEXT_NODE:
						rc.append(node.data)
		return ''.join(rc)

class Item:
		def __init__(self, n):
				self.node = n
				self.title = getText(n.getElementsByTagName("title")[0])
				self.desc = getText(n.getElementsByTagName("description")[0])
				d = getText(n.getElementsByTagName("pubDate")[0])
				self.ts = int(time.mktime(email.utils.parsedate(d)))
				self.date = datetime.datetime.fromtimestamp(self.ts)
				self.xmlError = 0

		def getAudioUrl(self):
				nodes = self.node.getElementsByTagName("enclosure")
				if len(nodes) == 0:
						return None
				if len(nodes) != 1:
						raise Exception("Bad number of enclosures = %d" % (len(nodes)))
				return nodes[0].getAttribute("url")
		
		def getName(self, options):
				if options.reverse:
						n = self.ts
						prefix = "chrono"
				else:
						n = 0xFFFFFFFF - self.ts
						prefix = ""
				if options.names:
						audio = self.getAudioUrl()
						if not audio:
								raise Exception(
										"--names option must noly be used for audio feeds")
						name = os.path.basename(urlparse.urlparse(audio).path)
						m = name
				else:
						name = self.title
						m = re.sub(r"[^A-Za-z0-9]", "_", name[0:20])
				return "%s%09X-%04d-%02d-%02d-%s" % (
						prefix, n, self.date.year, self.date.month, self.date.day, m)

		def getDescNoTags(self):
				t = re.sub(r"<br[^<>]*>", "\n", self.desc)
				t = re.sub(r"<[^<>]+>", "", t)
				return t

		def getDescTextXhtml(self):
				t = self.getDescNoTags()
				 # print t
				p = '''<!DOCTYPE html
PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
'''
				d = xml.dom.minidom.parseString('%s<p>%s</p>' % (p, t))
				return d.getElementsByTagName("p")[0].childNodes[0].data

		def getText(self):
				try:
						d = self.getDescTextXhtml()
				except Exception, e:
						self.xmlError = e
						d = "XML error: " + e.__str__()
						d += "\n"
						t = self.getDescNoTags()
						t = re.sub("&nbsp;", " ", t)
						d += t
				return "%s\n\n%s\n\n%s" % (
						self.date.strftime("%a, %d %b %Y, %H %M"), self.title, d) 
				
		
class Feed:
		def __init__(self, options):
				class Count:
						def __init__(self):
								self.work = 0
				
				self.count = Count()
				self.items = []
				self.options = options

		def loadAtom(self, p):
				if "-" == p:
						res = sys.stdin
				else:
						req = urllib2.Request(p)
						res = urllib2.urlopen(req)
				doc = xml.dom.minidom.parse(res)
				for n in doc.getElementsByTagName("item"):
						item = Item(n)
						self.items += [item]
				self.items.sort(key = lambda t: t.ts)

		def getOutputPath(self, item, out, suffix):
				return os.path.join(out, item.getName(self.options) + suffix)

		def getMainOutputPath(self, item, out):
				audio = item.getAudioUrl()
				if not audio:
						return self.getOutputPath(item, out, ".mp4")
				return self.getOutputPath(item, out, "." + 
						os.path.basename(urlparse.urlparse(audio).path).split('.')[-1])

		def run(self, out):
				work = [
						item for item in self.items
						if not os.path.isfile(self.getMainOutputPath(item, out))]
				self.count.work = len(work)

				if 0 == len(work):
						print "Every one of %d items is already present in %s" % (
														len(self.items), out)
				
				try:
						os.mkdir(out)
				except OSError, e:
						if e.errno != 17:
								raise e
				n = 1
				work.sort(key = lambda t: t.ts, reverse=not self.options.reverse)
				for item in work:
						print "%d / %d %s %s" % (
								n, self.count.work, item.getName(self.options), item.title)
						self.save(n, item, out)
						n += 1

		def hasStopWords(self, text):
				stopWords = ["Wir"]
				for w in stopWords:
						if text.find(w) != -1:
								print "Found '%s' in text" % (w)
								return True
				return False

		def save(self, n, item, out):
				op = self.getMainOutputPath(item, out)
				p = op + ".tmp.mp4"
				if os.path.isfile(p):
						os.unlink(p)
				audio = item.getAudioUrl()
				if audio:
						print "Downloading %s ..." % (audio)
						if self.options.curl:
								r = subprocess.Popen([
																"curl", "-L", audio, "-o", p]).wait()
								if r != 0:
										raise Exception("curl returned %d" % (r))
						else:
								src = urllib2.urlopen(urllib2.Request(audio))
								shutil.copyfileobj(src, open(p, "w"))
				else:
						if not self.convertText(n, item, out, op, p):
								return
				os.rename(p, op)

		def convertText(self, n, item, out, op, p):
				text = "Job %d of %d\n\n%s" % (n, self.count.work, item.getText())
				if self.hasStopWords(text):
						return False
				tf = open(self.getOutputPath(item, out, ".txt"), "w")
				tf.write(text.encode("utf-8"))
				c = subprocess.Popen(
						["say", "-v", "Tom", "--output-file", p, "--progress"],
						stdin=subprocess.PIPE)
				c.stdin.write(text.encode("utf-8"))
				c.stdin.close()
				r = c.wait()
				if r != 0:
						raise Exception("say returned %d" % (r))
				return True
				
				
if __name__ == '__main__':
		parser = argparse.ArgumentParser(
				description="Download podcasts")
		parser.add_argument(
				"--default", action="store_true")
		parser.add_argument(
				"--reverse", action="store_true", help="Oldest first")
		parser.add_argument(
				"--names", action="store_true", help="Save file names")
		parser.add_argument(
				"--curl", action="store_true", help="Run curl")
		parser.add_argument("INPUT", nargs="*")
		args = parser.parse_args()

		if args.default:
				p = Feed(args)
				p.loadAtom("https://stackoverflow.com/jobs/feed?v=true")
				p.run(out)
		else:
				p = Feed(args)
				for addr in args.INPUT:
						p.loadAtom(addr)
				p.run(out)
