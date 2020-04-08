#!/usr/bin/python
# -*- coding: utf-8 -*-
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

def getTextRecursive(n):
	rc = []
	for node in n.childNodes:
		if node.nodeType == node.TEXT_NODE:
			rc.append(node.data)
		else:
			rc.append(getTextRecursive(node))
	return ''.join(rc)
	

class Item:
	def __init__(self, n):
		self.node = n
		self.title = getText(n.getElementsByTagName("title")[0])
		d = getText(n.getElementsByTagName("pubDate")[0])
		d = d.replace(".", " ")
		d = re.sub(r"(?<=[0-9])(?=[A-Za-z])", " ", d)
		dt = email.utils.parsedate(d)
		if not dt:
			raise Exception("Bad date %s" % (d))
		self.ts = int(time.mktime(dt))
		self.date = datetime.datetime.fromtimestamp(self.ts)
		self.xmlError = 0

	def getDesc(self):
		return getText(self.node.getElementsByTagName("description")[0])

	def getAudioUrl(self):
		nodes = self.node.getElementsByTagName("enclosure")
		if len(nodes) == 0:
			return None
		if len(nodes) != 1:
			raise Exception("Bad number of enclosures = %d" % (len(nodes)))
		return nodes[0].getAttribute("url")
	
	def getName(self, options):
		if options.prefix:
			n = 0
			prefix = options.prefix
		elif not options.reverse:
			n = self.ts
			prefix = "c"
		else:
			n = 0xFFFFFFFF - self.ts
			prefix = ""
		if options.names:
			audio = self.getAudioUrl()
			if not audio:
				raise Exception(
					"--names option must only be used for audio feeds")
			name = os.path.basename(urlparse.urlparse(audio).path)
			m = name
		else:
			name = self.title
			m = re.sub(r"(?u)[^\w]+", "_", name)[0:30]
		if not options.prefix and options.reverse:
			# hex numbers don't get sorted right by Finder.app
			prefix = "%s%012d" % (prefix, n)
		return "%s-%04d-%02d-%02d-%s" % (
			prefix, self.date.year, self.date.month, self.date.day, m)

	def getDescNoTags(self):
		t = re.sub(r"<br[^<>]*>", "\n", self.getDesc())
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
			# d = self.getDescTextXhtml()
			d = self.getDescNoTags()
		except Exception, e:
			self.xmlError = e
			d = "XML error: " + e.__str__()
			d += "\n"
			t = self.getDescNoTags()
			t = re.sub("&nbsp;", " ", t)
			d += t
		return "%s\n\n%s\n\n%s" % (
			self.date.strftime("%a, %d %b %Y, %H %M"), self.title, d)

class Fb2Item(Item):
	def __init__(self, n, ts):
		self.ts = ts
		self.date = datetime.datetime.fromtimestamp(self.ts)
		self.title = getTextRecursive(n.getElementsByTagName("title")[0])
		self.node = n

	def getText(self):
		return getTextRecursive(self.node)		
	
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
		elif re.match(r"https?://", p):
			req = urllib2.Request(p)
			res = urllib2.urlopen(req)
		else:
			res = open(p, "rt")
		self.loadAtomFromStream(res)

	def loadFb2(self, doc):
		ts = os.stat(__file__).st_mtime
		for n in doc.getElementsByTagName("section"):
			self.items += [Fb2Item(n, ts)]
			ts += 24*3600

	def loadAtomFromStream(self, res):
		doc = xml.dom.minidom.parse(res)
		if doc.getElementsByTagName("FictionBook"):
			self.loadFb2(doc)
		else:
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
		work = self.items
		if not work:
			print("No items loaded")
			
		if self.options.contains:
			s = self.options.contains[0]
			work = [t for t in work if t.getText().find(s) >= 0]
			if not work:
				print("No items containing %s" % (s))
				return

		if self.options.podcast:
			work = [t for t in work if t.getAudioUrl()]
			if not work:
				print("No podcast items")
				return
			
		work = [
			item for item in work
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
		work.sort(key = lambda t: t.ts, reverse=self.options.recent)
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
		self.tmp = op + ".tmp.mp4"
		if os.path.isfile(self.tmp):
			os.unlink(self.tmp)
		audio = item.getAudioUrl()
		if audio:
			print "Downloading '%s'..." % (audio)
			if self.options.curl:
				r = 56 # Connection reset by peer
				while r == 56: 
					r = subprocess.Popen([
						"curl", "-Lk", "--retry", "99999",
						audio, "-o", self.tmp]).wait()
				if r != 0:
					raise Exception("curl returned %d" % (r))
			else:
				src = urllib2.urlopen(urllib2.Request(audio))
				shutil.copyfileobj(src, open(self.tmp, "w"))
		else:
			if not self.convertText(n, item, out, op, self.tmp):
				return
		os.rename(self.tmp, op)

	def getVoice(self, text):
		if text.find(u"в") != -1:
			return "Milena"
		return "Tom"

	def convertText(self, n, item, out, op, p):
		if self.options.textonly:
			text = item.getDescNoTags()
		else:
			text = "Job %d of %d\n\n%s" % (n, self.count.work, item.getText())
		if self.hasStopWords(text):
			return False
		tf = open(self.getOutputPath(item, out, ".txt"), "w")
		tf.write(text.encode("utf-8"))
		c = subprocess.Popen(
			["say", "-v", self.getVoice(text), "--output-file", p, "--progress"],
			stdin=subprocess.PIPE)
		c.stdin.write(text.encode("utf-8"))
		c.stdin.close()
		r = c.wait()
		if r != 0:
			raise Exception("say returned %d" % (r))
		return True
		
class Download:
	def __init__(self, args):
		self.args = args
		self.feed = Feed(args)

	def run(self, out):
		if self.args.default:
			self.feed.loadAtom("https://stackoverflow.com/jobs/feed?v=true")
			self.feed.run(out)
		else:
			if 0 == len(self.args.INPUT):
				self.feed.loadAtomFromStream(sys.stdin)
			for addr in self.args.INPUT:
				self.feed.loadAtom(addr)
			self.feed.run(out)
		
	def cleanup(self):
		if hasattr(self.feed, "tmp"):
			os.path.isfile(self.feed.tmp) and os.unlink(self.feed.tmp)
			
if __name__ == '__main__':
	parser = argparse.ArgumentParser(
		description="Download podcasts")
	parser.add_argument(
		"--default", action="store_true")
	parser.add_argument(
		"--recent", action="store_true", help="Newest first")
	parser.add_argument(
		"--reverse", action="store_true",
		help="Renumber so newest go first alphabetically")
	parser.add_argument(
		"--names", action="store_true", help="Save file names")
	parser.add_argument(
		"--curl", action="store_true", help="Run curl")
	parser.add_argument(
		"--prefix", nargs=1, action="store", help="Prefix")
	parser.add_argument(
		"--out", nargs=1, action="store", help="Storage directory")
	parser.add_argument(
		"--textonly", action="store_true",
		help="Don't convert title and date to speech")
	parser.add_argument(
		"--contains", nargs=1, action="store", help="Substring")
	parser.add_argument(
		"--podcast", action="store_true", help="Only those with an mp3 file")
	parser.add_argument("INPUT", nargs="*")
	args = parser.parse_args()

	d = Download(args)
	try:
		d.run(args.out and args.out[0] or out)
	finally:
		d.cleanup()
