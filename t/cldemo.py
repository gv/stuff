import sys
sys.path = ['ws', '/opt/ws'] + sys.path

import threading, socket, random, time

try:
		import cjson
except ImportError:
		import simplejson as cjson
		cjson.encode = cjson.dumps
		cjson.decode = cjson.loads

SVR_IP = "172.16.0.77"

def randomString():
		return ''.join(random.choice("poiuytrewqasdfghjklmnbvcxz") for _ in range(16))


clientCnt = 0
sockCnt = 0
connectionFailureCnt = 0

class Client:
		_count = 0
		s = None

		def __init__(self):
				self.id = Client._count
				Client._count += 1
				
				retries = 999999999
				while not self.s: 
						try:
								self.s = socket.create_connection((SVR_IP, 8888))
						except:
								global connectionFailureCnt
								connectionFailureCnt += 1
								retries -= 1
								if not retries:
										return
								lat = random.random()*10
								#print "retrying in %f" % lat 
								time.sleep(lat)

				global sockCnt
				sockCnt += 1

				self.s.send("GET /s HTTP/1.1\r\n" +
										"Host: and\r\n" +
										"Connection: Upgrade\r\nUpgrade: WebSocket\r\n" + 
										"Origin: http://and/\r\n" + 
										"\r\n")
				self.s.recv(1024) # handshake

				self.send(what="createPerson")
				self.s.recv(64536)

				global clientCnt
				clientCnt += 1

				while 1:
						self.send(what="createComment", 
											text=randomString() + " " + randomString())
						self.s.recv(64536)
						time.sleep(random.random() * 10)

		
		def send(self, m=None, **kw):
				m = m or kw
				self.s.send("\x00%s\xFF" % cjson.encode(m))

		def report(self, s):
				print "%d: %s" % (self.id, s)

runningCnt = 0

class Td(threading.Thread):
		def run(self):
				global runningCnt
				try:
						runningCnt += 1
						Client()
				finally:
						runningCnt -= 1

threadsStartedCnt = 0
class MasterTd(threading.Thread):
		def run(self):
				global threadsStartedCnt
				for _ in range(10000):
						Td().start()
						threadsStartedCnt += 1
						
		
# main

MasterTd().start()
while 1:
		print "%d started, %d running, %d sockets, %d clients, %d conn fails" % (threadsStartedCnt, runningCnt, sockCnt, clientCnt, connectionFailureCnt)
		time.sleep(1)
