import socket, select

port = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
port.bind(('', 9876))
port.listen(1)

import re

printablePtn = re.compile("[\0-\x09\x0b-\x1f]+")	
def getPrintable(s):
		return printablePtn.sub(".", s)
		

while 1:
		cl, addr = port.accept()
		print 'Connected by', addr
		srv = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		srv.connect(('localhost', 8000))
		try:
				while 1:
						r,_,_ = select.select([cl, srv], [], [])
						for s in r:
								if s == cl:
										data = cl.recv(1024)
										if not data: 
												print "cc"
												break
										srv.send(data)
										print getPrintable(data)
								elif s == srv:
										data = srv.recv(1024)
										if not data: 
												print "sc"
												break
										cl.send(data)
										print getPrintable(data)
						if not data:
								break
		except socket.error, e:
				print e
		srv.close()
		cl.close()

