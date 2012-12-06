import subprocess, sys
import time

N = 10
args = sys.argv[1:]
outs = []
procs = []


for i in xrange(N):
		p = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
		procs += [p]
		outs += [p.stdout]

while 1:
		runningCnt = 0
		for i in xrange(N):
				p = procs[i]
				if not p:
						continue
				if not p.poll() is None:
						print "#%d terminated" % (i)
						procs[i] = None
						continue
				runningCnt += 1
		print "%d running" % (runningCnt)
		if 0 == runningCnt:
				break
		time.sleep(1)
				

for p in procs:
		p.wait()
