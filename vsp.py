import sys
import csv

path = sys.argv[1]
inclusiveFirst = False

if len(sys.argv) > 2:
	if sys.argv[2] == "-i":
		inclusiveFirst = True
srcFile = open(path, 'r')
src = csv.reader(srcFile)

class Fn:
	# Function Name,Inclusive Samples,Exclusive Samples,
	# Inclusive Samples %,Exclusive Samples %,Source File,Function Line Number,
	# Function Address,Module Name,Module Path,Process Name,Process ID,
	def __init__(e, r):
		e.name = r[0]
		e.incl = int(r[1])
		e.excl = int(r[2])
		e.exclShare = r[4]

	def out(e):
		print "%6d=%5s%% %6d %s" % (e.excl, e.exclShare, e.incl, e.name)


class Ln:
	# D:\seven>less k_LineSummary.csv
	# Function Name,Exclusive Samples,Exclusive Samples %,
	# Source Line Begin,Source Character Begin,Source Line End,
	# Source Character End,Function Address,Function Line Number,Source File,
	# Module Name,Module Path,Process Name,Process ID,Line Name,
	def __init__(e, r):
		e.name = "%s:%s-%s" %(r[0], r[3], r[5])
		e.incl = 0
		e.excl = int(r[1])
		e.exclShare = r[2]

	def out(e):
		print "%6d=%5s%% %s" % (e.excl, e.exclShare, e.name)
		

Mode = Fn

if path.endswith("LineSummary.csv"):
	Mode = Ln

isHead = True
table = []
for r in src:
	if isHead:
		isHead = False
		continue
	e = Mode(r)
	table.append(e)

if inclusiveFirst:
	table.sort(key=lambda e: e.incl, reverse=True)
else:
	table.sort(key=lambda e: e.excl, reverse=True)

for e in table:
	e.out()
