import sys, os, time, datetime, tempfile
import atom.data
import gdata.sites.client
import gdata.sites.data

client = gdata.sites.client.SitesClient(source='yourCo-yourAppName-v1', site='gorsunov')
client.ClientLogin('gorsunov@gmail.com', file(".pwd").read(), client.source);
entry = client.CreatePage('filecabinet', 'File Storage', html='<b>HTML content</b>', page_name='files-%d' % time.time())

for path in sys.argv[1:]:
		_, t = tempfile.mkstemp(suffix=".jpg")
		os.system("convert %s -resize 700 %s" % (path, t))
		attachment = client.UploadAttachment(path, entry, content_type='image/jpg',
                                     title="%s %s" %(datetime.datetime.today(), path), description='auto')
		ta = client.UploadAttachment(t, entry, content_type='image/jpg',
                                     title="%s thumb %s" %(datetime.datetime.today(), path), description='auto')
		os.remove(t)
		print '<a href="%s"><img src="%s" /></a><br /><br />' % (
				attachment.GetAlternateLink().href, ta.GetAlternateLink().href)
		
