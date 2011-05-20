package vg.Sc;

import java.util.ArrayList;

import android.app.Activity;
import android.content.Context;
import android.os.Bundle;
import android.view.View;
import android.widget.*;
import android.content.ServiceConnection;
import android.content.ComponentName;
import android.os.IBinder;
import android.content.Intent;

import org.teleal.cling.android.AndroidUpnpService;
import org.teleal.cling.android.AndroidUpnpServiceImpl;

public class SessionList extends Activity implements NetworkNode.User
{
	ServiceConnection m_upnpSvcConn;

	@Override
    public void onCreate(Bundle savedInstanceState) 
	{
		m_cameras = new ArrayList<NetworkNode.Status>();

		super.onCreate(savedInstanceState);
		setContentView(R.layout.main);
		m_listVw = (ListView) findViewById(R.id.list);

		m_upnpSvcConn = new ServiceConnection() {
				public void onServiceConnected(ComponentName className, IBinder service) {
					AndroidUpnpService aus = (AndroidUpnpService) service;

					try {
						m_node = new NetworkNode(SessionList.this, aus.get());
						Toast.makeText(SessionList.this, "r: " + m_node.report(),
							Toast.LENGTH_LONG).show();
					} catch(Exception e) {
						// what to do
						Toast.makeText(SessionList.this, "E:" + e.toString(),
							Toast.LENGTH_LONG).show();						
					}

					Toast.makeText(SessionList.this, "r: " + m_node.report(),
						Toast.LENGTH_LONG).show();

					final Button refreshBtn = (Button) findViewById(R.id.refresh);
					refreshBtn.setOnClickListener(new View.OnClickListener() {
							public void onClick(View v) {
								refresh();
							}
						});
				}
			

				public void onServiceDisconnected(ComponentName className) {
					//m_ = null;
				}
			};
				
		
		getApplicationContext().bindService(
			new Intent(this, AndroidUpnpServiceImpl.class),
			m_upnpSvcConn,
			Context.BIND_AUTO_CREATE);

		
		
	}

	public void refresh() {
		
	}

	public void handleOtherNodePresence(NetworkNode.Status stat) {
		if(!m_cameras.contains(stat))
			m_cameras.add(stat);
		redisplayList();
	}

	public void handleOtherNodeQuit(NetworkNode.Status stat) {
		m_cameras.remove(stat);
		redisplayList();
	}

	private void redisplayList() {
		runOnUiThread(new Runnable() {
				public void run() {
					m_listVw.setAdapter(new ArrayAdapter<NetworkNode.Status>(
							SessionList.this, 
							R.layout.cams_list_entry, 
							m_cameras.toArray(new NetworkNode.Status[0])));
				}
			});
	}

	@Override protected void onDestroy() {
		super.onDestroy();
		if (m_upnpSvcConn != null) {
			//	upnpService.getRegistry().removeListener(registryListener);
		}
		getApplicationContext().unbindService(m_upnpSvcConn);
	}

	private ListView m_listVw;
	private ArrayList<NetworkNode.Status> m_cameras;
	private NetworkNode m_node;
}
