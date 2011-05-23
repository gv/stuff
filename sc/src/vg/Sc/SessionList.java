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
import android.util.Log;

import android.hardware.Camera;
import android.view.SurfaceView;
import android.view.SurfaceHolder;

import org.teleal.cling.android.AndroidUpnpService;
import org.teleal.cling.android.AndroidUpnpServiceImpl;

public class SessionList extends Activity 
	implements NetworkNode.User, SurfaceHolder.Callback {
	static final String TAG = "DuctTapedC";
	ServiceConnection m_upnpSvcConn;
	Camera m_cam;
	SurfaceHolder m_sfcHl;
	
	/* 
		 Activity events
	*/

	@Override
    public void onResume() {
		try {
			super.onResume();

			// Init camera

			m_cam = Camera.open();
			m_cam.startPreview();
			if(m_sfcHl != null)
				m_cam.setPreviewDisplay(m_sfcHl);
		} catch(Exception e) {
			Log.e(TAG, "onResume", e);
		}
	}


	@Override
    public void onPause() {
		super.onPause();
		m_cam.release();
		m_cam = null;
	}


	@Override
    public void onCreate(Bundle savedInstanceState) {
		m_cameras = new ArrayList<NetworkNode.Status>();

		super.onCreate(savedInstanceState);
		setContentView(R.layout.main);
		m_listVw = (ListView) findViewById(R.id.list);
		SurfaceHolder hl = ((SurfaceView) findViewById(R.id.surface)).getHolder();
		hl.addCallback(this);
		hl.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS);
		


	/*
		Init communications
	*/
	
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

	@Override protected void onDestroy() {
		super.onDestroy();
		if (m_upnpSvcConn != null) {
			//	upnpService.getRegistry().removeListener(registryListener);
		}
		getApplicationContext().unbindService(m_upnpSvcConn);
	}

	/*
		Events from SurfaceHolder
	*/

	public void surfaceCreated(SurfaceHolder h) {
	}

	public void surfaceChanged(SurfaceHolder hl, int format, int w, int h) {
		try {
			m_sfcHl = hl;
			if(hl.isCreating()) {
				if(m_cam != null)
					m_cam.setPreviewDisplay(m_sfcHl);
			}
		} catch(Exception e) {
			Log.e(TAG, "surfaceChanged", e);
		}			
	}			
	
	public void surfaceDestroyed(SurfaceHolder h) {
		m_sfcHl = null;
	}
	
	/*
		Events from NetworkNode
	*/

	public void handleOtherNodePresence(NetworkNode.Status stat) {
		if(!m_cameras.contains(stat))
			m_cameras.add(stat);
		redisplayList();
	}

	public void handleOtherNodeQuit(NetworkNode.Status stat) {
		m_cameras.remove(stat);
		redisplayList();
	}


	/*
		Utils
	*/

	private void refresh() {
		
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

	private ListView m_listVw;
	private ArrayList<NetworkNode.Status> m_cameras;
	private NetworkNode m_node;
}
