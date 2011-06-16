package vg.Sc;

import java.util.ArrayList;
import java.lang.Math;

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
import android.graphics.PixelFormat;
import android.graphics.drawable.Drawable;
import android.graphics.Paint;
import android.graphics.Point;
import android.graphics.Canvas;
import android.graphics.ColorFilter;

import org.teleal.cling.android.AndroidUpnpService;
import org.teleal.cling.android.AndroidUpnpServiceImpl;

public class SessionList extends Activity 
	implements NetworkNode.User, SurfaceHolder.Callback, Camera.PreviewCallback {
	static final String TAG = "DuctTapedC";
	ServiceConnection mUpnpSvcConn;
	Camera mCam;
	SurfaceHolder mSfcHl;
	int mViewFinderHeight = 0;
	View mOverlay;
	
	/* 
		 Activity events
	*/

	@Override
    public void onResume() {
		try {
			super.onResume();

			// Init camera

			mCam = Camera.open();
			mCam.setDisplayOrientation(90);
			mCam.startPreview();
			if(mSfcHl != null)
				mCam.setPreviewDisplay(mSfcHl);

			System.loadLibrary("ducttapedcams");

			mCam.addCallbackBuffer(new byte[getPreviewBufferSize()]);
			mCam.setPreviewCallbackWithBuffer(this);
		} catch(Exception e) {
			Log.e(TAG, "onResume", e);
		}
	}


	@Override
    public void onPause() {
		super.onPause();
		mCam.release();
		mCam = null;
	}


	@Override
    public void onCreate(Bundle savedInstanceState) {
		mCameras = new ArrayList<NetworkNode.Status>();

		super.onCreate(savedInstanceState);
		setContentView(R.layout.main);
		mListVw = (ListView) findViewById(R.id.list);
		SurfaceHolder hl = ((SurfaceView) findViewById(R.id.surface)).getHolder();
		hl.addCallback(this);
		hl.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS);
		
		mViewFinderHeight = 200;//findViewById(R.id.over).getHeight();
		Log.d(TAG, "h=" + mViewFinderHeight);
		mOverlay = findViewById(R.id.over);
		mOverlay.setBackgroundDrawable(new Drawable() {
				public int getOpacity() {
					return PixelFormat.TRANSPARENT;
				}

				public void draw(Canvas c) {
					// todo lock
					if(null == mCam) {
						// todo draw message
						return;
					}

					if(null == mLastVisionResult) {

						return;
					}
					float framePxWidth = mOverlay.getWidth();
					float framePxHeight = mOverlay.getHeight();
					Log.d(TAG, "overlay: " + framePxWidth + "x" + framePxHeight);
					framePxWidth /= mCam.getParameters().getPreviewSize().width;
					framePxHeight /= mCam.getParameters().getPreviewSize().height;

					Paint paint = new Paint();
					paint.setColor(0x77FF0000);
					paint.setStyle(Paint.Style.STROKE);
					
					int i = 0;
					int iSize = mLastVisionResult.length * 2 / 3;
					for(; iSize < mLastVisionResult.length; iSize++) {
						int x = (int)(framePxWidth * (float)mLastVisionResult[i++]);
						int y = (int)(framePxHeight * (float)mLastVisionResult[i++]);
						int r = mLastVisionResult[iSize] + 3;
						c.drawCircle(x, y, r, paint);
					}
				}
				
				public void setColorFilter(ColorFilter cf) {

				}

				public void setAlpha(int a) {
					
				}
			});

		/*
			Communication setup
		*/
	
		mUpnpSvcConn = new ServiceConnection() {
				public void onServiceConnected(ComponentName className, IBinder service) {
					AndroidUpnpService aus = (AndroidUpnpService) service;

					try {
						mNode = new NetworkNode(SessionList.this, aus.get());
						Log.d(TAG, "r: " + mNode.report());
					} catch(Exception e) {
						// what to do
						Log.e(TAG, "onServiceConnected", e);
					}

					/*
					final Button refreshBtn = (Button) findViewById(R.id.refresh);
					refreshBtn.setOnClickListener(new View.OnClickListener() {
							public void onClick(View v) {
								refresh();
							}
						});
					*/
				}
			

				public void onServiceDisconnected(ComponentName className) {
					//m_ = null;
				}
			};
				
		
		getApplicationContext().bindService(
			new Intent(this, AndroidUpnpServiceImpl.class),
			mUpnpSvcConn, 
			Context.BIND_AUTO_CREATE);
	}

	@Override protected void onDestroy() {
		super.onDestroy();
		if (mUpnpSvcConn != null) {
			//	upnpService.getRegistry().removeListener(registryListener);
		}
		getApplicationContext().unbindService(mUpnpSvcConn);
	}

	/*
		Events from SurfaceHolder
	*/

	public void surfaceCreated(SurfaceHolder h) {
	}

	public void surfaceChanged(SurfaceHolder hl, int format, int w, int h) {
		try {
			mSfcHl = hl;
			if(hl.isCreating()) {
				if(mCam != null)
					mCam.setPreviewDisplay(mSfcHl);
			}
		} catch(Exception e) {
			Log.e(TAG, "surfaceChanged", e);
		}			
	}			
	
	public void surfaceDestroyed(SurfaceHolder h) {
		mSfcHl = null;
	}
	
	/*
		Events from NetworkNode
	*/

	public void handleOtherNodePresence(NetworkNode.Status stat) {
		if(!mCameras.contains(stat))
			mCameras.add(stat);
		redisplayList();
	}

	public void handleOtherNodeQuit(NetworkNode.Status stat) {
		mCameras.remove(stat);
		redisplayList();
	}


	/*
		Camera.PreviewCallback
	*/

	public void onPreviewFrame(byte[] data, Camera cam) {
		// TODO lock
		if(null == mCam)
			return;

		int f = mCam.getParameters().getPreviewFormat();

		
		int width = cam.getParameters().getPreviewSize().width;
		int[] visionResult = findFeatures(data, width);
		int ptCnt = visionResult.length / 3;

		mLastPoints = new Point[ptCnt];
		for(int i = 0; i < ptCnt; i++)
			mLastPoints[i] = new Point(visionResult[i*2], visionResult[i*2+1]);

		mLastVisionResult = visionResult;

		mOverlay.invalidate();
		cam.addCallbackBuffer(data);
	}


	/*
		Utils
	*/

	private void refresh() {
	}

	private void redisplayList() {
		runOnUiThread(new Runnable() {
				public void run() {
					mListVw.setAdapter(new ArrayAdapter<NetworkNode.Status>(
							SessionList.this, 
							R.layout.cams_list_entry, 
							mCameras.toArray(new NetworkNode.Status[0])));
				}
			});
	}
	
	private int getPreviewBufferSize() {
		PixelFormat formatDesc = new PixelFormat();
		int f = mCam.getParameters().getPreviewFormat();
		PixelFormat.getPixelFormatInfo(f, formatDesc);
		int w = mCam.getParameters().getPreviewSize().width;
		int h = mCam.getParameters().getPreviewSize().height;
		Log.d(TAG, w + "x" + h + "x" + formatDesc.bitsPerPixel + " (" + f + ")");
		int size = w * h * formatDesc.bitsPerPixel / 8;
		return size;
	}
	
	private ListView mListVw;
	private ArrayList<NetworkNode.Status> mCameras;
	private NetworkNode mNode;
	private Point[] mLastPoints;
	private int[] mLastVisionResult;

	public native int[] findFeatures(byte[] buf, int width);
}
