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
import android.view.KeyEvent;

import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.hardware.Sensor;
import android.hardware.SensorEvent;

import org.teleal.cling.android.AndroidUpnpService;
import org.teleal.cling.android.AndroidUpnpServiceImpl;

public class SessionList extends Activity 
	implements NetworkNode.User, SurfaceHolder.Callback, Camera.PreviewCallback, 
						 SensorEventListener {
	static final String TAG = "DuctTapedC";
	ServiceConnection mUpnpSvcConn;
	Camera mCam;
	SurfaceHolder mSfcHl;
	View mOverlay;

	class Position {
		public int[] mVisionResult; // blob

		Position getRef() {
			if(null == mVisionResult)
				return null;
			Position r = new Position();
			r.mVisionResult = mVisionResult;
			return r;
		}
	}

	Position mCurPos = new Position(), mRefPos;
	
	/* 
		 Activity events
	*/

	@Override
    public void onResume() {
		mSensorManager.registerListener(this, mAccelerometer, 
			SensorManager.SENSOR_DELAY_NORMAL);
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
		mSensorManager.unregisterListener(this);
		mCam.release();
		mCam = null;
	}


	@Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
		switch (keyCode) {
		case KeyEvent.KEYCODE_DPAD_CENTER:
			mRefPos = mCurPos.getRef();
			return true;
		}
		return super.onKeyDown(keyCode, event);
	}

	private Point average(int[] v) {
		float x = 0, y = 0;
		int size = v.length * 2 / 3;
		for(int i = 0; i < size;) {
			x += v[i++];
			y += v[i++];
		}
		size /= 2;
		return new Point((int)(x/size), (int)(y/size)); 
	}

	private void drawArrow(Canvas c, 
		float startX, float startY, float tipX, float tipY) {
		Paint yellow = new Paint();
		yellow.setColor(0x88FFFF00);
		yellow.setStyle(Paint.Style.STROKE);
		yellow.setStrokeWidth(6);

		c.drawLine(startX, startY, tipX, tipY, yellow);
		float angle = (float)Math.atan(((startY - tipY)/(startX - tipX)));
		float a = 10, d = .3f;
		c.drawLine(tipX, tipY, (float)(tipX + Math.sin(angle + d) * a), 
			(float)(tipY + Math.cos(angle + d) * a), yellow);
		c.drawLine(tipX, tipY, (float)(tipX + Math.sin(angle - d) * a), 
			(float)(tipY + Math.cos(angle - d) * a), yellow);
	}

	private float mViewFinderHeight, mViewFinderWidth, mPreviewWidth, mPreviewHeight;

	private float screenX(int previewX, int previewY) {
		return mViewFinderWidth - mViewFinderWidth / mPreviewHeight * previewY;
	}

	private float screenY(int previewX, int previewY) {
		return mViewFinderHeight / mPreviewWidth * previewX;
	}

	@Override
    public void onCreate(Bundle savedInstanceState) {
		mCameras = new ArrayList<NetworkNode.Status>();

		super.onCreate(savedInstanceState);

		mSensorManager = (SensorManager)getSystemService(SENSOR_SERVICE);
		mAccelerometer = mSensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER);

		setContentView(R.layout.main);
		mListVw = (ListView) findViewById(R.id.list);
		SurfaceHolder hl = ((SurfaceView) findViewById(R.id.surface)).getHolder();
		hl.addCallback(this);
		hl.setType(SurfaceHolder.SURFACE_TYPE_PUSH_BUFFERS);
		
		mOverlay = findViewById(R.id.over);
		mOverlay.setBackgroundDrawable(new Drawable() {
				public int getOpacity() {
					return PixelFormat.TRANSPARENT;
				}

				public void draw(Canvas c) {
					Paint red = new Paint();
					red.setColor(0x77FF0000);
					red.setStyle(Paint.Style.STROKE);

					Paint levelLine = new Paint();
					levelLine.setColor(0x88FFFFFF);
					levelLine.setStyle(Paint.Style.STROKE);
					levelLine.setStrokeWidth(6);
					
					float viewWidth = mOverlay.getWidth();
					mViewFinderWidth = viewWidth;
					float viewHeight = mOverlay.getHeight();
					mViewFinderHeight = viewHeight;

					if(Math.abs(mAccY) >= Math.abs(mAccX)) {
						float ratio = mAccX / mAccY;
						c.drawLine(.0f, (1 - ratio) * viewHeight / 2, 
							viewWidth, (1 + ratio) * viewHeight / 2, levelLine);
					} else {
						float ratio = mAccY / mAccX;
						c.drawLine((1 - ratio) * viewWidth / 2, .0f, 
							(1 + ratio) * viewWidth / 2, viewHeight, levelLine);
					}

					// todo lock
					if(null == mCam) {
						c.drawText("Camera is sleeping...", 0, viewHeight, red);
						return;
					}

					// surface is rotated 90degree clockwise
					// preview X axis goes from bottom to top
					// preview Y axis goes from left to right
					float pvWidth = mCam.getParameters().getPreviewSize().width;
					mPreviewWidth = pvWidth;
					float pvHeight = mCam.getParameters().getPreviewSize().height;
					mPreviewHeight = pvHeight;

					//Log.d(TAG, "overlay: " + viewWidth + "x" + viewHeight + " / " + 
					//	pvWidth + "x" + pvHeight + " " + iSize/2);

					float framePxWidth = viewHeight / pvWidth;
					float framePxHeight = viewWidth / pvHeight;


					if(mRefPos != null) {
						int[] blob = mRefPos.mVisionResult;
						if(null != blob) {
							Paint green = new Paint();
							green.setColor(0x7700FF00);
							green.setStyle(Paint.Style.STROKE);

							int iSize = blob.length * 2 / 3, i = 0;
							for(; iSize < blob.length; iSize++) {
								int px = blob[i++];
								int py = blob[i++];
								int y = (int)screenY(px, py);
								int x = (int)screenX(px, py);
								int r = blob[iSize] + 3;
								c.drawCircle(x, y, r, green);
							}

							
							if(mCurPos.mVisionResult != null) {
								Point refWeightCenter = average(blob);
								Point curWeightCenter = average(mCurPos.mVisionResult);
								c.drawLine(viewWidth / 2, viewHeight / 2 - 15, 
									viewWidth / 2, viewHeight / 2 + 15, red);
								c.drawLine(viewWidth / 2 - 15, viewHeight / 2, 
									viewWidth / 2 + 15, viewHeight / 2, red);
								float tipX = viewWidth / 2 - framePxHeight * 
									(float)(curWeightCenter.y - refWeightCenter.y);
								float tipY = viewHeight / 2 + framePxWidth *
									(float)(curWeightCenter.x - refWeightCenter.x);
								drawArrow(c, (float)viewWidth / 2, (float)viewHeight / 2, 
									tipX, tipY);

								Paint rwcPaint = new Paint(green);
								rwcPaint.setStrokeWidth(8);
								c.drawCircle(screenX(refWeightCenter.x, refWeightCenter.y),
									screenY(refWeightCenter.x, refWeightCenter.y), 11, rwcPaint);
								
								Paint wcPaint = new Paint(red);
								wcPaint.setStrokeWidth(8);
								c.drawCircle(screenX(curWeightCenter.x, curWeightCenter.y),
									screenY(curWeightCenter.x, curWeightCenter.y), 11, wcPaint);
								

							}
							
						}
					}
					


					int[] blob = mCurPos.mVisionResult;
					if(null == blob) {
						c.drawText("Trying to see anything...", 0, viewHeight, red);
						return;
					}
					int iSize = blob.length * 2 / 3;

					int i = 0;
					for(; iSize < blob.length; iSize++) {
						int px = blob[i++];
						int py = blob[i++];
						int y = (int)(framePxWidth * (float)px);
						int x = (int)(viewWidth - framePxHeight * (float)py);
						int r = blob[iSize] + 3;
						c.drawCircle(x, y, r, red);
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
		Sensor stuff
	*/

	private SensorManager mSensorManager;
	private Sensor mAccelerometer;

	public SessionList() {
	}


	public void onAccuracyChanged(Sensor sensor, int accuracy) {
	}

	float mAccX = 1, mAccY = 0;
		 

	public void onSensorChanged(SensorEvent event) {
		if (event.sensor.getType() != Sensor.TYPE_ACCELEROMETER)
			return;
		mAccX = event.values[0];
		mAccY = event.values[1];
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

		/*mLastPoints = new Point[ptCnt];
		for(int i = 0; i < ptCnt; i++)
		mLastPoints[i] = new Point(visionResult[i*2], visionResult[i*2+1]);*/

		mCurPos.mVisionResult = visionResult;

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

	public native int[] findFeatures(byte[] buf, int width);
}
