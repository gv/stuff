package gv.il;

import java.io.File;
import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.lang.Thread;
import java.util.ArrayList;

import android.app.ListActivity;
import android.app.Activity;
import android.os.Bundle;
import android.widget.ListView;
import android.widget.ListAdapter;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.graphics.BitmapFactory;
import android.graphics.Bitmap;
import android.database.DataSetObserver;
import android.os.Environment;
import android.util.Log;
import android.graphics.Paint;
import android.graphics.Canvas;


// Worker threads are more fun to watch

class SdImagesWorkerThread extends Thread {
	private static final String TAG = "il";
	private static final Pattern mImgFilenamePat = 
		Pattern.compile(".+\\.(jpg|jpeg|gif|png|bmp)$");

	private SdImages mImgs;

	public SdImagesWorkerThread(SdImages imgs) {
		mImgs = imgs;
	}

	public void run() {
		File dir = Environment.getExternalStorageDirectory();
		collect(dir);
	}
	
	void collect(File dir) {
		String[] paths = dir.list();
		if(null == paths) {
			return;
		}
		for(int i = paths.length - 1; i >= 0; i--) {
			File f = new File(dir.getAbsolutePath(), paths[i]);
			if(f.isDirectory()) {
				collect(f);
			} else {
				/*
				Leaving that to remind that we don't know what image formats can
				be decoded by android.graphics .

				So, the following code would be more reliable, although it would 
				probably be slower.
				
				mBmpOpts.inSampleSize = 64;
				Bitmap b = mBmpFcy.decodeFile(f.getAbsolutePath(), mBmpOpts);
				if(b != null)
				*/

				Log.e(TAG, "checking " + f.getAbsolutePath());
				Matcher m = mImgFilenamePat.matcher(f.getAbsolutePath());
				if(m.matches()) {
					mImgs.addPath(f.getAbsolutePath());
					// give some time to UI thread, maybe it will draw something 
					try {
						Thread.sleep(123);
						} catch(java.lang.InterruptedException e) {}
				}
			}
		}
		
	}
}
		

class SdImages implements ListAdapter {
	private static final String TAG = "il";
	private static final BitmapFactory mBmpFcy = new BitmapFactory();
	
	// holding the whole Activity instead of Context, bc Activity can 
	// run things on UI thread
	private Activity mCt;

	private ArrayList mPaths = new ArrayList();
	private DataSetObserver mDso = null;
	private final Paint mErrMsgPaint = new Paint();


	public SdImages(Activity ct) {
		mCt = ct;

		mErrMsgPaint.setColor(0xFFFF0000);
		mErrMsgPaint.setStyle(Paint.Style.FILL);
		mErrMsgPaint.setTextSize(30);
	
		SdImagesWorkerThread wt = new SdImagesWorkerThread(this);
		wt.start();
	}

	public int getCount() {
		return mPaths.size();
	}

	public String getItem(int pos) {
		return (String)mPaths.get(pos);
	}

	public long getItemId(int p) {
		return p;
	}

	public int getItemViewType (int position) {
		return 0;
	}
	
	public View getView (int position, View convertView, ViewGroup parent) {
		String errMsg = null;
		LinearLayout v = (LinearLayout)convertView;
		if(null == v) {
			v = new LinearLayout(mCt);
			v.setOrientation(LinearLayout.VERTICAL);
			v.addView(new ImageView(mCt), 0);
			v.addView(new TextView(mCt), 1);
		}
		TextView label = (TextView)v.getChildAt(1);
		String path = (String)mPaths.get(position);
		label.setText(path);
		ImageView pic = (ImageView)v.getChildAt(0);
		pic.setAdjustViewBounds(true);
		pic.setMaxWidth(100);
		pic.setMaxHeight(100);
		Log.d(TAG, "Decoding " + path);
		try {
			Bitmap b = mBmpFcy.decodeFile(path);
			if(b != null) {
				Bitmap th = Bitmap.createScaledBitmap(b, 100, 100, false);
				pic.setImageBitmap(th);
			} else {
				errMsg = "Somehow undecodable";
			}
		} catch(OutOfMemoryError e) {
			// J2SE doc says a reasonable application shouldn't try to 
			// catch a java.lang.Error
			//
			// On the other hand, if a big allocation inside decodeFile()
			// fails, we still have all the memory we had before we called
			// decodeFile(), and if we don't catch this here, it would
			// jump at user, who definitely doesn't want that.
			errMsg = "Not enough memory";
		}

		if(errMsg != null) {
			Bitmap m = Bitmap.createBitmap(200, 60, Bitmap.Config.ARGB_8888);
			Canvas c = new Canvas(m);
			c.drawText(errMsg, 0, errMsg.length(), 0, 20, mErrMsgPaint);
			pic.setImageBitmap(m);
		}

		return v;
	}

	public int getViewTypeCount () {
		return 1;
	}

	public boolean hasStableIds() {
		return true;
	}

	public boolean isEmpty() {
		return mPaths.isEmpty();
	}

	public void registerDataSetObserver(DataSetObserver o) {
		mDso = o;
	}

	public void unregisterDataSetObserver(DataSetObserver o) {
		mDso = null;
	}

	public boolean areAllItemsEnabled() {
		return true;
	}

	public boolean isEnabled(int p) {
		return true;
	}

	public void addPath(final String path) {
		mCt.runOnUiThread(new Runnable() {
				public void run() {
					mPaths.add(path);
					if(mDso != null)
						mDso.onChanged();
				}
			});
	}
}


public class ImageList extends ListActivity
{
	@Override
		public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setListAdapter(new SdImages(this));
	}
}
