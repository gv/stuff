/*
 * Copyright (C) 2007 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
	TODO
	----

	DirectoryImageList;
	optimize memory usage somehow;
	draw some header with info;
	move "next" and "prev" buttons out of an image;
	pinch zoom;
	fling;
*/


package vg.panes;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.graphics.Bitmap;
import android.net.Uri;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.provider.MediaStore;
import android.util.AttributeSet;
import android.util.Log;
import android.view.GestureDetector;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;
import android.view.View.OnTouchListener;
import android.view.animation.AlphaAnimation;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.Toast;
import android.widget.ZoomButtonsController;

import java.util.Random;
import java.util.ArrayList;
import java.util.List;
import java.util.Arrays;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.RectF;
import android.text.Layout;
import android.util.AttributeSet;
import android.widget.TextView;


import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.view.View;
import android.view.ViewGroup;

import android.widget.ImageView;
import android.graphics.ColorFilter;
import android.graphics.PixelFormat;


import android.graphics.Rect;

public class ViewImage extends NoSearchActivity implements View.OnClickListener {
	public static final String KEY_IMAGE_LIST = "image_list";
	private static final String STATE_SHOW_CONTROLS = "show_controls";
	private static final String STATE_URI = "uri";
	private static final String TAG = "ViewImage";

	private ImageGetter mGetter;
	private Uri mSavedUri;
	boolean mPaused = true;
	private boolean mShowControls = true;

	// Choices for what adjacents to load.
	private static final int[] sOrderAdjacents = new int[] {0, 1, -1};

	final GetterHandler mHandler = new GetterHandler();

	private boolean mShowActionIcons;

	int mCurrentPosition = 0;

	private SharedPreferences mPrefs;

	IImageList mAllImages;

	private ImageManager.ImageListParam mParam;

	GestureDetector mGestureDetector;
	private ZoomButtonsController mZoomButtonsController;

	// The image view displayed for normal mode.
	private ImageViewTouch mImageView;
	// This is the cache for thumbnail bitmaps.
	private BitmapCache mCache;
	// private MenuHelper.MenuItemsResult mImageMenuRunnable;
	private final Runnable mDismissOnScreenControlRunner = new Runnable() {
			public void run() {
				hideOnScreenControls();
			}
    };

	private void hideOnScreenControls() {
		mZoomButtonsController.setVisible(false);
	}

	private void showOnScreenControls() {
		if (mPaused) return;
		// If the view has not been attached to the window yet, the
		// zoomButtonControls will not able to show up. So delay it until the
		// view has attached to window.
		/*if (mActionIconPanel.getWindowToken() == null) {
			mHandler.postGetterCallback(new Runnable() {
					public void run() {
						showOnScreenControls();
					}
				});
			return;
		}
		*/

		IImage image = mAllImages.getImageAt(mCurrentPosition);
		updateZoomButtonsEnabled();
		mZoomButtonsController.setVisible(true);
	}

	@Override
    public boolean dispatchTouchEvent(MotionEvent m) {
		if (mPaused) return true;
		if (mZoomButtonsController.isVisible()) {
			scheduleDismissOnScreenControls();
		}
		return super.dispatchTouchEvent(m);
	}

	private void updateZoomButtonsEnabled() {
		ImageViewTouch imageView = mImageView;
		float scale = imageView.getScale();
		mZoomButtonsController.setZoomInEnabled(scale < imageView.mMaxZoom);
		mZoomButtonsController.setZoomOutEnabled(scale > 1);
	}

	@Override
    protected void onDestroy() {
		// This is necessary to make the ZoomButtonsController unregister
		// its configuration change receiver.
		if (mZoomButtonsController != null) {
			mZoomButtonsController.setVisible(false);
		}
		super.onDestroy();
	}

	private void scheduleDismissOnScreenControls() {
		mHandler.removeCallbacks(mDismissOnScreenControlRunner);
		mHandler.postDelayed(mDismissOnScreenControlRunner, 2000);
	}

	private void setupOnScreenControls(View rootView, View ownerView) {
		setupZoomButtonController(ownerView);
		setupOnTouchListeners(rootView);
	}

	private void setupZoomButtonController(final View ownerView) {
		mZoomButtonsController = new ZoomButtonsController(ownerView);
		mZoomButtonsController.setAutoDismissed(false);
		mZoomButtonsController.setZoomSpeed(100);
		mZoomButtonsController.setOnZoomListener(
			new ZoomButtonsController.OnZoomListener() {
				public void onVisibilityChanged(boolean visible) {
					if (visible) {
						updateZoomButtonsEnabled();
					}
				}

				public void onZoom(boolean zoomIn) {
					if (zoomIn) {
						mImageView.zoomIn();
					} else {
						mImageView.zoomOut();
					}
					mZoomButtonsController.setVisible(true);
					updateZoomButtonsEnabled();
				}
			});
	}

	private void setupOnTouchListeners(View rootView) {
		mGestureDetector = new GestureDetector(this, new MyGestureListener());

		// If the user touches anywhere on the panel (including the
		// next/prev button). We show the on-screen controls. In addition
		// to that, if the touch is not on the prev/next button, we
		// pass the event to the gesture detector to detect double tap.
		final OnTouchListener buttonListener = new OnTouchListener() {
				public boolean onTouch(View v, MotionEvent event) {
					scheduleDismissOnScreenControls();
					return false;
				}
			};

		OnTouchListener rootListener = new OnTouchListener() {
				public boolean onTouch(View v, MotionEvent event) {
					buttonListener.onTouch(v, event);
					mGestureDetector.onTouchEvent(event);

					// We do not use the return value of
					// mGestureDetector.onTouchEvent because we will not receive
					// the "up" event if we return false for the "down" event.
					return true;
				}
			};

		rootView.setOnTouchListener(rootListener);
	}

	private class MyGestureListener 
		extends	GestureDetector.SimpleOnGestureListener {

		@Override
			public boolean onScroll(MotionEvent e1, MotionEvent e2,
				float distanceX, float distanceY) {
			if (mPaused) return false;
			ImageViewTouch imageView = mImageView;
			if (imageView.getScale() > 1F) {
				imageView.postTranslateCenter(-distanceX, -distanceY);
			}
			return true;
		}

		@Override
			public boolean onSingleTapUp(MotionEvent e) {
			if (mPaused) return false;


			return true;
		}

		@Override
			public boolean onSingleTapConfirmed(MotionEvent e) {
			if (mPaused) return false;
			showOnScreenControls();
			scheduleDismissOnScreenControls();
			return true;
		}

		@Override
			public boolean onDoubleTap(MotionEvent e) {
			if (mPaused) return false;
			ImageViewTouch imageView = mImageView;

			// Switch between the original scale and 3x scale.
			if (imageView.getScale() > 2F) {
				mImageView.zoomTo(1f);
			} else {
				mImageView.zoomToPoint(3f, e.getX(), e.getY());
			}
			return true;
		}
	}

	boolean isPickIntent() {
		String action = getIntent().getAction();
		return (Intent.ACTION_PICK.equals(action)
			|| Intent.ACTION_GET_CONTENT.equals(action));
	}

	@Override
    public boolean onCreateOptionsMenu(Menu menu) {
		super.onCreateOptionsMenu(menu);

		return true;
	}

	@Override
    public boolean onPrepareOptionsMenu(Menu menu) {
		super.onPrepareOptionsMenu(menu);
		if (mPaused) return false;

		return true;
	}

	@Override
    public boolean onMenuItemSelected(int featureId, MenuItem item) {
		boolean b = super.onMenuItemSelected(featureId, item);
		return b;
	}

	void setImage(int pos, boolean showControls) {
		mCurrentPosition = pos;
		IImage image = mAllImages.getImageAt(pos);
		final String path = image.getDataPath();

		Bitmap cachedThumb = mCache.getBitmap(pos);
		if (cachedThumb != null) {
			mImageView.setImageRotateBitmapResetBase(
				new RotateBitmap(cachedThumb, image.getDegreesRotated()), true);
			mImageView.mPath = path;
			updateZoomButtonsEnabled();
		}

		ImageGetterCallback cb = new ImageGetterCallback() {
				public void completed() {
				}

				public boolean wantsThumbnail(int pos, int offset) {
					return !mCache.hasBitmap(pos + offset);
				}

				public boolean wantsFullImage(int pos, int offset) {
					return offset == 0;
				}

				public int fullImageSizeToUse(int pos, int offset) {
					// this number should be bigger so that we can zoom.  we may
					// need to get fancier and read in the fuller size image as the
					// user starts to zoom.
					// Originally the value is set to 480 in order to avoid OOM.
					// Now we set it to 2048 because of using
					// native memory allocation for Bitmaps.
					final int imageViewSize = 2048;
					return imageViewSize;
				}

				public int [] loadOrder() {
					return sOrderAdjacents;
				}

				public void imageLoaded(int pos, int offset, RotateBitmap bitmap,
					boolean isThumb) {
					// shouldn't get here after onPause()

					// We may get a result from a previous request. Ignore it.
					if (pos != mCurrentPosition) {
						bitmap.recycle();
						return;
					}

					if (isThumb) {
						mCache.put(pos + offset, bitmap.getBitmap());
					}
					if (offset == 0) {
						// isThumb: We always load thumb bitmap first, so we will
						// reset the supp matrix for then thumb bitmap, and keep
						// the supp matrix when the full bitmap is loaded.
						mImageView.setImageRotateBitmapResetBase(bitmap, isThumb);
						mImageView.mPath = path;
						updateZoomButtonsEnabled();

						//if(mPaneNum >= 0) 
						//	mImageView.showRect(mPanes.get(mPaneNum));
					}
				}

				public void detectingPanes(Rect area, RotateBitmap bitmap) {
					mImageView.mDrawDebugInfo = true;
					mImageView.mDetectionArea = area;
					mImageView.invalidate();
				}	
						
				public void paneDetected(Rect pane, RotateBitmap bitmap) {
					mImageView.mPanes.add(pane);
					mImageView.invalidate();
				}

				public void panesDetectionComplete(Rect[] panes, RotateBitmap bitmap) {
					mImageView.mDrawDebugInfo = false;
					mImageView.mPanes = Arrays.asList(panes);
					mImageView.setPaneNum(0);
				}
			};

		//mPaneNum = -1;
		
		// Could be null if we're stopping a slide show in the course of pausing
		if (mGetter != null) {
			mGetter.setPosition(pos, cb, mAllImages, mHandler);
		}
		if (showControls) showOnScreenControls();
		scheduleDismissOnScreenControls();
	}

	@Override
    public void onCreate(Bundle instanceState) {
		super.onCreate(instanceState);

		Intent intent = getIntent();
		mShowActionIcons = intent.getBooleanExtra(
			MediaStore.EXTRA_SHOW_ACTION_ICONS, true);

		mPrefs = PreferenceManager.getDefaultSharedPreferences(this);

		setDefaultKeyMode(DEFAULT_KEYS_SHORTCUT);
		requestWindowFeature(Window.FEATURE_NO_TITLE);
		setContentView(R.layout.viewimage);

		mImageView = (ImageViewTouch) findViewById(R.id.image);
		mImageView.setEnableTrackballScroll(true);
		mCache = new BitmapCache(3);
		mImageView.setRecycler(mCache);

		makeGetter();

		mParam = getIntent().getParcelableExtra(KEY_IMAGE_LIST);

		if (instanceState != null) {
			mSavedUri = instanceState.getParcelable(STATE_URI);
			mShowControls = instanceState.getBoolean(STATE_SHOW_CONTROLS, true);
		} else {
			mSavedUri = getIntent().getData();
		}

		getWindow().addFlags(
			WindowManager.LayoutParams.FLAG_FULLSCREEN);

		setupOnScreenControls(findViewById(R.id.rootLayout), mImageView);
	}

	private static int getPreferencesInteger(
		SharedPreferences prefs, String key, int defaultValue) {
		String value = prefs.getString(key, null);
		try {
			return value == null ? defaultValue : Integer.parseInt(value);
		} catch (NumberFormatException ex) {
			Log.e(TAG, "couldn't parse preference: " + value, ex);
			return defaultValue;
		}
	}

	private void makeGetter() {
		mGetter = new ImageGetter(getContentResolver());
	}

	private boolean init(Uri uri) {
		if (uri == null) return false;
		if(mParam != null) {
			mAllImages = ImageManager.makeImageList(getContentResolver(), mParam);
		} else {
			// common case
			mAllImages = ImageManager.makeImageList(
				getContentResolver(), uri, ImageManager.SORT_ASCENDING);
		}

		// DEBUG PRINT
		for(int i = mAllImages.getCount() - 1; i >= 0; i--) {
			IImage m = mAllImages.getImageAt(i);
			Log.d(TAG, "mAllImages: " + m.fullSizeImageUri() + " " + 
				m.getDataPath());
		}
		// END 
				
		IImage image = mAllImages.getImageForUri(uri);
		if (image == null) return false;
		mCurrentPosition = mAllImages.getImageIndex(image);
		return true;
	}

	private Uri getCurrentUri() {
		if (mAllImages.getCount() == 0) return null;
		IImage image = mAllImages.getImageAt(mCurrentPosition);
		if (image == null) return null;
		return image.fullSizeImageUri();
	}

	@Override
    public void onSaveInstanceState(Bundle b) {
		super.onSaveInstanceState(b);
		b.putParcelable(STATE_URI,
			mAllImages.getImageAt(mCurrentPosition).fullSizeImageUri());
	}

	@Override
    public void onStart() {
		super.onStart();
		mPaused = false;

		if (!init(mSavedUri)) {
			Log.w(TAG, "init failed: " + mSavedUri);
			finish();
			return;
		}

		// normally this will never be zero but if one "backs" into this
		// activity after removing the sdcard it could be zero.  in that
		// case just "finish" since there's nothing useful that can happen.
		int count = mAllImages.getCount();
		if (count == 0) {
			finish();
			return;
		} else if (count <= mCurrentPosition) {
			mCurrentPosition = count - 1;
		}

		if (mGetter == null) {
			makeGetter();
		}


		setImage(mCurrentPosition, mShowControls);
		mShowControls = false;
	}

	@Override
    public void onStop() {
		super.onStop();
		mPaused = true;

		// mGetter could be null if we call finish() and leave early in
		// onStart().
		if (mGetter != null) {
			mGetter.cancelCurrent();
			mGetter.stop();
			mGetter = null;
		}

		// removing all callback in the message queue
		mHandler.removeAllGetterCallbacks();

		if (mAllImages != null) {
			mSavedUri = getCurrentUri();
			mAllImages.close();
			mAllImages = null;
		}

		hideOnScreenControls();
		mImageView.clear();
		mCache.clear();

	}

	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.next_image:
			moveNextOrPrevious(1);
			break;
		case R.id.prev_image:
			moveNextOrPrevious(-1);
			break;
		}
	}

	private void moveNextOrPrevious(int delta) {
		int nextImagePos = mCurrentPosition + delta;
		if ((0 <= nextImagePos) && (nextImagePos < mAllImages.getCount())) {
			setImage(nextImagePos, true);
			showOnScreenControls();
		}
	}

}

// This is made external class to be initialized in XML layout description
class ImageViewTouch extends ImageViewTouchBase {
	private final ViewImage mViewImage;
	private boolean mEnableTrackballScroll;
	public int mPaneNum = -1;
	public List<Rect> mPanes = new ArrayList<Rect>();
	public Rect mDetectionArea;
	public boolean mDrawDebugInfo = false;
	public String mPath;

	public void setPaneNum(int n) {
		if(n == mPaneNum)
			return;
		
		if(n >= mPanes.size())
			return;

		if(n < 0) {
			showRect(new Rect(0, 0, mBitmapDisplayed.getWidth(), 
					mBitmapDisplayed.getHeight()));
			mPaneNum = -1;
			return;
		}

		showRect(mPanes.get(n));
		mPaneNum = n;
	}


	public ImageViewTouch(Context context) {
		super(context);
		mViewImage = (ViewImage) context;
	}

	public ImageViewTouch(Context context, AttributeSet attrs) {
		super(context, attrs);
		mViewImage = (ViewImage) context;
	}

	public void setEnableTrackballScroll(boolean enable) {
		mEnableTrackballScroll = enable;
	}

	protected void postTranslateCenter(float dx, float dy) {
		super.postTranslate(dx, dy);
		center(true, true);
	}

	private static final float PAN_RATE = 20;

	// This is the time we allow the dpad to change the image position again.
	private long mNextChangePositionTime;

	@Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
		if (mViewImage.mPaused) return false;

		int current = mViewImage.mCurrentPosition;

		int nextImagePos = -2; // default no next image
		try {
			switch (keyCode) {
			case KeyEvent.KEYCODE_DPAD_DOWN: 
			case KeyEvent.KEYCODE_DPAD_RIGHT: 
			case KeyEvent.KEYCODE_DPAD_CENTER: 
			case KeyEvent.KEYCODE_SEARCH:
				if(mPaneNum == mPanes.size() - 1)
					nextImagePos = current + 1;
				setPaneNum(mPaneNum + 1);
				return true;

			case KeyEvent.KEYCODE_DPAD_UP: 
			case KeyEvent.KEYCODE_DPAD_LEFT: 
			case KeyEvent.KEYCODE_BACK:
				if(mPaneNum == -1)
					nextImagePos = current - 1;
				setPaneNum(mPaneNum - 1);
				return true;
			}
		} finally {
			if (nextImagePos >= 0
				&& nextImagePos < mViewImage.mAllImages.getCount()) {
				synchronized (mViewImage) {
					mViewImage.setImage(nextImagePos, true);
				}
			} 
		}

		Log.d("Sending keypress to ImageViewTouch.onKeyDown", 
			String.format("e: %d", keyCode));
		return super.onKeyDown(keyCode, event);
	}

	protected void onDraw(Canvas c) {
		super.onDraw(c);
	
		if(mBitmapDisplayed.getBitmap() == null)
			return;

		RectF head = new RectF(0, -50, mBitmapDisplayed.getWidth(), 0);
		getImageViewMatrix().mapRect(head);
		
		int headHeight = getHeight() / 12;

		if(head.bottom <= 1) {
			head = new RectF(0, mBitmapDisplayed.getHeight(),
				mBitmapDisplayed.getWidth(), mBitmapDisplayed.getHeight() + 50);
			getImageViewMatrix().mapRect(head);
			head.bottom = head.top + headHeight;
		} else {
			head.top = head.bottom - headHeight;
		}

		Paint headBgPaint = new Paint();
		headBgPaint.setColor(0xFFe0e0e0);
		c.drawRect(head, headBgPaint);

		Paint headTextPaint = new Paint();
		headTextPaint.setColor(0xFF336644);
		if(mPath != null) {
			c.drawText(mPath, head.left, head.bottom, headTextPaint);
		}

		if(!mDrawDebugInfo)
			return;
		Paint p = new Paint();
		p.setColor(0xE0FF0000);
		p.setStyle(Paint.Style.STROKE);
		p.setStrokeWidth(7);

		RectF displayedRect;
		for(Rect r: mPanes) {
			displayedRect = new RectF(r);
			getImageViewMatrix().mapRect(displayedRect);
			c.drawRoundRect(displayedRect, 2, 2, p);
		}

		if(mDetectionArea != null) {
			Paint detectionAreaPaint = new Paint();
			detectionAreaPaint.setColor(0xE000FFFF);
			detectionAreaPaint.setStyle(Paint.Style.STROKE);
			detectionAreaPaint.setStrokeWidth(7);
			
			displayedRect = new RectF(mDetectionArea);
			getImageViewMatrix().mapRect(displayedRect);
			c.drawRoundRect(displayedRect, 2, 2, detectionAreaPaint);
		}
	}

	public void setImageRotateBitmapResetBase(RotateBitmap b, boolean isThumb) {
		mPanes = new ArrayList<Rect>();
		super.setImageRotateBitmapResetBase(b, isThumb);
	}
}

// This is a cache for Bitmap displayed in ViewImage (normal mode, thumb only).
class BitmapCache implements ImageViewTouchBase.Recycler {
	public static class Entry {
		int mPos;
		Bitmap mBitmap;
		public Entry() {
			clear();
		}
		public void clear() {
			mPos = -1;
			mBitmap = null;
		}
	}

	private final Entry[] mCache;

	public BitmapCache(int size) {
		mCache = new Entry[size];
		for (int i = 0; i < mCache.length; i++) {
			mCache[i] = new Entry();
		}
	}

	// Given the position, find the associated entry. Returns null if there is
	// no such entry.
	private Entry findEntry(int pos) {
		for (Entry e : mCache) {
			if (pos == e.mPos) {
				return e;
			}
		}
		return null;
	}

	// Returns the thumb bitmap if we have it, otherwise return null.
	public synchronized Bitmap getBitmap(int pos) {
		Entry e = findEntry(pos);
		if (e != null) {
			return e.mBitmap;
		}
		return null;
	}

	public synchronized void put(int pos, Bitmap bitmap) {
		// First see if we already have this entry.
		if (findEntry(pos) != null) {
			return;
		}

		// Find the best entry we should replace.
		// See if there is any empty entry.
		// Otherwise assuming sequential access, kick out the entry with the
		// greatest distance.
		Entry best = null;
		int maxDist = -1;
		for (Entry e : mCache) {
			if (e.mPos == -1) {
				best = e;
				break;
			} else {
				int dist = Math.abs(pos - e.mPos);
				if (dist > maxDist) {
					maxDist = dist;
					best = e;
				}
			}
		}

		// Recycle the image being kicked out.
		// This only works because our current usage is sequential, so we
		// do not happen to recycle the image being displayed.
		if (best.mBitmap != null) {
			best.mBitmap.recycle();
		}

		best.mPos = pos;
		best.mBitmap = bitmap;
	}

	// Recycle all bitmaps in the cache and clear the cache.
	public synchronized void clear() {
		for (Entry e : mCache) {
			if (e.mBitmap != null) {
				e.mBitmap.recycle();
			}
			e.clear();
		}
	}

	// Returns whether the bitmap is in the cache.
	public synchronized boolean hasBitmap(int pos) {
		Entry e = findEntry(pos);
		return (e != null);
	}

	// Recycle the bitmap if it's not in the cache.
	// The input must be non-null.
	public synchronized void recycle(Bitmap b) {
		for (Entry e : mCache) {
			if (e.mPos != -1) {
				if (e.mBitmap == b) {
					return;
				}
			}
		}
		b.recycle();
	}
}


class ActionMenuButton extends TextView {
	private static final int CORNER_RADIUS = 8;
	private static final int PADDING_H = 5;
	private static final int PADDING_V = 1;

	private final RectF mRect = new RectF();
	private Paint mPaint;

	public ActionMenuButton(Context context) {
		super(context);
		init();
	}

	public ActionMenuButton(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public ActionMenuButton(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	private void init() {
		setFocusable(true);
		// We need extra padding below to prevent the bubble being cut.
		setPadding(PADDING_H, 0, PADDING_H, PADDING_V);

		mPaint = new Paint(Paint.ANTI_ALIAS_FLAG);
		mPaint.setColor(getContext().getResources()
			.getColor(R.color.bubble_dark_background));
	}

	@Override
    protected void drawableStateChanged() {
		invalidate();
		super.drawableStateChanged();
	}

	@Override
    public void draw(Canvas canvas) {
		final Layout layout = getLayout();
		final RectF rect = mRect;
		final int left = getCompoundPaddingLeft();
		final int top = getExtendedPaddingTop();

		rect.set(left + layout.getLineLeft(0) - PADDING_H,
			top + layout.getLineTop(0) - PADDING_V,
			Math.min(left + layout.getLineRight(0) + PADDING_H,
				getScrollX() + getRight() - getLeft()),
			top + layout.getLineBottom(0) + PADDING_V);
		canvas.drawRoundRect(rect, CORNER_RADIUS, CORNER_RADIUS, mPaint);

		super.draw(canvas);
	}
}

//
// This is a layout which makes the children even spaced.
// Currently it does not consider the padding parameters.
//
class EvenlySpacedLayout extends ViewGroup {
	private boolean mHorizontal;

	// Wheather we keep the space in both ends of the layout
	private boolean mKeepEndSpace;

	public EvenlySpacedLayout(Context context, AttributeSet attrs) {
		super(context, attrs);
		TypedArray a = context.obtainStyledAttributes(
			attrs, R.styleable.EvenlySpacedLayout, 0, 0);
		mHorizontal = (0 == a.getInt(
				R.styleable.EvenlySpacedLayout_orientation, 0));
		mKeepEndSpace = a.getBoolean(
			R.styleable.EvenlySpacedLayout_keepEndSpace, true);
		a.recycle();
	}

	@Override
    protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
		int count = getChildCount();
		int width = 0;
		int height = 0;
		for (int i = 0; i < count; i++) {
			View child = getChildAt(i);
			if (child.getVisibility() == GONE) continue;
			measureChild(child, widthMeasureSpec, heightMeasureSpec);
			if (mHorizontal) {
				width += child.getMeasuredWidth();
				height = Math.max(height, child.getMeasuredHeight());
			} else {
				height += child.getMeasuredHeight();
				width = Math.max(width, child.getMeasuredWidth());
			}
		}
		setMeasuredDimension(resolveSize(width, widthMeasureSpec),
			resolveSize(height, heightMeasureSpec));
	}

	private void layoutHorizontal(boolean changed, int l, int t, int r, int b) {
		int count = getChildCount();

		int usedWidth = 0;
		int usedChildren = 0;
		for (int i = 0; i < count; i++) {
			View child = getChildAt(i);
			if (child.getVisibility() == GONE) continue;
			usedWidth += child.getMeasuredWidth();
			++usedChildren;
		}

		int spacing = (r - l - usedWidth) /
			(mKeepEndSpace ? (usedChildren + 1) : (usedChildren - 1));
		int left = mKeepEndSpace ? spacing : 0;
		int top = 0;
		for (int i = 0; i < count; i++) {
			View child = getChildAt(i);
			if (child.getVisibility() == GONE) continue;
			int w = child.getMeasuredWidth();
			int h = child.getMeasuredHeight();
			child.layout(left, top, left + w, top + h);
			left += w;
			left += spacing;
		}
	}

	private void layoutVertical(boolean changed, int l, int t, int r, int b) {
		int count = getChildCount();

		int usedHeight = 0;
		int usedChildren = 0;
		for (int i = 0; i < count; i++) {
			View child = getChildAt(i);
			if (child.getVisibility() == GONE) continue;
			usedHeight += child.getMeasuredHeight();
			++usedChildren;
		}

		int spacing = (b - t - usedHeight) /
			(mKeepEndSpace ? (usedChildren + 1) : (usedChildren - 1));
		int top = mKeepEndSpace ? spacing : 0;
		int left = 0;
		for (int i = 0; i < count; i++) {
			View child = getChildAt(i);
			if (child.getVisibility() == GONE) continue;
			int w = child.getMeasuredWidth();
			int h = child.getMeasuredHeight();
			child.layout(left, top, left + w, top + h);
			top += h;
			top += spacing;
		}
	}

	@Override
    protected void onLayout(boolean changed, int l, int t, int r, int b) {
		if (mHorizontal) {
			layoutHorizontal(changed, l, t, r, b);
		} else {
			layoutVertical(changed, l, t, r, b);
		}
	}
}
