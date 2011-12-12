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
	private static final String STATE_URI = "uri";
	private static final String TAG = "ViewImage";

	private ImageGetter mGetter;
	private Uri mSavedUri;
	boolean mPaused = true;
	private boolean mShowControls = true;

	// Choices for what adjacents to load.
	private static final int[] sOrderAdjacents = new int[] {0, 1, -1};

	final GetterHandler mHandler = new GetterHandler();

	private final Random mRandom = new Random(System.currentTimeMillis());
	private int [] mShuffleOrder = null;
	private boolean mUseShuffleOrder = false;
	private boolean mSlideShowLoop = false;

	static final int MODE_NORMAL = 1;
	private int mMode = MODE_NORMAL;

	private boolean mFullScreenInNormalMode;
	private boolean mShowActionIcons;
	private View mActionIconPanel;

	private int mSlideShowInterval;
	private int mLastSlideShowImage;
	int mCurrentPosition = 0;

	// represents which style animation to use
	private int mAnimationIndex;
	private Animation [] mSlideShowInAnimation;
	private Animation [] mSlideShowOutAnimation;

	private SharedPreferences mPrefs;

	private View mNextImageView;
	private View mPrevImageView;
	private final Animation mHideNextImageViewAnimation =
		new AlphaAnimation(1F, 0F);
	private final Animation mHidePrevImageViewAnimation =
		new AlphaAnimation(1F, 0F);
	private final Animation mShowNextImageViewAnimation =
		new AlphaAnimation(0F, 1F);
	private final Animation mShowPrevImageViewAnimation =
		new AlphaAnimation(0F, 1F);

	public static final String KEY_IMAGE_LIST = "image_list";
	private static final String STATE_SHOW_CONTROLS = "show_controls";

	IImageList mAllImages;

	private ImageManager.ImageListParam mParam;

	private int mSlideShowImageCurrent = 0;
	private final ImageViewTouchBase [] mSlideShowImageViews =
		new ImageViewTouchBase[2];

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

	private void updateNextPrevControls() {
		boolean showPrev = mCurrentPosition > 0;
		boolean showNext = mCurrentPosition < mAllImages.getCount() - 1;

		boolean prevIsVisible = mPrevImageView.getVisibility() == View.VISIBLE;
		boolean nextIsVisible = mNextImageView.getVisibility() == View.VISIBLE;

		if (showPrev && !prevIsVisible) {
			Animation a = mShowPrevImageViewAnimation;
			a.setDuration(500);
			mPrevImageView.startAnimation(a);
			mPrevImageView.setVisibility(View.VISIBLE);
		} else if (!showPrev && prevIsVisible) {
			Animation a = mHidePrevImageViewAnimation;
			a.setDuration(500);
			mPrevImageView.startAnimation(a);
			mPrevImageView.setVisibility(View.GONE);
		}

		if (showNext && !nextIsVisible) {
			Animation a = mShowNextImageViewAnimation;
			a.setDuration(500);
			mNextImageView.startAnimation(a);
			mNextImageView.setVisibility(View.VISIBLE);
		} else if (!showNext && nextIsVisible) {
			Animation a = mHideNextImageViewAnimation;
			a.setDuration(500);
			mNextImageView.startAnimation(a);
			mNextImageView.setVisibility(View.GONE);
		}
	}

	private void hideOnScreenControls() {
		if (mShowActionIcons
			&& mActionIconPanel.getVisibility() == View.VISIBLE) {
			Animation animation = new AlphaAnimation(1, 0);
			animation.setDuration(500);
			mActionIconPanel.startAnimation(animation);
			mActionIconPanel.setVisibility(View.INVISIBLE);
		}

		if (mNextImageView.getVisibility() == View.VISIBLE) {
			Animation a = mHideNextImageViewAnimation;
			a.setDuration(500);
			mNextImageView.startAnimation(a);
			mNextImageView.setVisibility(View.INVISIBLE);
		}

		if (mPrevImageView.getVisibility() == View.VISIBLE) {
			Animation a = mHidePrevImageViewAnimation;
			a.setDuration(500);
			mPrevImageView.startAnimation(a);
			mPrevImageView.setVisibility(View.INVISIBLE);
		}

		mZoomButtonsController.setVisible(false);
	}

	private void showOnScreenControls() {
		if (mPaused) return;
		// If the view has not been attached to the window yet, the
		// zoomButtonControls will not able to show up. So delay it until the
		// view has attached to window.
		if (mActionIconPanel.getWindowToken() == null) {
			mHandler.postGetterCallback(new Runnable() {
					public void run() {
						showOnScreenControls();
					}
				});
			return;
		}
		updateNextPrevControls();

		IImage image = mAllImages.getImageAt(mCurrentPosition);
		/*if (image instanceof VideoObject) {
			mZoomButtonsController.setVisible(false);
			} else {*/
		updateZoomButtonsEnabled();
		mZoomButtonsController.setVisible(true);
		//}

		if (mShowActionIcons
			&& mActionIconPanel.getVisibility() != View.VISIBLE) {
			Animation animation = new AlphaAnimation(0, 1);
			animation.setDuration(500);
			mActionIconPanel.startAnimation(animation);
			mActionIconPanel.setVisibility(View.VISIBLE);
		}
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
		mNextImageView = rootView.findViewById(R.id.next_image);
		mPrevImageView = rootView.findViewById(R.id.prev_image);

		mNextImageView.setOnClickListener(this);
		mPrevImageView.setOnClickListener(this);

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

		mNextImageView.setOnTouchListener(buttonListener);
		mPrevImageView.setOnTouchListener(buttonListener);
		rootView.setOnTouchListener(rootListener);
	}

	private class MyGestureListener extends
																		GestureDetector.SimpleOnGestureListener {

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
			setMode(MODE_NORMAL);
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

		/* mImageMenuRunnable = MenuHelper.addImageMenuItems(
			 menu,
			 MenuHelper.INCLUDE_ALL,
			 ViewImage.this,
			 mHandler,
			 mDeletePhotoRunnable,
			 new MenuHelper.MenuInvoker() {
			 public void run(final MenuHelper.MenuCallback cb) {
			 if (mPaused) return;
			 setMode(MODE_NORMAL);

			 IImage image = mAllImages.getImageAt(mCurrentPosition);
			 Uri uri = image.fullSizeImageUri();
			 cb.run(uri, image);

			 // We might have deleted all images in the callback, so
			 // call setImage() only if we still have some images.
			 if (mAllImages.getCount() > 0) {
			 mImageView.clear();
			 setImage(mCurrentPosition, false);
			 }
			 }
			 });

			 item = menu.add(Menu.NONE, Menu.NONE,
			 MenuHelper.POSITION_GALLERY_SETTING, R.string.camerasettings);
			 item.setOnMenuItemClickListener(
			 new MenuItem.OnMenuItemClickListener() {
			 public boolean onMenuItemClick(MenuItem item) {
			 Intent preferences = new Intent();
			 preferences.setClass(ViewImage.this, GallerySettings.class);
			 startActivity(preferences);
			 return true;
			 }
			 });
			 item.setAlphabeticShortcut('p');
			 item.setIcon(android.R.drawable.ic_menu_preferences);
		*/
		return true;
	}

	protected Runnable mDeletePhotoRunnable = new Runnable() {
			public void run() {
				mAllImages.removeImageAt(mCurrentPosition);
				if (mAllImages.getCount() == 0) {
					finish();
					return;
				} else {
					if (mCurrentPosition == mAllImages.getCount()) {
						mCurrentPosition -= 1;
					}
				}
				mImageView.clear();
				mCache.clear();  // Because the position number is changed.
				setImage(mCurrentPosition, true);
			}
    };

	@Override
    public boolean onPrepareOptionsMenu(Menu menu) {

		super.onPrepareOptionsMenu(menu);
		if (mPaused) return false;

		setMode(MODE_NORMAL);
		IImage image = mAllImages.getImageAt(mCurrentPosition);

		/*if (mImageMenuRunnable != null) {
			mImageMenuRunnable.gettingReadyToOpen(menu, image);
			}

			Uri uri = mAllImages.getImageAt(mCurrentPosition).fullSizeImageUri();
			MenuHelper.enableShareMenuItem(menu, MenuHelper.isWhiteListUri(uri));

			MenuHelper.enableShowOnMapMenuItem(menu, MenuHelper.hasLatLngData(image));
		*/
		return true;
	}

	@Override
    public boolean onMenuItemSelected(int featureId, MenuItem item) {
		boolean b = super.onMenuItemSelected(featureId, item);
		/*if (mImageMenuRunnable != null) {
			mImageMenuRunnable.aboutToCall(item,
			mAllImages.getImageAt(mCurrentPosition));
			}*/
		return b;
	}

	void setImage(int pos, boolean showControls) {
		mCurrentPosition = pos;

		mBmp = null;
		mGraphicInfoView.setVisibility(View.GONE);
		mPanes = new ArrayList<Rect>();

		Bitmap b = mCache.getBitmap(pos);
		if (b != null) {
			IImage image = mAllImages.getImageAt(pos);
			mImageView.setImageRotateBitmapResetBase(
				new RotateBitmap(b, image.getDegreesRotated()), true);
						
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
						updateZoomButtonsEnabled();

						if(mPaneNum >= 0) 
							mImageView.showRect(mPanes.get(mPaneNum));
					}
				}

				public void detectingPanes(Rect area, RotateBitmap bitmap) {
					mBmp = bitmap;
					mDetectionArea = area;
					mGraphicInfoView.setVisibility(View.VISIBLE);
					mGraphicInfoView.invalidate();
				}	
						
				public void paneDetected(Rect pane, RotateBitmap bitmap) {
					mPanes.add(pane);
					mGraphicInfoView.invalidate();
				}

				public void panesDetectionComplete(Rect[] panes, RotateBitmap bitmap) {
					mPanes = Arrays.asList(panes);
					setPaneNum(0);
					mGraphicInfoView.setVisibility(View.GONE);
				}
			};

		mPaneNum = -1;
		
		// Could be null if we're stopping a slide show in the course of pausing
		if (mGetter != null) {
			mGetter.setPosition(pos, cb, mAllImages, mHandler);
		}
		if (showControls) showOnScreenControls();
		scheduleDismissOnScreenControls();
	}

	public int mPaneNum = -1;

	public void setPaneNum(int n) {
		if(n == mPaneNum)
			return;
		
		if(n >= mPanes.size())
			return;

		if(n < 0) {
			mImageView.showRect(new Rect(0, 0, mBmp.getWidth(), mBmp.getHeight()));
			mPaneNum = -1;
			return;
		}

		mImageView.showRect(mPanes.get(n));
		mPaneNum = n;
	}

	ImageView mGraphicInfoView;
	List<Rect> mPanes;
	Rect mDetectionArea;
	RotateBitmap mBmp;
	
	class DrawableImageInfo extends android.graphics.drawable.Drawable {
		/*
			int getEnergyHeight(int e) {
			return e * mBmp.getBitmap().getHeight() 
			/ RotateBitmap.MAX_ENERGY;
			}
		*/

		public void draw(Canvas c) {
			if(null == mBmp)
				return;
			/*
				Paint gp = new Paint();
				gp.setColor(0x77FF00FF);

				for(int i = 0; i < mBmp.mFirstHorizProj.length; i++) {
				RectF bar  = new RectF(i, 0, i + 1, 
				getEnergyHeight(mBmp.mFirstHorizProj[i])); 
				mImageView.getImageViewMatrix().mapRect(bar);
				c.drawRect(bar, gp);
				}
			*/

			/*
				Paint wp = new Paint();
				wp.setColor(0xFFFFFFFF);
				RectF thresh = new RectF(0, getEnergyHeight(mBmp.maxSepEnergy) - 1,
				mBmp.mFirstHorizProj.length, getEnergyHeight(mBmp.maxSepEnergy) + 1);
				mImageView.getImageViewMatrix().mapRect(thresh);
				c.drawRect(thresh, wp);
			*/
			
			Paint p = new Paint();
			p.setColor(0xE0FF0000);
			p.setStyle(Paint.Style.STROKE);
			p.setStrokeWidth(7);

			RectF displayedRect;
			for(Rect r: mPanes) {
				displayedRect = new RectF(r);
				mImageView.getImageViewMatrix().mapRect(displayedRect);
				c.drawRoundRect(displayedRect, 2, 2, p);
			}

			Paint detectionAreaPaint = new Paint();
			detectionAreaPaint.setColor(0xE000FFFF);
			detectionAreaPaint.setStyle(Paint.Style.STROKE);
			detectionAreaPaint.setStrokeWidth(7);

			Log.d(TAG, String.format("det: %s",  mDetectionArea.toString()));
			displayedRect = new RectF(mDetectionArea);
			mImageView.getImageViewMatrix().mapRect(displayedRect);
			Log.d(TAG, String.format("dis: %s",  displayedRect.toString()));
			c.drawRoundRect(displayedRect, 2, 2, detectionAreaPaint);
			
			Log.d(TAG, "draw");
		}

		public void setColorFilter(ColorFilter cf) {

		}

		public int getOpacity() {
			return PixelFormat.TRANSLUCENT;
		}

		public void setAlpha(int a) {
		}
	}

	

	@Override
    public void onCreate(Bundle instanceState) {
		super.onCreate(instanceState);

		Intent intent = getIntent();
		mFullScreenInNormalMode = intent.getBooleanExtra(
			MediaStore.EXTRA_FULL_SCREEN, true);
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

		mGraphicInfoView = (ImageView) findViewById(R.id.over);
		mGraphicInfoView.setImageDrawable(new DrawableImageInfo());

		makeGetter();

		mAnimationIndex = -1;

		mActionIconPanel = findViewById(R.id.action_icon_panel);

		mParam = getIntent().getParcelableExtra(KEY_IMAGE_LIST);

		if (instanceState != null) {
			mSavedUri = instanceState.getParcelable(STATE_URI);
			mShowControls = instanceState.getBoolean(STATE_SHOW_CONTROLS, true);
		} else {
			mSavedUri = getIntent().getData();
		}

		// We only show action icons for URIs that we know we can share and
		// delete. Although we get read permission (for the images) from
		// applications like MMS, we cannot pass the permission to other
		// activities due to the current framework design.
		/*if (!MenuHelper.isWhiteListUri(mSavedUri)) {
			mShowActionIcons = false;
			}*/

		if (mShowActionIcons) {
			View view = mActionIconPanel.findViewById(R.id.nextPane);
			view.setVisibility(View.VISIBLE);
			view.setOnClickListener(this);
		}

		if (mFullScreenInNormalMode) {
			getWindow().addFlags(
				WindowManager.LayoutParams.FLAG_FULLSCREEN);
		}
		if (mShowActionIcons) {
			mActionIconPanel.setVisibility(View.VISIBLE);
		}

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

	void setMode(int mode) {
		if (mMode == mode) {
			return;
		}
		View normalPanel = findViewById(R.id.abs);

		Window win = getWindow();
		mMode = mode;
		normalPanel.setVisibility(View.VISIBLE);

		win.clearFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
		win.addFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN);

		// mGetter null is a proxy for being paused
		if (mGetter != null) {
			setImage(mCurrentPosition, true);
		}
	}


	private void makeGetter() {
		mGetter = new ImageGetter(getContentResolver());
	}

	private IImageList buildImageListFromUri(Uri uri) {
		String sortOrder = mPrefs.getString(
			"pref_gallery_sort_key", "descending");
		int sort = sortOrder.equals("ascending")
			? ImageManager.SORT_ASCENDING
			: ImageManager.SORT_DESCENDING;
		return ImageManager.makeImageList(getContentResolver(), uri, sort);
	}

	private boolean init(Uri uri) {
		if (uri == null) return false;
		mAllImages = (mParam == null)
			? buildImageListFromUri(uri)
			: ImageManager.makeImageList(getContentResolver(), mParam);

		// DEBUG PRINT
		for(int i = mAllImages.getCount() - 1; i >= 0; i--) {
			Log.d(TAG, "mAllImages: " + mAllImages.getImageAt(i).fullSizeImageUri());
		}
				
		IImage image = mAllImages.getImageForUri(uri);
		if (image == null) return false;
		mCurrentPosition = mAllImages.getImageIndex(image);
		mLastSlideShowImage = mCurrentPosition;
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

	private void startShareMediaActivity(IImage image) {
		boolean isVideo = false;//image instanceof VideoObject;
		Intent intent = new Intent();
		intent.setAction(Intent.ACTION_SEND);
		intent.setType(image.getMimeType());
		intent.putExtra(Intent.EXTRA_STREAM, image.fullSizeImageUri());
		try {
			startActivity(Intent.createChooser(intent, getText(
						isVideo ? R.string.sendVideo : R.string.sendImage)));
		} catch (android.content.ActivityNotFoundException ex) {
			Toast.makeText(this, isVideo
				? R.string.no_way_to_share_image
				: R.string.no_way_to_share_video,
				Toast.LENGTH_SHORT).show();
		}
	}

	public void onClick(View v) {
		switch (v.getId()) {
			/*
				case R.id.discard:
				//MenuHelper.deletePhoto(this, mDeletePhotoRunnable);
				break;
				/*case R.id.share: {
				IImage image = mAllImages.getImageAt(mCurrentPosition);
				if (!MenuHelper.isWhiteListUri(image.fullSizeImageUri())) {
				return;
				}
				startShareMediaActivity(image);
				break;
				}
				case R.id.setas: {
				IImage image = mAllImages.getImageAt(mCurrentPosition);
				Intent intent = Util.createSetAsIntent(image);
				try {
				startActivity(Intent.createChooser(
				intent, getText(R.string.setImage)));
				} catch (android.content.ActivityNotFoundException ex) {
				Toast.makeText(this, R.string.no_way_to_share_video,
				Toast.LENGTH_SHORT).show();
				}
				break;
				}
			*/
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

	@Override
    protected void onActivityResult(int requestCode, int resultCode,
			Intent data) {
		/*switch (requestCode) {
			case MenuHelper.RESULT_COMMON_MENU_CROP:
			if (resultCode == RESULT_OK) {
			// The CropImage activity passes back the Uri of the
			// cropped image as the Action rather than the Data.
			mSavedUri = Uri.parse(data.getAction());

			// if onStart() runs before, then set the returned
			// image as currentImage.
			if (mAllImages != null) {
			IImage image = mAllImages.getImageForUri(mSavedUri);
			// image could be null if SD card is removed.
			if (image == null) {
			finish();
			} else {
			mCurrentPosition = mAllImages.getImageIndex(image);
			setImage(mCurrentPosition, false);
			}
			}
			}
			break;
			}*/
	}
}

class ImageViewTouch extends ImageViewTouchBase {
	private final ViewImage mViewImage;
	private boolean mEnableTrackballScroll;

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

		// Don't respond to arrow keys if trackball scrolling is not enabled
		if (!mEnableTrackballScroll) {
			if ((keyCode >= KeyEvent.KEYCODE_DPAD_UP)
				&& (keyCode <= KeyEvent.KEYCODE_DPAD_RIGHT)) {
				return super.onKeyDown(keyCode, event);
			}
		}

		int current = mViewImage.mCurrentPosition;

		int nextImagePos = -2; // default no next image
		try {
			switch (keyCode) {
			case KeyEvent.KEYCODE_DPAD_CENTER: {
				if (mViewImage.isPickIntent()) {
					IImage img = mViewImage.mAllImages
						.getImageAt(mViewImage.mCurrentPosition);
					mViewImage.setResult(ViewImage.RESULT_OK,
						new Intent().setData(img.fullSizeImageUri()));
					mViewImage.finish();
				}
				break;
			}
			case KeyEvent.KEYCODE_DPAD_LEFT: {
				if (getScale() <= 1F && event.getEventTime()
					>= mNextChangePositionTime) {
					nextImagePos = current - 1;
					mNextChangePositionTime = event.getEventTime() + 500;
				} else {
					panBy(PAN_RATE, 0);
					center(true, false);
				}
				return true;
			}
			case KeyEvent.KEYCODE_DPAD_RIGHT: {
				if (getScale() <= 1F && event.getEventTime()
					>= mNextChangePositionTime) {
					nextImagePos = current + 1;
					mNextChangePositionTime = event.getEventTime() + 500;
				} else {
					panBy(-PAN_RATE, 0);
					center(true, false);
				}
				return true;
			}
			case KeyEvent.KEYCODE_DPAD_UP: {
				panBy(0, PAN_RATE);
				center(false, true);
				return true;
			}
			case KeyEvent.KEYCODE_DPAD_DOWN: {
				panBy(0, -PAN_RATE);
				center(false, true);
				return true;
			}
			case KeyEvent.KEYCODE_DEL:
				//MenuHelper.deletePhoto(
				//        mViewImage, mViewImage.mDeletePhotoRunnable);
				break;
				
			case KeyEvent.KEYCODE_SEARCH:
				mViewImage.setPaneNum(mViewImage.mPaneNum + 1);
				break;

			case KeyEvent.KEYCODE_BACK:
				mViewImage.setPaneNum(mViewImage.mPaneNum - 1);
				break;
			}
		} finally {
			if (nextImagePos >= 0
				&& nextImagePos < mViewImage.mAllImages.getCount()) {
				synchronized (mViewImage) {
					mViewImage.setMode(ViewImage.MODE_NORMAL);
					mViewImage.setImage(nextImagePos, true);
				}
			} else if (nextImagePos != -2) {
				center(true, true);
			}
		}

		Log.d("Pwer", String.format("e: %d", keyCode));
		return super.onKeyDown(keyCode, event);
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
