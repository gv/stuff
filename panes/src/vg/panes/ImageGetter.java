/*
 * Copyright (C) 2009 The Android Open Source Project
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

import android.content.ContentResolver;
import android.graphics.Bitmap;
import android.os.Handler;
import android.os.Message;
import android.os.Process;
import android.provider.MediaStore;

import android.graphics.Rect;
import android.util.Log;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

/*
 * Here's the loading strategy.  For any given image, load the thumbnail
 * into memory and post a callback to display the resulting bitmap.
 *
 * Then proceed to load the full image bitmap.   Three things can
 * happen at this point:
 *
 * 1.  the image fails to load because the UI thread decided
 * to move on to a different image.  This "cancellation" happens
 * by virtue of the UI thread closing the stream containing the
 * image being decoded.  BitmapFactory.decodeStream returns null
 * in this case.
 *
 * 2.  the image loaded successfully.  At that point we post
 * a callback to the UI thread to actually show the bitmap.
 *
 * 3.  when the post runs it checks to see if the image that was
 * loaded is still the one we want.  The UI may have moved on
 * to some other image and if so we just drop the newly loaded
 * bitmap on the floor.
 */

interface ImageGetterCallback {
	public void imageLoaded(int pos, int offset, RotateBitmap bitmap,
		boolean isThumb);
	public boolean wantsThumbnail(int pos, int offset);
	public boolean wantsFullImage(int pos, int offset);
	public int fullImageSizeToUse(int pos, int offset);
	public void completed();
	public int [] loadOrder();
	public void panesDetectionComplete(Rect[] panes, RotateBitmap bitmap);
	public void paneDetected(Rect pane, RotateBitmap bitmap);
	public void detectingPanes(Rect area, RotateBitmap bitmap);
}

class ImageGetter {

	@SuppressWarnings("unused")
    private static final String TAG = "ImageGetter";

	// The thread which does the work.
	private Thread mGetterThread;

	// The current request serial number.
	// This is increased by one each time a new job is assigned.
	// It is only written in the main thread.
	private int mCurrentSerial;

	// The base position that's being retrieved.  The actual images retrieved
	// are this base plus each of the offets. -1 means there is no current
	// request needs to be finished.
	private int mCurrentPosition = -1;

	// The callback to invoke for each image.
	private ImageGetterCallback mCB;

	// The image list for the images.
	private IImageList mImageList;

	// The handler to do callback.
	private GetterHandler mHandler;

	// True if we want to cancel the current loading.
	private volatile boolean mCancel = true;

	// True if the getter thread is idle waiting.
	private boolean mIdle = false;

	// True when the getter thread should exit.
	private boolean mDone = false;

	private ContentResolver mCr;

	private class ImageGetterThread implements Runnable {

		private Runnable callback(final int position, final int offset,
			final boolean isThumb,
			final RotateBitmap bitmap,
			final int requestSerial) {
			return new Runnable() {
				public void run() {
					// check for inflight callbacks that aren't applicable
					// any longer before delivering them
					if (requestSerial == mCurrentSerial) {
						mCB.imageLoaded(position, offset, bitmap, isThumb);
					} else if (bitmap != null) {
						bitmap.recycle();
					}
				}
			};
		}

		private Runnable completedCallback(final int requestSerial) {
			return new Runnable() {
				public void run() {
					if (requestSerial == mCurrentSerial) {
						mCB.completed();
					}
				}
			};
		}

		public void run() {
			// Lower the priority of this thread to avoid competing with
			// the UI thread.
			Process.setThreadPriority(Process.THREAD_PRIORITY_BACKGROUND);

			while (true) {
				synchronized (ImageGetter.this) {
					while (mCancel || mDone || mCurrentPosition == -1) {
						if (mDone) return;
						mIdle = true;
						ImageGetter.this.notify();
						try {
							ImageGetter.this.wait();
						} catch (InterruptedException ex) {
							// ignore
						}
						mIdle = false;
					}
				}

				executeRequest();

				synchronized (ImageGetter.this) {
					mCurrentPosition = -1;
				}
			}
		}
		private void executeRequest() {
			int imageCount = mImageList.getCount();

			int [] order = mCB.loadOrder();
			for (int i = 0; i < order.length; i++) {
				if (mCancel) return;
				int offset = order[i];
				int imageNumber = mCurrentPosition + offset;
				if (imageNumber >= 0 && imageNumber < imageCount) {
					if (!mCB.wantsThumbnail(mCurrentPosition, offset)) {
						continue;
					}

					IImage image = mImageList.getImageAt(imageNumber);
					if (image == null) continue;
					if (mCancel) return;

					Bitmap b = image.thumbBitmap(IImage.NO_ROTATE);
					if (b == null) continue;
					if (mCancel) {
						b.recycle();
						return;
					}

					Runnable cb = callback(mCurrentPosition, offset,
						true,
						new RotateBitmap(b, image.getDegreesRotated()),
						mCurrentSerial);
					mHandler.postGetterCallback(cb);
				}
			}

			for (int i = 0; i < order.length; i++) {
				if (mCancel) return;
				int offset = order[i];
				int imageNumber = mCurrentPosition + offset;
				if (imageNumber >= 0 && imageNumber < imageCount) {
					if (!mCB.wantsFullImage(mCurrentPosition, offset)) {
						continue;
					}

					IImage image = mImageList.getImageAt(imageNumber);
					if (image == null) continue;
					//if (image instanceof VideoObject) continue;
					if (mCancel) return;

					int sizeToUse = mCB.fullImageSizeToUse(
						mCurrentPosition, offset);
					Bitmap b = image.fullSizeBitmap(sizeToUse, 3 * 1024 * 1024,
						IImage.NO_ROTATE, IImage.USE_NATIVE);

					if (b == null) continue;
					if (mCancel) {
						b.recycle();
						return;
					}

					RotateBitmap rb = new RotateBitmap(b,
						image.getDegreesRotated());

					Runnable cb = callback(mCurrentPosition, offset,
						false, rb, mCurrentSerial);
					mHandler.postGetterCallback(cb);

					findPanes(rb);
				}

			}

			mHandler.postGetterCallback(completedCallback(mCurrentSerial));
		}

		public ArrayList<Rect> mPanes;
		public int[] mFirstHorizProj;
		public int maxSepEnergy;

		public void findPanes(RotateBitmap rb) {
			Bitmap b = rb.getBitmap();
			mPanes  = new ArrayList<Rect>();
			int w = b.getWidth();
			int h = b.getHeight(), i;
		
			int[] energy = new int[w*h];
			b.getPixels(energy, 0, b.getWidth(), 0, 0, w, h);
			for(i = energy.length - 1; i >= w; i--) {
				int e = energy[i], u = energy[i-w], l = energy[i-1];
				energy[i] = 
					Math.abs(e & 0xFF - u & 0xFF) + 
					Math.abs((e >> 8) & 0xFF - (u >> 8) & 0xFF) + 
					Math.abs((e >> 16) & 0xFF - (u >> 16) & 0xFF) + 
					Math.abs(e & 0xFF - l & 0xFF) + 
					Math.abs((e >> 8) & 0xFF - (l >> 8) & 0xFF) + 
					Math.abs((e >> 16) & 0xFF - (l >> 16) & 0xFF);
			}
		
			for(; i > 0; i--) {
				int e = energy[i], l = energy[i-1];
				energy[i] = 
					Math.abs(e & 0xFF - l & 0xFF) + 
					Math.abs((e >> 8) & 0xFF - (l >> 8) & 0xFF) + 
					Math.abs((e >> 16) & 0xFF - (l >> 16) & 0xFF);
			}

			energy[0] = 0;
			
			mFirstHorizProj = null;
			findPanes(rb, energy, w, new Rect(0, 0, w, h), true, 0);
			
			Collections.sort(mPanes, new Comparator<Rect>() {
					public int compare(Rect l, Rect r) {
						if(l.bottom < r.top)
							return -1;

						if(r.bottom < l.top)
							return 1;
						
						if(l.left < r.left)
							return -1;

						if(r.left > r.left)
							return 1;
						
						return 0;
					}
				});

			mHandler.postGetterCallback(panesDetectionCompleteCallback(
					mPanes.toArray(new Rect[1]), rb));

			for(Rect r: mPanes) {
				Log.d(TAG, String.format("pane: %d, %d, %d, %d", 
						r.left, r.top, r.right, r.bottom));
			}
		}
	
		public static final int MAX_ENERGY = 6*255;
		final int MIN_SEP_DIM = 3;
		final int MIN_PANE_DIM = 20;

		private void findPanes(RotateBitmap rb, int[] energy, int w, 
			Rect r, boolean vertical, int count) {
			mHandler.postGetterCallback(detectingPanesCallback(r, rb));

			Log.d(TAG, String.format("searching: %d, %d, %d, %d, %s", 
					r.left, r.top, r.right, r.bottom, 
					vertical ? "separating vertically" : "separating horizontally"));

			int projection[] = new int[vertical ? (r.right - r.left) : (r.bottom - r.top)];

			for(int x = r.left; x < r.right; x++) {
				for(int y = r.top; y < r.bottom; y++) {
					int e = energy[y*w + x];
					if(vertical && e > projection[x - r.left])
						projection[x - r.left] = e;
					else if(!vertical && e > projection[y - r.top])
						projection[y - r.top] = e;
				}
			}

			// DEBUG
			if(mFirstHorizProj == null) 
				mFirstHorizProj = projection;

			double sum = 0;
			for(int i = projection.length - 1; i >= 0; i--) {
				sum += projection[i];
			}
		
			maxSepEnergy = (int)(sum / projection.length / 4);
			Log.d(TAG, String.format("Max separator energy: %d", maxSepEnergy));
		
			int paneRight = projection.length, len = MIN_SEP_DIM;
			ArrayList<Rect> found = new ArrayList<Rect> ();

			for(int i = projection.length - 1; i >= -1; i--) {
				int e;
				if(i == -1)
					e = maxSepEnergy;
				else 
					e = projection[i]; 

				while(e <= maxSepEnergy && (len < MIN_SEP_DIM || i > 0)) {
					len++; 
					i--;
					if(i >= 0)
						e = projection[i];
				}

				if(len >= MIN_SEP_DIM) {
					// i is at rightmost "picture"
					// i + len is at rightmost "no picture"
					// paneRight is at rightmost "picture we just found"
					Log.d(TAG, String.format("Separator found: %d, %d", i, len));
					if(paneRight - i - len >= MIN_PANE_DIM) {
						if(vertical) {
							found.add(new Rect(
									r.left + i + len + 2,
									r.top, 
									r.left + paneRight,
									r.bottom));
						} else {
							found.add(new Rect(
									r.left,
									r.top + i + len + 2,
									r.right, 
									r.top + paneRight));
						}
					}
					paneRight = i;
				}
				len = 0;
			}
			
			if(found.size() == 1) {
				if(1 == count++) {
					mPanes.add(found.get(0));
					mHandler.postGetterCallback(paneDetectedCallback(found.get(0), rb));
					return;
				}
			}

			for(Rect foundRect: found) {
				findPanes(rb, energy, w, foundRect, !vertical, count);
			}
		}

		private Runnable paneDetectedCallback(final Rect p, final RotateBitmap b) {
			return new Runnable() {
				public void run() {
					mCB.paneDetected(p, b);
				}
			};
		}

		private Runnable panesDetectionCompleteCallback(final Rect[] pp, 
			final RotateBitmap b) {
			return new Runnable() {
				public void run() {
					mCB.panesDetectionComplete(pp, b);
				}
			};
		}

		private Runnable detectingPanesCallback(final Rect area, final RotateBitmap b) {
			return new Runnable() {
				public void run() {
					mCB.detectingPanes(area, b);
				}
			};
		}
	}

	public ImageGetter(ContentResolver cr) {
		mCr = cr;
		mGetterThread = new Thread(new ImageGetterThread());
		mGetterThread.setName("ImageGettter");
		mGetterThread.start();
	}

	// Cancels current loading (without waiting).
	public synchronized void cancelCurrent() {
		Util.Assert(mGetterThread != null);
		mCancel = true;
		BitmapManager.instance().cancelThreadDecoding(mGetterThread, mCr);
	}

	// Cancels current loading (with waiting).
	private synchronized void cancelCurrentAndWait() {
		cancelCurrent();
		while (mIdle != true) {
			try {
				wait();
			} catch (InterruptedException ex) {
				// ignore.
			}
		}
	}

	// Stops this image getter.
	public void stop() {
		synchronized (this) {
			cancelCurrentAndWait();
			mDone = true;
			notify();
		}
		try {
			mGetterThread.join();
		} catch (InterruptedException ex) {
			// Ignore the exception
		}
		mGetterThread = null;
	}

	public synchronized void setPosition(int position, ImageGetterCallback cb,
		IImageList imageList, GetterHandler handler) {
		// Cancel the previous request.
		cancelCurrentAndWait();

		// Set new data.
		mCurrentPosition = position;
		mCB = cb;
		mImageList = imageList;
		mHandler = handler;
		mCurrentSerial += 1;

		// Kick-start the current request.
		mCancel = false;
		BitmapManager.instance().allowThreadDecoding(mGetterThread);
		notify();
	}
}

class GetterHandler extends Handler {
	private static final int IMAGE_GETTER_CALLBACK = 1;

	@Override
    public void handleMessage(Message message) {
		switch(message.what) {
		case IMAGE_GETTER_CALLBACK:
			((Runnable) message.obj).run();
			break;
		}
	}

	public void postGetterCallback(Runnable callback) {
		postDelayedGetterCallback(callback, 0);
	}

	public void postDelayedGetterCallback(Runnable callback, long delay) {
		if (callback == null) {
			throw new NullPointerException();
		}
		Message message = Message.obtain();
		message.what = IMAGE_GETTER_CALLBACK;
		message.obj = callback;
		sendMessageDelayed(message, delay);
	}

	public void removeAllGetterCallbacks() {
		removeMessages(IMAGE_GETTER_CALLBACK);
	}
}
