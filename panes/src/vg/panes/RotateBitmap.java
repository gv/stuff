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

import android.graphics.Bitmap;
import android.graphics.Matrix;
import android.graphics.Rect;
import java.util.ArrayList;
import android.util.Log;

public class RotateBitmap {
    public static final String TAG = "RotateBitmap";
    private Bitmap mBitmap;
    private int mRotation;

    public RotateBitmap(Bitmap bitmap) {
        mBitmap = bitmap;
        mRotation = 0;
    }

    public RotateBitmap(Bitmap bitmap, int rotation) {
        mBitmap = bitmap;
        mRotation = rotation % 360;
    }

    public void setRotation(int rotation) {
        mRotation = rotation;
    }

    public int getRotation() {
        return mRotation;
    }

    public Bitmap getBitmap() {
        return mBitmap;
    }

    public void setBitmap(Bitmap bitmap) {
        mBitmap = bitmap;
    }

    public Matrix getRotateMatrix() {
        // By default this is an identity matrix.
        Matrix matrix = new Matrix();
        if (mRotation != 0) {
            // We want to do the rotation at origin, but since the bounding
            // rectangle will be changed after rotation, so the delta values
            // are based on old & new width/height respectively.
            int cx = mBitmap.getWidth() / 2;
            int cy = mBitmap.getHeight() / 2;
            matrix.preTranslate(-cx, -cy);
            matrix.postRotate(mRotation);
            matrix.postTranslate(getWidth() / 2, getHeight() / 2);
        }
        return matrix;
    }

    public boolean isOrientationChanged() {
        return (mRotation / 90) % 2 != 0;
    }

    public int getHeight() {
        if (isOrientationChanged()) {
            return mBitmap.getWidth();
        } else {
            return mBitmap.getHeight();
        }
    }

    public int getWidth() {
        if (isOrientationChanged()) {
            return mBitmap.getHeight();
        } else {
            return mBitmap.getWidth();
        }
    }

    public void recycle() {
        if (mBitmap != null) {
            mBitmap.recycle();
            mBitmap = null;
        }
    }


	
	public ArrayList<Rect> mPanes;
	public int[] mFirstHorizProj;
	public int maxSepEnergy;

	public void findPanes() {
		Bitmap b = mBitmap;
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
		findPanes(energy, w, new Rect(0, 0, w, h), true);
		
		for(Rect r: mPanes) {
			Log.d(TAG, String.format("pane: %d, %d, %d, %d", 
					r.left, r.top, r.right, r.bottom));
		}
	}
	
	public static final int MAX_ENERGY = 6*255;
	final int MIN_SEP_DIM = 3;
	final int MIN_PANE_DIM = 20;

	private void findPanes(int[] energy, int w, Rect r, boolean vertical) {
		Log.d(TAG, String.format("searching: %d, %d, %d, %d", 
				r.left, r.top, r.right, r.bottom));

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
		
		int sepRight = 0,
			paneRight = projection.length - 1;

		for(int i = projection.length - 1; i >= 0; i--) {
			int e = projection[i], len = 0; 

			if(i == 0)
				e = maxSepEnergy;

			while(e <= maxSepEnergy && (len < MIN_SEP_DIM || i > 0)) {
				len++, i--;
				if(i >= 0)
					e = projection[i];
			}

			if(len >= MIN_SEP_DIM) {
				// A valid separator
				Log.d(TAG, String.format("Separator found: %d, %d", i, len));
				if(paneRight - i - len >= MIN_PANE_DIM) {
					// A big enough pane
					if(vertical) 
						findPanes(energy, w, new Rect(
								r.left + i + len,
								r.top, 
								r.left + paneRight,
								r.bottom), false);
					else
						findPanes(energy, w, new Rect(
								r.left,
								r.top + sepRight,
								r.right, 
								r.top + paneRight), true);
				}
				paneRight = i;
			}
		}

	}
}

