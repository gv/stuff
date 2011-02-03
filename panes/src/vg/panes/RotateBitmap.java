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
			

		//b.setPixels(energy, 0, b.getWidth(), 0, 0, w, h);
		mFirstHorizProj = null;
		findPanes(energy, w, new Rect(0, 0, w, h));
		
		for(Rect r: mPanes) {
			Log.d(TAG, String.format("pane: %d, %d, %d, %d", 
					r.left, r.top, r.right, r.bottom));
		}
	}
	
	public static final int MAX_ENERGY = 6*255;
	final int MIN_SEP_DIM = 3;
	final int MIN_PANE_DIM = 20;

	
	private void findPanes(int[] energy, int w, Rect r) {
		Log.d(TAG, String.format("searching: %d, %d, %d, %d", 
				r.left, r.top, r.right, r.bottom));

		int maxSepEnergy = MAX_ENERGY / 6;

		int horizProj[] = new int[r.right - r.left];

		int sepRight = 0, sepLeft = 0;
		int probableSepRight = 0;

		for(int x = horizProj.length - 1; x >= 0; x--) {
			for(int y = r.top; y < r.bottom; y++) {
				horizProj[x] = Math.max(horizProj[x], energy[y*w+x+r.left]);
			}
		
			if(horizProj[x] < maxSepEnergy) {
				if(probableSepRight == 0) {
					probableSepRight = x;
				}
			} else {
				if(probableSepRight > 0) {
					if(probableSepRight - x > sepRight - sepLeft) {
						sepLeft = x;
						sepRight = probableSepRight;
						Log.d(TAG, String.format("ps: %d, %d, %d, %d", 
								probableSepRight, x, sepRight, sepLeft));
					}
					probableSepRight = 0;
				}
			}
		}

		if(probableSepRight > sepRight - sepLeft) {
			sepRight = probableSepRight;
			sepLeft = 0;
		}
				
		if(mFirstHorizProj == null) 
			mFirstHorizProj = horizProj;

		Log.d(TAG, String.format("Horizontal separator found: %d, %d", 
				sepLeft + r.left, sepRight + r.left));

		if(sepRight - sepLeft < MIN_SEP_DIM) {
			// whole
			mPanes.add(r);
			return;
		}

		if(sepLeft >= MIN_PANE_DIM) {
			Rect leftPartLoc = new Rect(r.left, r.top, 
				r.left + sepLeft, r.bottom);
			Log.d(TAG, "going left");
			findPanes(energy, w, leftPartLoc);
		}
		
		if(horizProj.length - 1 - sepRight >= MIN_PANE_DIM) {
			Rect rightPartLoc = new Rect(r.left + sepRight, r.top, 
				r.right, r.bottom);
			Log.d(TAG, "going right");
			findPanes(energy, w, rightPartLoc);
		}
	}

}

