package gv.yy;

import android.app.Activity;
import android.os.Bundle;

import android.os.*;
import android.graphics.*;
import android.view.*;
import android.widget.*;

import java.io.*;

public class Moments extends Activity
{
	static final String inpPrefix = "/flash/y/";
	static final String outpPrefix = "/flash/o";

	String[] mNames;
	int mStartPos, mEndPos;
	ListView mStartSelector, mEndSelector;
	Handler mHandler;

	void setStatus(String what) {
		TextView status = (TextView)findViewById(R.id.status);
		status.setText(what);
	}

	@Override public void onCreate(Bundle savedInstanceState)	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.main);

		mStartSelector = (ListView)findViewById(R.id.startSelector);
		mEndSelector = (ListView)findViewById(R.id.endSelector);
	}

	@Override	public void onStart() {
		super.onStart();
		File dir = new File(inpPrefix);
		String names[] = dir.list();
		
		if(null == names) {
			setStatus("no input");
			return;
		}

		if(names.length == 0) {
			setStatus("no files");
			return;
		}

		names.sort();
		mNames = names;

		ListAdapter ad = new ArrayAdapter<String>(this, R.layout.entry,  mNames);
		mStartSelector.setAdapter(ad);
		mEndSelector.setAdapter(ad);

		AdapterView.OnItemClickListener listener = new AdapterView.OnItemClickListener() {
				public void onItemClick(AdapterView<?> adapterView, View view, int i, long l) {

				}
			};
		mStartSelector.setOnItemClickListener(listener);
		mEndSelector.setOnItemClickListener(listener);

		mStartPos = 0;
		mEndPos = 0;
		displayPos(mStartPos);
	}
	
	void displayPos(int pos) {
		String path = inpPrefix + mNames[pos];
		setStatus("Loading " + path);
				
		Bitmap b = BitmapFactory.decodeFile(path);
		ImageView image = (ImageView)findViewById(R.id.image);
		image.setImageBitmap(b);
		setStatus("Displaying " + path);
	}
}
