package vg.panes;

import android.app.Activity;
import android.os.Bundle;
import android.content.Intent;
import android.provider.MediaStore;
import android.util.Log;
import android.net.Uri;
import android.database.Cursor;

public class MainScreen extends Activity
{
	private static final String TAG = "panes";
	private static final int PICK_REQUEST = 1;
	
	/** Called when the activity is first created. */
	@Override
    public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.main);

		/*
		Cursor c = managedQuery(Uri.parse("content://com.google.settings/partner"), 
			null, null, null, null);
		Log.d(TAG, String.format("%d records", c.getCount()));

		while(c.moveToNext()) {
			for(int i = 0; i < c.getColumnCount(); i++) {
				Log.d(TAG, String.format("%s: %s", c.getColumnName(i), c.getString(i)));
			}
		}
		*/
		
		Intent what = new Intent(Intent.ACTION_PICK, 
			MediaStore.Images.Media.INTERNAL_CONTENT_URI);
		startActivityForResult(what, PICK_REQUEST);
	}

	public void onActivityResult(int requestCode, int resultCode, Intent data) {
		switch(requestCode) {
		case PICK_REQUEST:
			if(null == data) {
				// other picking methods go here
				return;
			}
			String path = data.getData().toString();
			Log.d(TAG, path);
			Intent viewIntent = new Intent(this, ViewImage.class);
			viewIntent.setData(data.getData());
			startActivity(viewIntent);
		}
	}
}
