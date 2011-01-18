package vg.panes;

import android.app.Activity;
import android.os.Bundle;
import android.content.Intent;
import android.provider.MediaStore;
import android.util.Log;

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

		Intent what = new Intent(Intent.ACTION_PICK, 
			MediaStore.Images.Media.INTERNAL_CONTENT_URI);
		startActivityForResult(what, PICK_REQUEST);
	}

	public void onActivityResult(int requestCode, int resultCode, Intent data) {
		switch(requestCode) {
		case PICK_REQUEST:
			String path = data.getData().toString();
			Log.d(TAG, path);
			Intent viewIntent = new Intent(this, ViewImage.class);
			viewIntent.setData(data.getData());
			startActivity(viewIntent);
		}
	}
}
