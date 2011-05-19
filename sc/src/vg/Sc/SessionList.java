package vg.Sc;

import android.app.Activity;
import android.os.Bundle;
import android.widget.Button;
import android.view.View;

public class SessionList extends Activity
{
    /** Called when the activity is first created. */
	@Override
    public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);
		setContentView(R.layout.main);

		final Button refreshBtn = (Button) findViewById(R.id.refresh);
		refreshBtn.setOnClickListener(new View.OnClickListener() {
				public void onClick(View v) {
					// Perform action on click
				}
			});
	}
}
