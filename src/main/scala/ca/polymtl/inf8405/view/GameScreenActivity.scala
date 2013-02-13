package ca.polymtl.inf8405.view

import android.app.Activity
import android.os.Bundle
import android.view.Menu

class GameScreenActivity extends Activity with TypedActivity
{
  override def onCreateOptionsMenu( menu: Menu ) =
  {
    getMenuInflater.inflate( R.menu.gamescreen, menu )
    true
  }

  protected override def onCreate( savedInstanceState: Bundle )
  {
    super.onCreate( savedInstanceState )
    val size = getIntent.getIntExtra(SelectSizeActivity.EXTRA_MESSAGE, 0)
    setContentView( new DrawView( this, size ) )
  }

}