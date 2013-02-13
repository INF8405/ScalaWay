package ca.polymtl.inf8405.view

import android.app.Activity
import android.os.Bundle
import android.view.{View, Menu}
import android.content.Intent

class Maps7x7Activity extends Activity with TypedActivity
{
  override def onCreateOptionsMenu( menu: Menu ) =
  {
    getMenuInflater.inflate( R.menu.activity_maps7x7, menu )
    true
  }

  protected override def onCreate( savedInstanceState: Bundle )
  {
    super.onCreate( savedInstanceState )
    setContentView( R.layout.activity_maps7x7 )
    getActionBar.setDisplayHomeAsUpEnabled( true )
  }

  def selectSize7Lv1( view: View )
  {
    val intent = new Intent( this, classOf[GameScreenActivity] )
    intent.putExtra(SelectSizeActivity.EXTRA_MESSAGE, 7)
    startActivity(intent)
  }
}