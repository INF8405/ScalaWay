package ca.polymtl.inf8405
package view

import android.app.Activity
import android.os.Bundle
import android.view.{View, Menu}
import android.content.Intent


object SelectSizeActivity
{
  val EXTRA_MESSAGE = "SelectedSize"
}

class SelectSizeActivity extends Activity with TypedActivity
{
  override def onCreateOptionsMenu( menu: Menu ) =
  {
    getMenuInflater.inflate( R.menu.select_size, menu )
    true
  }

  protected override def onCreate( savedInstanceState: Bundle )
  {
    super.onCreate( savedInstanceState )
    setContentView( R.layout.select_size )
    getActionBar.setDisplayHomeAsUpEnabled( true )
  }

  def selectSize7( view: View )
  {
    val intent = new Intent( this, classOf[Maps7x7Activity] )
    intent.putExtra( SelectSizeActivity.EXTRA_MESSAGE, 7 )
    startActivity( intent )
  }

  def selectSize8( view: View )
  {
    val intent = new Intent( this, classOf[GameScreenActivity] )
    intent.putExtra( SelectSizeActivity.EXTRA_MESSAGE, 8 )
    startActivity( intent )
  }
}