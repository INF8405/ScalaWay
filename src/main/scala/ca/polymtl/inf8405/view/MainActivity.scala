package ca.polymtl.inf8405
package view

import android.os.Bundle
import android.view.{View}
import android.content.Intent

/**
 * Main window
 */
class MainActivity extends CustomWindowTitle with TypedActivity
{

  protected override def onCreate( savedInstanceState: Bundle )
  {
    super.onCreate( savedInstanceState )
    title.foreach( _.setText("Flow Game" ) )

    setContentView( R.layout.activity_main )
  }

  // Open a new windows which allow to choose the size map
  def playGame( view: View )
  {
    val intent = new Intent(this, classOf[SelectSizeActivity] )
    startActivity(intent)
  }

}