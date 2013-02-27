package ca.polymtl.inf8405
package view

import android.app.Activity
import android.os.Bundle
import android.view.{View}
import android.content.Intent

class MainActivity extends Activity with TypedActivity
{
  val EXTRA_MESSAGE = "com.example.myfirstapp.MESSAGE"

  protected override def onCreate( savedInstanceState: Bundle )
  {
    super.onCreate( savedInstanceState )
    setContentView( R.layout.activity_main )
  }

  def playGame( view: View )
  {
    val intent = new Intent(this, classOf[SelectSizeActivity] )
    startActivity(intent)
  }
}