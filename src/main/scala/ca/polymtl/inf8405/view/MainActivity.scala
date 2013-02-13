package ca.polymtl.inf8405
package view

import android.app.Activity
import android.os.Bundle
import android.view.{View, Menu}
import android.content.Intent

class MainActivity extends Activity with TypedActivity
{
  val EXTRA_MESSAGE = "com.example.myfirstapp.MESSAGE"

  protected override def onCreate( savedInstanceState: Bundle )
  {
    super.onCreate( savedInstanceState )
    setContentView( R.layout.activity_main )
  }

  override def onCreateOptionsMenu( menu: Menu ) =
  {
    getMenuInflater.inflate(R.menu.activity_main, menu)
    true
  }

  def playGame( view: View )
  {
    val intent = new Intent(this, classOf[SelectSizeActivity] )
    startActivity(intent)
  }

//  def test(View view)
//  {
//    Intent intent = new Intent(this, TestActivity.class);
//    startActivity(intent);
//  }
}