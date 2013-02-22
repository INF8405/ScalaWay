package ca.polymtl.inf8405.view

import android.app.Activity
import android.content.Context
import android.graphics.Point
import android.os.Bundle
import android.view.{WindowManager, Window}
import android.widget.TextView
import android.widget.ImageView

class CustomWindowTitle extends Activity with TypedActivity
{
  protected var title: Option[TextView] = None
  protected var icon: Option[ImageView] = None

  override protected def onCreate( savedInstanceState: Bundle )
  {
    super.onCreate(savedInstanceState)
    requestWindowFeature(Window.FEATURE_CUSTOM_TITLE)

    setContentView(R.layout.select_size)

    getWindow.setFeatureInt(Window.FEATURE_CUSTOM_TITLE, R.layout.window_title)

    val title_ = findView( TR.title_custom_window )
    title = Some( title_ )
    title_.setText( "Custom title" )

    //icon = Some( findView( TR.ic_launcher ) )
  }

  def getDimention =
  {
    val wm = getSystemService( Context.WINDOW_SERVICE ).asInstanceOf[WindowManager]
    val display = wm.getDefaultDisplay
    var screenSize = new Point()
    display.getSize(screenSize)

    ( screenSize.x, screenSize.y )
  }
}
