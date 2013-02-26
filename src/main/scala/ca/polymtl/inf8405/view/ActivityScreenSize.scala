package ca.polymtl.inf8405.view

import android.content.Context
import android.view.WindowManager
import android.graphics.Point
import android.app.Activity
import android.widget.RelativeLayout
import android.view.ViewGroup.LayoutParams

trait ActivityScreenSize extends Activity
{
  def screenSize =
  {
    val wm = getSystemService( Context.WINDOW_SERVICE ).asInstanceOf[WindowManager]
    val display = wm.getDefaultDisplay
    var screenSize = new Point()
    display.getSize(screenSize)

    Dimension( screenSize.x, screenSize.y )
  }
}

trait ButtonLayout extends ActivityScreenSize
{
  lazy val buttonLayout = new RelativeLayout.LayoutParams(
    screenSize.width * 2 / 3,
    LayoutParams.WRAP_CONTENT
  )
}