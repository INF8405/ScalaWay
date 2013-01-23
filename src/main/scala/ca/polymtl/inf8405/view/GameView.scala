package ca.polymtl.inf8405.view

import android.view.View
import android.graphics.{Paint, Canvas}
import android.content.Context
import android.util.AttributeSet

class GameView( context: Context, attributes: AttributeSet ) extends View( context, attributes )
{
  val blue = new Paint()
  blue.setColor( 0x0000FF )


  override def onDraw( canvas: Canvas )
  {
    super.onDraw( canvas )



    canvas.drawCircle( 1,1,1, blue )
    //canvas.drawCircle( )
  }
}
