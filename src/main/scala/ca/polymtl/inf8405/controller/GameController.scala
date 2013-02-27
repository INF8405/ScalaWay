package ca.polymtl.inf8405
package controller

import android.view.View.OnTouchListener
import android.view.{MotionEvent, View}
import model.Coordinate
import view.DrawView

class GameController( drawView: DrawView, observer: GridObserver ) extends OnTouchListener
{
  var lastCoord: Option[Coordinate] = None

  override def onTouch( view: View, event: MotionEvent ): Boolean =
  {
    view.invalidate()

    event.getAction match
    {
      case MotionEvent.ACTION_DOWN =>
      {
        lastCoord = Some( fromEvent( event ) )
        true
      }
      case MotionEvent.ACTION_MOVE =>
      {
        if ( lastCoord.isEmpty )
        {
          lastCoord = Some( fromEvent( event ) )
        }
        else
        {
          val currentCoordinate = fromEvent( event )
          for { last <- lastCoord if last != currentCoordinate }
          {
            observer.link( last, currentCoordinate )
            lastCoord = Some( currentCoordinate )
          }
        }

        true
      }
      case MotionEvent.ACTION_UP =>
      {
        lastCoord = None

        true
      }
      case _ => false
    }
  }



  def fromEvent( event: MotionEvent ) = pixelToModel( event.getX, event.getY )

  def pixelToModel( x: Float, y: Float) =
  {
    val modelSize: Float = observer.grid.size
    val screenSize: Float = drawView.GRID_SIZE

    val cx = Math.floor( modelSize * x / screenSize ).asInstanceOf[Int]
    val cy = Math.floor( modelSize * y / screenSize ).asInstanceOf[Int]

    Coordinate( cx, cy )
  }
}
