package ca.polymtl.inf8405
package view

import model.{GridFactory, Grid}

import android.view.View
import android.content.Context
import android.graphics.{Paint, Canvas, Color}

class DrawView( context: Context, size: Int ) extends View( context )
{
  val model = GridFactory.SevenBySeven.level1

  val paint = new Paint
  paint.setColor( Color.RED )
  paint.setStrokeWidth( 3 )

  setBackgroundColor( Color.BLUE )

  override def onDraw( canvas: Canvas )
  {

  }
}
