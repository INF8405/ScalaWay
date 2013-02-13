package ca.polymtl.inf8405
package view

import model.{GridFactory, Grid}

import android.view.{WindowManager, View}
import android.content.Context
import android.graphics.{Point, Paint, Canvas, Color}

class DrawView( context: Context, size: Int ) extends View( context )
{
  val ( width, height ) = screenSize
  val GRID_SIZE = Math.min( width, height )
  val CELL_SIZE = GRID_SIZE / size
  val TOKEN_RADIUS = 3 * CELL_SIZE / 8

  val model = GridFactory.SevenBySeven.level1

  val gridPaint = new Paint
  gridPaint.setColor( Color.BLACK )
  gridPaint.setStrokeWidth( 5 )

  val tokenPaint = new Paint
  tokenPaint.setStrokeWidth( 1 )

  val colorMap = Map(
    GridFactory.red -> Color.RED,
    GridFactory.blue -> Color.BLUE,
    GridFactory.orange -> Color.rgb( 255, 204, 0 ),
    GridFactory.green -> Color.GREEN,
    GridFactory.yellow -> Color.YELLOW
  )

  setBackgroundColor( Color.WHITE )

  override def onDraw( canvas: Canvas )
  {
    def drawEmptyGrid()
    {
      for {
        x <- 1 to model.size
        y <- 1 to model.size }
      {
        canvas.drawLine( CELL_SIZE * x, 0, CELL_SIZE * x, GRID_SIZE, gridPaint )
        canvas.drawLine( 0, CELL_SIZE * y, GRID_SIZE, CELL_SIZE * y, gridPaint )
      }
    }

    def drawLinks()
    {

    }

    def drawTokens()
    {
      tokenPaint.setColor( Color.RED )

      for {
        token <- model.tokens
        color <- colorMap.get( token.color ) }
      {
        tokenPaint.setColor( color )

        val x = token.position.x * CELL_SIZE + CELL_SIZE / 2
        val y = token.position.y * CELL_SIZE + CELL_SIZE / 2
        canvas.drawCircle( x, y, TOKEN_RADIUS, tokenPaint )
      }
    }

    drawEmptyGrid()
    drawTokens()
    drawLinks()
  }

  def screenSize =
  {
    val wm = context.getSystemService(Context.WINDOW_SERVICE).asInstanceOf[WindowManager]
    val display = wm.getDefaultDisplay()
    var screenSize = new Point()
    display.getSize(screenSize)
    ( screenSize.x, screenSize.y )
  }
}
