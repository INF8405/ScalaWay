package ca.polymtl.inf8405
package view

import model._

import android.view.{WindowManager, View}
import android.content.Context
import android.graphics.{Point, Paint, Canvas, Color => AColor}
import model.Coordinate

class DrawView( context: Context, size: Int ) extends View( context )
{
  val ( width, height ) = screenSize
  val GRID_SIZE = Math.min( width, height )
  val CELL_SIZE = GRID_SIZE / size
  val TOKEN_RADIUS = 3 * CELL_SIZE / 8

  val model =
    FastGrid( GridFactory.SevenBySeven.level1, Coordinate( 0, 1 ) ) >>
    Right >> Right >> Right >> Down >> Down >> Down

  val gridPaint = new Paint
  gridPaint.setColor( AColor.BLACK )
  gridPaint.setStrokeWidth( 5 )

  val tokenPaint = new Paint
  tokenPaint.setStrokeWidth( 1 )

  val linkPaint = new Paint
  linkPaint.setStrokeWidth( 2 * TOKEN_RADIUS )

  val colorMap = Map(
    GridFactory.red -> AColor.RED,
    GridFactory.blue -> AColor.BLUE,
    GridFactory.orange -> AColor.rgb( 255, 204, 0 ),
    GridFactory.green -> AColor.GREEN,
    GridFactory.yellow -> AColor.YELLOW
  )

  setBackgroundColor( AColor.WHITE )

  override def onDraw( canvas: Canvas )
  {
    def drawEmptyGrid()
    {
      for {
        x <- 1 to model.grid.size
        y <- 1 to model.grid.size }
      {
        canvas.drawLine( CELL_SIZE * x, 0, CELL_SIZE * x, GRID_SIZE, gridPaint )
        canvas.drawLine( 0, CELL_SIZE * y, GRID_SIZE, CELL_SIZE * y, gridPaint )
      }
    }

    def drawLinks()
    {
      for { link <- model.grid.links }
      {
        def accPosition( aLink: Linkable ) : ( Color, List[Coordinate] ) =
        {
          aLink match
          {
            case Token( col, pos ) => ( col, List( pos ) )
            case Link( from, dir ) =>
            {
              val ( col, pos :: tail ) = accPosition( from )
              ( col, pos + dir :: pos :: tail )
            }
          }
        }

        val ( color, links ) = accPosition( link )

        def draw( color: Int, links: List[Coordinate] )
        {
          links match
          {
            case c1 :: c2 :: tail =>
            {
              val x1 = c1.x * CELL_SIZE + CELL_SIZE / 2
              val y1 = c1.y * CELL_SIZE + CELL_SIZE / 2

              val x2 = c2.x * CELL_SIZE + CELL_SIZE / 2
              val y2 = c2.y * CELL_SIZE + CELL_SIZE / 2

              linkPaint.setColor( color )
              tokenPaint.setColor( color )
              canvas.drawLine( x1, y1, x2, y2, linkPaint )
              canvas.drawCircle( x1, y1, TOKEN_RADIUS, tokenPaint )

              draw( color, c2 :: tail )
            }
            case _ :: Nil => ()
          }
        }

        for{ c <- colorMap.get( color ) } { draw( c, links ) }
      }
    }

    def drawTokens()
    {
      for {
        token <- model.grid.tokens
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
