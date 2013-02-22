package ca.polymtl.inf8405
package view

import model._
import controller._

import android.view.{WindowManager, View}
import android.content.Context
import android.graphics.{Point, Paint, Canvas, Color => AColor}
import model.Coordinate
import controller.GameController
import android.view.View.MeasureSpec

class DrawView( context: Context, size: Int, level: Int ) extends View( context )
{
  private var linkAmount = 0

  private val ( width, height ) = screenSize
  val GRID_SIZE = Math.min( width, height )
  private val CELL_SIZE = GRID_SIZE / size
  private val TOKEN_RADIUS = 3 * CELL_SIZE / 8
  private val LINK_OFFSET = 100

  val observer =
  {
    GridObserver( GridFactory.SevenBySeven.level1, new GridListener {
      def apply( event: GridEvent )
      {
        event match
        {
          case Complete =>
          {
            // bravo
          }
          case LinkChanged( amount ) =>
          {
            linkAmount = amount
          }
        }
      }
    })
  }

  setOnTouchListener( new GameController( this, observer ) )

  val gridPaint = new Paint
  gridPaint.setColor( AColor.BLACK )
  gridPaint.setStrokeWidth( 5 )

  val tokenPaint = new Paint
  tokenPaint.setStrokeWidth( 1 )

  val linkPaint = new Paint
  linkPaint.setStrokeWidth( TOKEN_RADIUS )

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
        x <- 1 to observer.grid.size
        y <- 1 to observer.grid.size }
      {
        canvas.drawLine( CELL_SIZE * x, 0, CELL_SIZE * x, GRID_SIZE, gridPaint )
        canvas.drawLine( 0, CELL_SIZE * y, GRID_SIZE, CELL_SIZE * y, gridPaint )
      }
    }

    def drawLinks()
    {
      for { link <- observer.grid.links }
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
              canvas.drawCircle( x1, y1, TOKEN_RADIUS / 2, tokenPaint )

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
        token <- observer.grid.tokens
        color <- colorMap.get( token.color ) }
      {
        tokenPaint.setColor( color )

        val x = token.position.x * CELL_SIZE + CELL_SIZE / 2
        val y = token.position.y * CELL_SIZE + CELL_SIZE / 2
        canvas.drawCircle( x, y, TOKEN_RADIUS, tokenPaint )
      }
    }

    def drawLinkAmount( )
    {
      val paint = new Paint()
      paint.setColor( AColor.GREEN )
      paint.setTextSize( CELL_SIZE )

      canvas.drawText( s"links: $linkAmount", 0, LINK_OFFSET, paint )
    }

    drawLinkAmount()
    drawEmptyGrid()
    drawTokens()
    drawLinks()
  }

  override def onMeasure( width: Int, height: Int )
  {
    val measuredWidth = MeasureSpec.getSize( width )
    val measuredHeight = MeasureSpec.getSize( height )

    val min = Math.min( measuredHeight, measuredWidth )

    setMeasuredDimension( min, min )
  }

  // TODO refactor trait
  def screenSize =
  {
    val wm = context.getSystemService(Context.WINDOW_SERVICE).asInstanceOf[WindowManager]
    val display = wm.getDefaultDisplay
    var screenSize = new Point
    display.getSize(screenSize)
    ( screenSize.x, screenSize.y )
  }
}
