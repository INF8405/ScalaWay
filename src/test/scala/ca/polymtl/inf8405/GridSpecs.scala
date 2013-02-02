package ca.polymtl.inf8405

import model._

import org.scalatest._
import matchers.ShouldMatchers

class GridSpecs extends FunSpec with ShouldMatchers
{
  describe("linking")
  {
    it("should creat the desire structure")
    {
      val tokenCoordinate = Coordinate( 0, 0 )
      val red = Color(1)
      val token = Token(red, tokenCoordinate)
      val grid = FastGrid( Grid(
        List( token ),
        Nil, 2, 2
      ), tokenCoordinate )

      ( grid >> Up >> Right >> Down ).grid should be
      {
        Grid(
          List( Token( red, tokenCoordinate ) ),
          List( Link( Link( Link( token, Up ), Right ), Down )
          ),
          2, 2
        )
      }
    }
  }

  describe("a self breaking link")
  {
    val tokenCoordinate = Coordinate( 1, 0 )
    val red = Color(1)
    val token = Token(red, tokenCoordinate)
    val grid = FastGrid( Grid(
      List( token ),
      Nil, 2, 3
    ), tokenCoordinate )

    it("breaks on a marker")
    {
      //  2 1       _ _
      //  3 x   >>  _ x

      grid >> Up >> Left >> Down >> Right should be( grid )
    }

    it("breaks on a link")
    {
      //  3 2       _ _
      //  4 1   >>  _ 1
      //    x         x

      grid >> Up >> Up >> Left >> Down >> Right should be( grid >> Up )
    }
  }

  describe("a breaking link")
  {
    it("should `cut` other links")
    {
      val rtc = Coordinate( 0, 0 )
      val red = Color(1)
      val rt = Token(red, rtc)

      val btc = Coordinate( 1, 0 )
      val blue = Color(2)
      val bt = Token(blue, btc)


      val grid = FastGrid( Grid(
        List( rt, bt ),
        Nil, 2, 2
      ), rtc )

      //  2 4     4 3
      //  1 3 >>  1 2
      //  x o     x o

      grid >> Up >> Up <*> btc >> Up >> Up >> Left should be(
        grid >> Up <*> btc >> Up >> Up >> Left
      )
    }

    it("can start inside of a link")
    {
      // 3 2
      //   1  >>  1 2
      //   x      x

      def markerPos = Coordinate( 1, 0 )
      def breakPos = markerPos + Up

      def grid = FastGrid( Grid(
        List( Token( Color(1), markerPos )),
        Nil,
        3,3
      ), markerPos)

      grid >> Up >> Up >> Left <*> breakPos >> Right should be(
        grid >> Up >> Right
      )
    }
  }

  describe("a full-allLinked grid")
  {
    //  b - b
    //  r - r

    val red = Color(1)
    val redCoord1 = Coordinate( 0, 0 )


    val blue = Color(2)
    val blueCoord1 = Coordinate( 0, 1 )


    val grid = FastGrid( Grid(
      List(
        Token( red, redCoord1 ),
        Token( red, Coordinate( 0, 2 ) ),
        Token( blue, blueCoord1 ),
        Token( blue, Coordinate( 2, 1 ) )
      ),
      Nil,
      2, 2
    ), redCoord1)

    val solvedGrid = ( grid >> Right >> Right <*> blueCoord1 >> Right >> Right ).grid

    it("should be like that")
    {
      assert( solvedGrid.isAllLinked )
      assert( solvedGrid.isFull )
    }
  }

  describe("minimal complete grid")
  {
    // r r

    val red = Color(1)
    val redPos1 = Coordinate(0,0)
    val grid = FastGrid(
      Grid(
        List( Token( red, Coordinate( 1, 0 ) ) ),
        Nil,
        2, 1
      ),
      redPos1
    )

    val solvedGrid = ( grid >> Right ).grid

    it("should be like that")
    {
      assert( solvedGrid.isAllLinked )
      assert( solvedGrid.isFull )
    }
  }

  describe("a non-full-allLinked grid")
  {
    //  _ _
    //  r r
    val red = Color(1)
    val redPos1 = Coordinate(0,0)
    val grid = FastGrid(
      Grid(
        List(
          Token( red, redPos1 ),
          Token( red, Coordinate( 1, 0 ) )
        ),
        Nil,
        2, 2
      ),
      redPos1
    )

    val solvedGrid = ( grid >> Right ).grid

    it("should be like that")
    {
      assert( solvedGrid.isAllLinked )
      assert( !solvedGrid.isFull )
    }
  }

  describe("full not linked")
  {
    // r r
    val red = Color(1)
    val redPos1 = Coordinate(0,0)
    val grid = Grid( List(
      Token( red, redPos1 ),
      Token( red, Coordinate( 1, 0  ) )
      ),
      Nil,
      2, 1
    )

    it("should be like that")
    {
      assert( !grid.isAllLinked )
      assert( grid.isFull )
    }
  }

  describe("an out of bound link")
  {
    it("should ignore linking and return unmodified grid")
    {
      //  b b
      //  r r

      val red = Color(1)
      val rm1c = Coordinate( 0, 0 )
      val rm2c = Coordinate( 1, 0 )

      val blue = Color(2)
      val bm2c = Coordinate( 1, 1 )
      val bm1c = Coordinate( 0, 1 )

      val grid = Grid(
        List(
          Token( red, rm1c ),
          Token( red, rm2c ),
          Token( blue, bm1c ),
          Token( blue, bm2c  )
        ),
        Nil,
        2, 2
      )

      val gridRed = FastGrid( grid, rm1c )
      gridRed >> Down should be ( gridRed )   // Overflow down
      gridRed >> Left should be ( gridRed )   // Overflow left

      val gridBlue = FastGrid( grid, bm2c )
      gridBlue >> Up should be( gridBlue )    // Overflow up
      gridBlue >> Right should be( gridBlue ) // Overflow right
    }
  }
}

/*
 * Starting from a position we can compose ( >> ) and set a new starting position ( <*> )
 */
case class FastGrid( grid: Grid, from: Coordinate )
{
  def >>( direction: Direction ) = FastGrid( grid.link( from, direction ), from + direction )
  def <*>( newFrom: Coordinate ) = copy( from = newFrom )

  override def toString = grid.toString
}