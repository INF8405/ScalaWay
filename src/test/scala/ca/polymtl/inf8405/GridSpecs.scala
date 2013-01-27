package ca.polymtl.inf8405

import model._

import org.scalatest._
import matchers.ShouldMatchers

class GridSpecs extends FunSpec with ShouldMatchers
{
  describe("a self breaking link")
  {
    def tokenCoordinate = Coordinate( 40, 40 )

    def grid = FastGrid( Grid(
      List( Token( Color( 1 ), tokenCoordinate ) ),
      Nil,
      100, 100
    ), tokenCoordinate )

    it("breaks on marker")
    {
      //  2 1       _ _
      //  3 x   >>  _ x

      grid >> Up >> Left >> Down >> Right should be( grid )
    }

    it("breaks on link")
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
      def t1c = Coordinate( 0, 0 )
      def t2c = Coordinate( 1, 0 )

      def grid = FastGrid( Grid(
        List(
          Token( Color( 1 ), t1c ),
          Token( Color( 2 ), t2c )
        ),
        Nil,
        100, 100
      ), t1c )

      //  2 4     4 3
      //  1 3 >>  1 2
      //  x o     x o

      grid >> Up >> Up <*> t2c >> Up >> Up >> Left should be(
        grid >> Up <*> t2c >> Up >> Up >> Left
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
        List( Token( Color(1), markerPos ) ),
        Nil,
        100,100
      ), markerPos)

      grid >> Up >> Up >> Left <*> breakPos >> Right should be(
        grid >> Up >> Right
      )
    }
  }

  describe("a full-allLinked grid")
  {
    //  b b
    //  r r

    val red = Color(1)
    val redMarker1 = Token( red, Coordinate( 0, 0 ) )
    val redMarker2 = Token( red, Coordinate( 1, 0 ) )

    val blue = Color(2)
    val blueMarker1 = Token( blue, Coordinate( 0, 1 ) )
    val blueMarker2 = Token( blue, Coordinate( 1, 1 ) )

    val grid = Grid(
      List( redMarker1, redMarker2, blueMarker1, blueMarker2 ),
      List( Link( redMarker1, Right ), Link( blueMarker1, Right ) ),
      2, 2
    )

    it("should be like that")
    {
      assert( grid.isAllLinked )
      assert( grid.isFull )
    }
  }

  describe("a non-full-allLinked grid")
  {
    //  _ _
    //  r r

    val red = Color(1)
    val redMarker1 = Token( red, Coordinate( 0, 0 ) )
    val redMarker2 = Token( red, Coordinate( 1, 0 ) )

    val grid = Grid(
      List( redMarker1, redMarker2 ),
      List( Link( redMarker1, Right ) ),
      2, 2
    )

    it("should be like that")
    {
      assert( !grid.isFull )
      assert( grid.isAllLinked )
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
      val redMarker1 = Token( red, rm1c )
      val redMarker2 = Token( red, Coordinate( 1, 0 ) )

      val blue = Color(2)
      val bm2c = Coordinate( 1, 1 )
      val blueMarker1 = Token( blue, Coordinate( 0, 1 ) )
      val blueMarker2 = Token( blue, bm2c )

      val grid = Grid(
        List( redMarker1, redMarker2, blueMarker1, blueMarker2 ),
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
}
