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
      val grid = FastGrid( Grid(
        Map( tokenCoordinate -> Marker( red ) ),
        2, 2
      ), tokenCoordinate )

      ( grid >> Up >> Right >> Down ).grid should be
      {
        Grid( Map(
          tokenCoordinate -> Marker( red, Some( Coordinate( 0, 1 ) ) ),
          Coordinate( 0, 1 ) -> Link( tokenCoordinate, Some( Coordinate( 1, 1 ) ) ),
          Coordinate( 1, 1 ) -> Link( Coordinate( 0, 1 ), Some( Coordinate( 1, 0 ) ) ),
          Coordinate( 1, 0 ) -> Link( Coordinate( 1, 1 ), None )
        ), 2, 2 )
      }
    }
  }
  
  describe("a self breaking link")
  {
    def tokenCoordinate = Coordinate( 40, 40 )

    def grid = FastGrid( Grid(
      Map( tokenCoordinate -> Marker( Color( 1 ) ) ),
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
      //  4 1   >>  _ 13
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
        Map(
          t1c -> Marker( Color( 1 ) ),
          t2c -> Marker( Color( 2 ) )
        ),
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
      def breakPos = markerPos +: Up

      def grid = FastGrid( Grid(
        Map( markerPos -> Marker( Color(1) ) ),
        100,100
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
      Map(
        redCoord1 -> Marker( red ),
        Coordinate( 0, 2 ) -> Marker( red ),
        blueCoord1 -> Marker( blue ),
        Coordinate( 2, 1 ) -> Marker( blue )
      ),
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
    val grid = FastGrid( Grid( Map(
        redPos1 -> Marker( red ),
        Coordinate( 1, 0 ) -> Marker( red )
      ), 2, 1),
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
    val grid = FastGrid( Grid( Map(
      redPos1 -> Marker( red ),
      Coordinate( 1, 0 ) -> Marker( red )
    ), 2, 2),
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
    val grid = Grid( Map(
      redPos1 -> Marker( red ),
      Coordinate( 1, 0 ) -> Marker( red )
    ), 2, 1)

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
        Map(
          rm1c -> Marker( red ),
          rm2c -> Marker( red ),
          bm1c -> Marker( blue ),
          bm2c -> Marker( blue )
        ),
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
  def >>( direction: Direction ) = {
    val newPos = from +: direction
    FastGrid( grid.link( from, newPos ), newPos )
  }
  def <*>( newFrom: Coordinate ) = copy( from = newFrom )
}

sealed abstract class Direction
{
  def +: ( coordinate: Coordinate ): Coordinate
}
case object Up extends Direction
{
  def +: ( coordinate: Coordinate ) = coordinate.copy( y = coordinate.y + 1 )
}
case object Down extends Direction
{
  def +: ( coordinate: Coordinate ) = coordinate.copy( y = coordinate.y - 1 )
}
case object Left extends Direction
{
  def +: ( coordinate: Coordinate ) = coordinate.copy( x = coordinate.x - 1 )
}
case object Right extends Direction
{
  def +: ( coordinate: Coordinate ) = coordinate.copy( x = coordinate.x + 1 )
}