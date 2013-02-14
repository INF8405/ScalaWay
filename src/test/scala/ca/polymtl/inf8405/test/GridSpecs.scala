package ca.polymtl.inf8405
package test

import model._

import org.scalatest._
import matchers.ShouldMatchers

class GridSpecs extends FunSpec with ShouldMatchers
{
  describe("linking")
  {
    it("should create the desire structure")
    {
      val tokenCoordinate = Coordinate( 0, 0 )
      val red = Color(1)
      val token = Token(red, tokenCoordinate)
      val grid = FastGrid( Grid(
        List( token ),
        Nil, 2
      ), tokenCoordinate )

      ( grid >> Down >> Right >> Up ).grid should be
      {
        Grid(
          List( Token( red, tokenCoordinate ) ),
          List( Link( Link( Link( token, Down ), Right ), Up )
          ),
          2
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
      Nil, 3
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
        Nil, 3
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
      //   x      x
      //   1  >>  1 2
      // 3 2

      def markerPos = Coordinate( 1, 0 )
      def breakPos = markerPos + Down

      def grid = FastGrid( Grid(
        List( Token( Color(1), markerPos )),
        Nil,
        3
      ), markerPos)

      grid >> Down >> Down >> Left <*> breakPos >> Right should be(
        grid >> Down >> Right
      )
    }
  }

  describe("a end link")
  {
    it("should link a link to a token")
    {
      val t1 = Token( Color( 1 ), Coordinate( 0, 0 ) )
      val t2 = Token( Color( 1 ), Coordinate( 1, 0 ) )

      assert( Link( t1, Right ).isEnd( t2 ) )
    }

    it("shoul not link if token have different colors")
    {
      val t1 = Token( Color( 1 ), Coordinate( 0, 0 ) )
      val t2 = Token( Color( 2 ), Coordinate( 1, 0 ) )

      assert( !Link( t1, Right ).isEnd( t2 ) )
    }

    it("should link a link to a link")
    {
      val t1 = Token( Color( 1 ), Coordinate( 0, 0 ) )
      val t2 = Token( Color( 1 ), Coordinate( 2, 2 ) )

      val l1 = Link( Link( t1, Down ), Down )
      val l2 = Link( Link( t2, Left ), Left )

      assert( l1.isEnd( l2 ) )
    }

    it("should link a link to a link in a grid")
    {
      val red = Color(1)
      val t1p = Coordinate( 0, 0 )
      val t1 = Token( red, t1p )
      val t2p = Coordinate( 1, 0 )
      val t2= Token( red, t2p )

      val fg = FastGrid( Grid( List( t1, t2 ), Nil, 3 ), t1p )

      fg >> Down >> Right >> Right <*> t2p >> Down should be {
        fg <*> t2p >> Down >> Left >> Up
      }
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
      2
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
    // r1 r2
    // b1 b2

    val red = Color(1)
    val blue = Color(2)
    val redPos1 = Coordinate(0,0)
    val bluePos1 = Coordinate(0,1)
    val grid = FastGrid(
      Grid(
        List(
          Token( red, redPos1 ),              // r1
          Token( red, bluePos1 ),             // b1
          Token( red, Coordinate( 1, 0 ) ),   // r2
          Token( red, Coordinate( 1, 1 ) )    // b2
        ),
        links = Nil,
        2
      ),
      redPos1
    )

    val solvedGrid = ( grid >> Right <*> bluePos1 >> Right ).grid

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
        2
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
    // r1 r2
    // b1 b2

    val red = Color(1)
    val blue = Color(2)
    val redPos1 = Coordinate(0,0)
    val bluePos1 = Coordinate(0,1)
    val grid = FastGrid(
      Grid(
        List(
          Token( red, redPos1 ),              // r1
          Token( red, bluePos1 ),             // b1
          Token( red, Coordinate( 1, 0 ) ),   // r2
          Token( red, Coordinate( 1, 1 ) )    // b2
        ),
        links = Nil,
        2
      ),
      redPos1
    )

    val solvedGrid = ( grid >> Right <*> bluePos1 ).grid

    it("should be like that")
    {
      assert( !solvedGrid.isAllLinked )
      assert( solvedGrid.isFull )
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
        2
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