package ca.polymtl.inf8405
package test

import model._

import org.scalatest._
import matchers.ShouldMatchers
import scala.collection

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
        Set( token ),
        Set(), 2
      ), tokenCoordinate )

      ( grid >> Down >> Right >> Up ).grid should be
      {
        Grid(
          Set( Token( red, tokenCoordinate ) ),
          Set( Link( Link( Link( token, Down ), Right ), Up )
          ),
          2
        )
      }
    }

    it("shall not pass over tokens")
    {
      //  _   b
      //  r1  r2


      val r1c = Coordinate( 0, 1 )
      val bc = Coordinate( 1, 0 )

      val red = Color(1)
      val blue = Color(2)

      val grid = FastGrid( Grid(
        Set(
          Token( red, r1c ),
          Token( red, Coordinate( 1, 1 ) ),
          Token( blue, bc )
        ),
        Set(), 2
      ), r1c )

      ( grid >> Right <*> bc >> Down ) should be
      {
        grid >> Right
      }
    }
  }

  describe("a token")
  {
    it("should not have two links")
    {
      val red = Color(1)
      val tokenPos = Coordinate(0,0)
      val grid =
        Grid(
          Set(
            Token( red, tokenPos )
          ),
          links = Set(),
          2
        )

      grid link( tokenPos, Right ) link( tokenPos, Down ) should be(
        grid.link( tokenPos, Down )
      )
    }
  }

  describe("a self breaking link")
  {
    val tokenCoordinate = Coordinate( 1, 2 )
    val red = Color(1)
    val token = Token(red, tokenCoordinate)
    val grid = FastGrid( Grid(
      Set( token ),
      Set(), 3
    ), tokenCoordinate )

    it("breaks on a token")
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
      val rtc = Coordinate( 0, 2 )
      val red = Color(1)
      val rt = Token(red, rtc)

      val btc = Coordinate( 1, 2 )
      val blue = Color(2)
      val bt = Token(blue, btc)


      val grid = FastGrid( Grid(
        Set( rt, bt ),
        Set(), 3
      ), rtc )

      //  2 4     4 3
      //  1 3 >>  1 2
      //  r b     r b

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
        Set( Token( Color(1), markerPos )),
        Set(),
        3
      ), markerPos)

      grid >> Down >> Down >> Left <*> breakPos >> Right should be(
        grid >> Down >> Right
      )
    }
  }

  describe("a end link")
  {
    it("should not be an end link if not linked")
    {
      val t1 = Token( Color( 1 ), Coordinate( 0, 0 ) )
      val t2 = Token( Color( 1 ), Coordinate( 1, 0 ) )

      assert( ! t1.isEnd( t2 ) )
    }

    it("should not be an end link if not linked far away")
    {
      val t1 = Token( Color( 1 ), Coordinate( 0, 0 ) )
      val t2 = Token( Color( 1 ), Coordinate( 5, 5 ) )

      assert( ! t1.isEnd( t2 ) )
    }

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

    it("should be an end link if a linkable links to an other linkable")
    {
      //  t1
      //   3
      //   4,2  1 t2


      val t1 = Token( Color( 1 ), Coordinate( 0, 0 ) )
      val t2 = Token( Color( 1 ), Coordinate( 2, 2 ) )

      val l1 = Link( Link( t1, Down ), Down )
      val l2 = Link( Link( t2, Left ), Left )

      assert( l1.isEnd( l2 ) )
    }

    it("should be an end link if a linkable links to an other linkable in a grid")
    {
      val red = Color(1)
      val t1p = Coordinate( 0, 0 )
      val t1 = Token( red, t1p )
      val t2p = Coordinate( 1, 0 )
      val t2= Token( red, t2p )

      val fg = FastGrid( Grid( Set( t1, t2 ), Set(), 3 ), t1p )

      fg >> Down >> Right >> Right <*> t2p >> Down should be {
        fg <*> t2p >> Down >> Left >> Up
      }
    }

    it("should match the other end")
    {
      val red = Color(1)
      val t1p = Coordinate( 0, 0 )
      val t1 = Token( red, t1p )
      val t2p = Coordinate( 0, 5 )
      val t2 = Token( red, t2p )

      val link = Link( t1, Right )

      assert( !link.isEnd( t2 ) )
    }
  }

  describe("an origin of a link")
  {
    it("should be the token at the root of a link")
    {
      val token = Token( Color(1), Coordinate(1,1) )
      val falseToken = Token( Color(1), Coordinate(2,2) )
      val link = Link( Link( Link( token, Up ), Left ), Down )

      assert( link.isOrigin( token ) )
      assert( !link.isOrigin( falseToken ) )
    }
  }

  describe("a full-allLinked grid")
  {
    //  r - r
    //  b - b

    val red = Color(1)
    val redCoord1 = Coordinate( 0, 0 )


    val blue = Color(2)
    val blueCoord1 = Coordinate( 0, 1 )

    val green = Color(3)
    val greenCoord1 = Coordinate( 0, 2 )

    val grid = FastGrid( Grid(
      Set(
        Token( red, redCoord1 ),
        Token( red, Coordinate( 2, 0 ) ),
        Token( blue, blueCoord1 ),
        Token( blue, Coordinate( 2, 1 ) ),
        Token( green, greenCoord1 ),
        Token( green, Coordinate( 2, 2 ) )
      ),
      Set(),
      3
    ), redCoord1)

    val solvedGrid = ( grid >> Right >> Right <*> blueCoord1 >> Right >> Right <*> greenCoord1 >> Right >> Right ).grid

    it("should be like that")
    {
      assert( 3 === solvedGrid.tubesDone )
      assert( solvedGrid.isAllLinked )
      assert( solvedGrid.isFull === true)
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
        Set(
          Token( red, redPos1 ),              // r1
          Token( blue, bluePos1 ),            // b1
          Token( red, Coordinate( 1, 0 ) ),   // r2
          Token( blue, Coordinate( 1, 1 ) )   // b2
        ),
        links = Set(),
        2
      ),
      redPos1
    )

    val solvedGrid = ( grid >> Right <*> bluePos1 >> Right ).grid

    it("should be like that")
    {
      assert( 2 === solvedGrid.tubesDone )
      assert( solvedGrid.isAllLinked === true)
      assert( solvedGrid.isFull === true)
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
        Set(
          Token( red, redPos1 ),
          Token( red, Coordinate( 1, 0 ) )
        ),
        Set(),
        2
      ),
      redPos1
    )

    val solvedGrid = ( grid >> Right ).grid

    it("should be like that")
    {
      assert( 1 === solvedGrid.tubesDone )
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
        Set(
          Token( red, redPos1 ),                // r1
          Token( blue, bluePos1 ),              // b1
          Token( red, Coordinate( 1, 0 ) ),     // r2
          Token( blue, Coordinate( 1, 1 ) )     // b2
        ),
        links = Set(),
        2
      ),
      redPos1
    )

    val notSolvedGrid = ( grid >> Right ).grid

    it("should be like that")
    {
      assert( 1 === notSolvedGrid.tubesDone )
      assert( notSolvedGrid.isAllLinked === false )
      assert( notSolvedGrid.isFull === true )
    }
  }

  describe("an out of bound link")
  {
    it("should ignore linking and return unmodified grid")
    {
      //  t1 t3
      //  t2 t4

      val red = Color(1)
      val t1c = Coordinate( 0, 0 )
      val t2c = Coordinate( 0, 1 )
      val t3c = Coordinate( 1, 0 )
      val t4c = Coordinate( 1, 1 )

      val grid = Grid(
        Set(
          Token( red, t1c ),
          Token( red, t2c ),
          Token( red, t3c ),
          Token( red, t4c  )
        ),
        Set(),
        2
      )

      val grid1 = FastGrid( grid, t1c )
      grid1 >> Up should be ( grid1 )
      grid1 >> Left should be ( grid1 )

      val grid2 = FastGrid( grid, t4c )
      grid2 >> Down should be( grid2 )
      grid2 >> Right should be( grid2 )

      val grid3 = FastGrid( grid, t3c )
      grid3 >> Up should be( grid3 )
      grid3 >> Right should be( grid3 )

      val grid4 = FastGrid( grid, t3c )
      grid4 >> Up should be( grid4 )
      grid4 >> Right should be( grid4 )
    }

    it("breaks on a marker with invalid coord")
    {
      //    ov
      //    ov
      //  1 x
      //  2 3

      val red = Color(1)
      val tc = Coordinate( 1, 0 )
      val grid = FastGrid( Grid(
        Set( Token( red, tc ) ),
        Set(),
        2
      ), tc )

      grid    >>
        Up    >>     // Overflow
        Up    >>     // Overflow
        Left  >>
        Down  >>
        Right should be (

        grid >> Left >> Down >> Right
      )
    }
  }

  describe("Coords")
  {
    it("should return all coordinates 2x2 grid")
    {
      val grid = Grid(Set.empty[Token], Set.empty[Link], 2)
      val allCoords = grid.coords
      assert ( allCoords.flatten.size === 4 )
      assert ( allCoords.flatten.contains(Coordinate(0,0)))
      assert ( allCoords.flatten.contains(Coordinate(0,1)))
      assert ( allCoords.flatten.contains(Coordinate(1,0)))
      assert ( allCoords.flatten.contains(Coordinate(1,1)))
    }

    it("should return all coordinates 3x3 grid")
    {
      val grid = Grid(Set.empty[Token], Set.empty[Link], 3)
      val allCoords = grid.coords
      val l = List(Coordinate(0,0), Coordinate(1,0), Coordinate(2,0),
        Coordinate(0,1), Coordinate(1,1), Coordinate(2,1),
        Coordinate(0,2), Coordinate(1,2), Coordinate(2,2))
      assert ( allCoords.flatten.size === 9 )

      val b = allCoords.flatten.forall( coord => l.contains(coord) )
      assert( b === true)
      //assert ( allCoords.flatten ===  l)
    }
  }

  describe("Link between token(0,0) and token(2,0) in 3x3 grid")
  {
    //  r - r
    val red = Color(1)
    val redCoord1 = Coordinate( 0, 0 )

    val grid = FastGrid( Grid(
      Set(
        Token( red, redCoord1 ),
        Token( red, Coordinate( 2, 0 ) )
      ),
      Set(),
      3
    ), redCoord1)

    val solvedGrid = ( grid >> Right >> Right ).grid

    it("should have a coordinate 1 0")
    {
      assert( solvedGrid.links.head.from.position === Coordinate(1,0))
    }
  }

  describe("Sublinkable ")
  {
    val red = Token(Color(1), Coordinate(0,0))
    val link = Link(red, Right)
    val link2 = Link(link, Right)
    it("should return all sublinks and tokens in the link (token, link)")
    {
      link.subLinkables should be (List(link, red))
    }
    it("should return all sublinks and tokens in the link (token, link, link2)")
    {
      link2.subLinkables should be (List(link2, link, red))
    }
  }
}