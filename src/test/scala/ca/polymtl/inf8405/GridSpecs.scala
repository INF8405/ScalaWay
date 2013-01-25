package ca.polymtl.inf8405

import model._

import org.scalatest._
import matchers.ShouldMatchers

class GridSpecs extends FunSpec with ShouldMatchers
{
  def tokenCoordinate = Coordinate( 40, 40 )

  def grid = FastGrid( Grid(
    List( Token( Color( 1 ), tokenCoordinate ) ),
    Nil,
    100, 100
  ), tokenCoordinate )

  describe("a breaking link")
  {
    it("breaks a link when they collide back to marker")
    {
      //  2 1       _ _
      //  3 x   >>  _ x

      grid >> Up >> Left >> Down >> Right should be( grid )
    }

    it("breaks a link leaving a trail when collide back to itself")
    {
      //  3 2       _ _
      //  4 1   >>  _ 1
      //    x         x

      grid >> Up >> Up >> Left >> Down >> Right should be( grid >> Up )
    }
  }
}

case class FastGrid( grid: Grid, from: Coordinate )
{
  def >>( direction: Direction ) = new FastGrid( grid.link( from, direction ), from + direction )
}
