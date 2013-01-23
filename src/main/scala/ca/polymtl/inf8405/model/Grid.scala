package ca.polymtl.inf8405.model

case class Grid( cells: Map[Coordinate, Cell], width: Int, height: Int )
{
  require( linksInside )

  def link( from: Coordinate, to: Coordinate ) =
  {
    ( cells.get( from ), cells.get( to ) ) match
    {
      case ( Some( cell ), None ) => update( cell, to )
      case ( Some( fromCell ), Some( toCell ) ) => update( fromCell, toCell )
      case _ => this
    }
  }

  private def update( from: Cell, to: Coordinate ) =
  {

  }

  private def update( from: Cell, to: Cell ) =
  {

  }

  private def linksInside =
  {
    cells.forall{
      case ( k, _ ) => {
        0 <= k.x && k.x <= width &&
        0 <= k.y && k.y <= height
      }
    }
  }
}

abstract class Cell( color: Color, val coordinate: Coordinate )
{
  def link( to: Coordinate ): Update
  def link( to: Cell ): Update
}


class Link( color: Color, coordinate: Coordinate, next: Option[Link] ) extends Cell( color, coordinate )
{
  def nextChain0: List[Coordinate] = coordinate :: nextChain
  def nextChain = next.map( _.nextChain0 ).getOrElse( Nil )

  def link( to: Coordinate ): Update =
  {
    val nextCell = new Link( color, to, None )
    val thisCell = new Link( color, coordinate, Some( nextCell ) )
    new Update( nextChain, nextCell, thisCell )
  }

  def link( to: Cell ): Update =
  {
    null
  }
}

class Marker( color: Color, coordinate: Coordinate, next: Option[Link] ) extends Link( color, coordinate, next )


case class Coordinate( x: Int, y: Int )
case class Color( code: Int )

class Update( remove: List[Coordinate], add: Cell, update: Cell )
{
  def apply( cells: Map[Coordinate, Cell] ): Map[Coordinate,Cell] =
  {
    ( cells -- remove )
  }
}

// Empty - _ => Rien
// Marker - Empty => Link
//