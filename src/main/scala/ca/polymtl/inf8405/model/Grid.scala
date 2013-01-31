package ca.polymtl.inf8405.model

import collection.immutable.Set

object GridFactory
{
  def sevenBySeven = SevenBySeven
  object SevenBySeven
  {
    def level1: Grid = ???
    def level2: Grid = ???
    def level3: Grid = ???
  }

  def heightByHeight = EightByEight
  object EightByEight
  {
    def level1: Grid = ???
    def level2: Grid = ???
    def level3: Grid = ???
  }
}

/*
 * 0,0 is the bottom left corner
 *
 * @param width
 * @param height
 */
case class Grid(
  cells: Map[Coordinate,Cell],
  width: Int, height: Int )
{
  def link( from: Coordinate, to: Coordinate ): Grid =
  {
    def update( current: Cell, remove: List[Coordinate] ) =
    {
      val removeSet = Set.empty ++ remove

      copy( cells = (
        ( cells filterKeys( removeSet contains ) ) +
        ( from -> current.link( to ) ) +
        ( to -> Link(from, None ) )
      ))
    }

    ( cells.get( from ), cells.get( to ) ) match
    {
      case ( Some( cell ), None ) =>
      {
        update(
          cell,
          followingLinksCoordinate( from )
        )
      }
      case ( Some( cellFrom ), Some( Link( prev, _ ) ) ) =>
      {
        update(
          cellFrom,
          followingLinksCoordinate( from ) ++
          followingLinksCoordinate( prev )
        )
      }
      case ( Some( cellFrom ), Some( Marker( color, _ ) ) ) =>
      {
        this
      }
      case _ => this
    }
  }

  def followingLinksCoordinate( coordinate: Coordinate ): List[Coordinate] =
  {
    cells.get( coordinate ).map{
      case Link( _, Some( next ) ) => coordinate :: followingLinksCoordinate( next )
      case Marker( _, Some( next ) ) => coordinate :: followingLinksCoordinate( next )
      case _ => Nil
    }.getOrElse( Nil )
  }

  def color( cell: Cell ): Option[Color] =
  {
    cell match
    {
      case Link( from, _ ) => cells get( from ) flatMap( color _ )
      case Marker( color, _ ) => Some( color )
    }
  }

  def isFull: Boolean = ???
  def isAllLinked: Boolean = ???
}

sealed abstract class Cell
{
  def to: Option[Coordinate]
  def link( next: Coordinate ): Cell
}

case class Link( from: Coordinate, to: Option[Coordinate] ) extends Cell
{
  def link( next: Coordinate ) = copy( to = Some( next ) )
}

case class Marker( color: Color, to: Option[Coordinate] = None ) extends Cell
{
  def link( next: Coordinate ) = copy( to = Some( next ) )
}

case class Coordinate( x: Int, y: Int )
case class Color( code: Int )
