package ca.polymtl.inf8405.model

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

case class Grid( tokens: List[Token], links: List[Link], width: Int, height: Int )
{
  import scala.collection.JavaConversions._

  def jtokens: java.util.List[Token] = tokens
  def jlinks: java.util.List[Link] = links

  def link( from: Coordinate, direction: Direction ): Grid =
  {
    ???
  }

  def isFull: Boolean = ???
  def isAllLinked: Boolean = ???
}

trait Linkable
{
  def position: Coordinate
}
case class Token( color: Color, coordinate: Coordinate ) extends Linkable
{
  def position = coordinate
}
case class Link( from: Linkable, direction: Direction ) extends Linkable
{
  def position = from.position + direction
}

sealed abstract class Direction
{
  def + ( coordinate: Coordinate ): Coordinate
}
case object Up extends Direction
{
  def + ( coordinate: Coordinate ) = coordinate.copy( y = coordinate.y + 1 )
}
case object Down extends Direction
{
  def + ( coordinate: Coordinate ) = coordinate.copy( y = coordinate.y - 1 )
}
case object Left extends Direction
{
  def + ( coordinate: Coordinate ) = coordinate.copy( x = coordinate.x - 1 )
}
case object Right extends Direction
{
  def + ( coordinate: Coordinate ) = coordinate.copy( x = coordinate.x + 1 )
}

case class Coordinate( x: Int, y: Int )
{
  def +( direction: Direction ) = direction + this
}

case class Color( code: Int )
