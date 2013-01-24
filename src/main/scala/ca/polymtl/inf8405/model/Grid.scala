package ca.polymtl.inf8405.model

object GridFactory
{
  object SevenBySeven
  {
    def level1: Grid = ???
    def level2: Grid = ???
    def level3: Grid = ???
  }

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
  def position = ???
}

sealed abstract class Direction
case object Up extends Direction
case object Down extends Direction
case object Left extends Direction
case object Right extends Direction

case class Coordinate( x: Int, y: Int )
case class Color( code: Int )
