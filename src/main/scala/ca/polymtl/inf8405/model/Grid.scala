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

/*
 * 0,0 is the bottom left corner
 *
 * @param width
 * @param height
 */
case class Grid( tokens: List[Token], links: List[Link], width: Int, height: Int )
{
  import scala.collection.JavaConversions._

  def jtokens: java.util.List[Token] = tokens
  def jlinks: java.util.List[Link] = links

  def link( from: Coordinate, direction: Direction ): Grid =
  {
    if (isInvalidLink(from, direction)) this
    else
    {
      val fromLinkable = allLinkables.find(_.position == from)
      fromLinkable match
      {
        case Some(x) =>
        {
          val newLink = Link( x, direction )
          links.map { link =>
            val conflict = link.subLinks.find( _.position == newLink.position )
            conflict match
            {
              case None => link
              case Some( Link( from, _ ) ) => from
            }
          }

          val newLinks = links.filter( _.isInstanceOf[Link] ).map{
            case l if l == fromLinkable => newLink
            case l => l
          }
          Grid(tokens, newLinks, width, height)
        }
        case None => this
      }
    }
  }

  def isFull: Boolean =
  {
    val allCoords = List.tabulate(width, height){ case (x,y) => Coordinate(x, y) }.flatten
    allCoords.forall( coordinate => allLinkables.exists( _.position == coordinate ) )
  }
  def isAllLinked: Boolean = ???
  def allLinkables = tokens ++ links

  def isInvalidLink( from: Coordinate, to: Direction ): Boolean = false // TODO
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
  def subLinks: List[Link] = from match {
    case x: Link => this :: x.subLinks
    case x: Token => Nil
  }
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
