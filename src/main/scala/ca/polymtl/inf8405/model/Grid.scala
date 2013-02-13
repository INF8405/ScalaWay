package ca.polymtl.inf8405.model

object GridFactory
{
  val red = Color(1)
  val blue = Color(2)
  val green = Color(3)
  val yellow = Color(4)
  val orange = Color(5)

  object SevenBySeven
  {
    def level1: Grid = 
    {
      Grid( List(
        Token( red,     Coordinate( 2, 4 ) ),
        Token( red,     Coordinate( 4, 3 ) ),
        Token( blue,    Coordinate( 0, 0 ) ),
        Token( blue,    Coordinate( 0, 5 ) ),
        Token( green,   Coordinate( 0, 1 ) ),
        Token( green,   Coordinate( 5, 1 ) ),
        Token( yellow,  Coordinate( 2, 2 ) ),
        Token( yellow,  Coordinate( 4, 1 ) ),
        Token( orange,  Coordinate( 1, 1 ) ),
        Token( orange,  Coordinate( 4, 2 ) )
      ), Nil, 7, 7 )
    }
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
    if( isInvalidLink( from, direction ) ) this
    else
    {
      allLinkables.find( _.position == from ) match
      {
        case Some( fromLinkable ) =>
        {
          var addNewLink = true
          val newLink = Link( fromLinkable, direction )
          val mappedLinks = links.map { link =>
            
            val conflict = link.subLinks.find( _.position == newLink.position )

            conflict match
            {
              case None => Some( link )
              case Some( c @ Link( from: Link, _ ) ) => 
              {
                val ourSelf = fromLinkable == link
                addNewLink = !ourSelf

                if( ourSelf ) Some( c )
                else Some( from )
              }
              case Some( c @ Link( from: Token, _ ) ) =>
              {
                val ourSelf = fromLinkable == link
                addNewLink = !ourSelf

                if( ourSelf ) Some( c )
                else None
              }
              case _ => addNewLink = fromLinkable != link; None
            }
          }

          val reducedLink = mappedLinks.foldRight( List.empty[Link] ){ 
            case( Some( link ), acc ) => link :: acc
            case( _, acc ) => acc
          }
          
          val newLinks = 
            if( addNewLink ) newLink :: reducedLink.filter( _ != fromLinkable )
            else reducedLink.filter( _ != fromLinkable )

          Grid( tokens, newLinks, width, height )
        }
        case None => this
      }
    }
  }

  def coords = List.tabulate(width, height){ case (x,y) => Coordinate(x, y) } 

  def isFull: Boolean =
  {
    coords.flatten.forall( coordinate => allLinkables.exists( _.position == coordinate ) )
  }
  def isAllLinked: Boolean = ???
  def allLinkables = tokens ++ links

  def isInvalidLink( from: Coordinate, to: Direction ): Boolean = false // TODO

  // override def toString = 
  // {
  //   val out = for{
  //     column <- coords
  //     line <- column
  //   } yield allLinkables.find( _.position == line ) match
  //     {
  //       case Some( cell ) => cell.toString
  //       case None => " "
  //     }

  //   val out2 = for { l <- out } yield l.mkString( " " )
  //   out2.mkString( "\n" )
  // }
}

trait Linkable
{
  def position: Coordinate
  def subLinks: List[Linkable]
}
case class Token( color: Color, coordinate: Coordinate ) extends Linkable
{
  def position = coordinate
  def subLinks = List(this)
  override def toString = color.toString
}
case class Link( from: Linkable, direction: Direction ) extends Linkable
{
  def position = from.position + direction
  def subLinks = this :: from.subLinks
  override def toString = direction.toString
}

sealed abstract class Direction
{
  def + ( coordinate: Coordinate ): Coordinate
}
case object Up extends Direction
{
  def + ( coordinate: Coordinate ) = coordinate.copy( y = coordinate.y + 1 )
  override def toString = "^"
}
case object Down extends Direction
{
  def + ( coordinate: Coordinate ) = coordinate.copy( y = coordinate.y - 1 )
  override def toString = "v"
}
case object Left extends Direction
{
  def + ( coordinate: Coordinate ) = coordinate.copy( x = coordinate.x - 1 )
  override def toString = "<"
}
case object Right extends Direction
{
  def + ( coordinate: Coordinate ) = coordinate.copy( x = coordinate.x + 1 )
  override def toString = ">"
}

case class Coordinate( x: Int, y: Int )
{
  def +( direction: Direction ) = direction + this
}

case class Color( code: Int )
{
  override def toString = code.toString
}
