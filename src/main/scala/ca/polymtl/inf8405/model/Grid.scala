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

  def find( coordinate: Coordinate ) =
  {
    val tokenPosition = tokens.find( _.position == coordinate )

    if ( tokenPosition.isEmpty ) links.find( _.position == coordinate )
    else tokenPosition
  }

  def link( from: Coordinate, direction: Direction ): Grid =
  {
    val to = from + direction

    ( find( from ), find( to ) ) match {
      case ( None, _ ) => this
      case ( Some( linkable ), None ) =>
      {
        //links.find( _.from == from ).map( )

        copy( links = Link( linkable, direction ) :: links )
      }
      case ( Some( linkable1 ), Some( linkable2 ) ) =>
      {
        if ( linkable1 sameColorAs linkable2 )
        // self link ? => destroy and return this
        // other link ? => destroy and link

        // marker - marker ?

        this
      }
      case _ => this
    }

    ???
  }

  def isFull: Boolean = ???
  def isAllLinked: Boolean = ???
}

trait Linkable
{
  def position: Coordinate
  def sameColorAs( other: Linkable ) = other == this
}

case class Token( color: Color, coordinate: Coordinate ) extends Linkable
{
  def position = coordinate
}
case class Link( from: Linkable, direction: Direction ) extends Linkable
{
  def position = from.position + direction
  override def sameColorAs( other: Linkable ) =
  {
    if( super.sameColorAs( other ) || other == from ) true
    else from.sameColorAs( other )
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
