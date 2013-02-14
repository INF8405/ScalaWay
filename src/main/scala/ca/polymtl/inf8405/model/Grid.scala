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
        Token( red,     Coordinate( 2, 2 ) ),
        Token( red,     Coordinate( 4, 3 ) ),
        Token( blue,    Coordinate( 0, 1 ) ),
        Token( blue,    Coordinate( 0, 6 ) ),
        Token( green,   Coordinate( 0, 5 ) ),
        Token( green,   Coordinate( 5, 5 ) ),
        Token( yellow,  Coordinate( 2, 4 ) ),
        Token( yellow,  Coordinate( 4, 5 ) ),
        Token( orange,  Coordinate( 1, 5 ) ),
        Token( orange,  Coordinate( 4, 4 ) )
      ), Nil, 7 )
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

case class Grid( tokens: List[Token], links: List[Link], size: Int )
{
  import scala.collection.JavaConversions._

  def jtokens: java.util.List[Token] = tokens
  def jlinks: java.util.List[Link] = links

  def link( from: Coordinate, to: Coordinate ): Grid =
  {
    DirectionSet.values.find( from + _ == to ).
      map( link( from, _ ) ).
      getOrElse( this )
  }

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
            
            val conflict = link.subLinkables.find( _.position == newLink.position )

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

          val reducedLink = for ( link <- mappedLinks if !link.isEmpty ) yield link.get

          val newLinks = 
            if( addNewLink ) newLink :: reducedLink.filter( _.subLinks.exists( _.from == fromLinkable ) )
            else reducedLink.filter( _ != fromLinkable )

          this.copy( links = newLinks )
        }
        case None => this
      }
    }
  }

  def coords = List.tabulate(size,size){ case (x,y) => Coordinate(x, y) }

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
  def color: Color
  def subLinkables: List[Linkable]
  def subLinks: List[Link]
}
case class Token( col: Color, coordinate: Coordinate ) extends Linkable
{
  def position = coordinate
  def color = col
  def subLinkables = List(this)
  def subLinks = Nil
  override def toString = col.toString + " " + coordinate.toString
}
case class Link( from: Linkable, direction: Direction ) extends Linkable
{
  def position = from.position + direction
  def color = from.color
  def subLinkables = this :: from.subLinkables
  def subLinks = this :: from.subLinks
  override def toString = direction.toString
}

object DirectionSet
{
  val values = Set(
    Up,
    Down,
    Left,
    Right
  )
}

sealed abstract class Direction
{
  def + ( coordinate: Coordinate ): Coordinate
}
case object Up extends Direction
{
  def + ( coordinate: Coordinate ) = coordinate.copy( y = coordinate.y - 1 )
  override def toString = "^"
}
case object Down extends Direction
{
  def + ( coordinate: Coordinate ) = coordinate.copy( y = coordinate.y + 1 )
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
  override def toString = s"[$x $y]"
}

case class Color( code: Int )
{
  override def toString = code.toString
}


// TODO: Put back in test
/*
 * Starting from a position we can compose ( >> ) and set a new starting position ( <*> )
 */
case class FastGrid( grid: Grid, from: Coordinate )
{
  def >>( direction: Direction ) = FastGrid( grid.link( from, direction ), from + direction )
  def <*>( newFrom: Coordinate ) = copy( from = newFrom )

  override def toString = grid.toString
}