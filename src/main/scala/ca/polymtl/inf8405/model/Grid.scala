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
      val to = from + direction

      /**
       * Iterates links and maybe find the full segment and the intersection at a given position
       * @param position
       * @return ( full segment, intersection )
       */
      def findLinkableAndIntersection( position: Coordinate ): Option[( Linkable, Linkable )] =
      {
        val intersections =
        for {
          linkable <- allLinkables
          subLink <- linkable.subLinkables if subLink.position == position
        } yield ( linkable, subLink )

        intersections.headOption
      }

      ( findLinkableAndIntersection( from ), findLinkableAndIntersection( to ) ) match
      {
        case ( Some( ( fromLink, fromIntersect ) ), Some( ( toLink, toIntersect ) ) ) =>
        {
          if ( fromIntersect.isEnd( toIntersect ) )
          {
            // put two links together
            val newLink = Link( fromIntersect, direction ) + toIntersect
            this.copy( links = newLink :: links.filter( l => !(l == fromLink || l == toLink ) ) )
          }
          else
          {
            this
          }
        }
        case ( Some( ( fromLink, fromIntersect ) ), None ) =>
        {
          this.copy( links = Link( fromIntersect, direction ) :: links.filter( _ != fromLink ) )
        }
        case _ => this
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
  def isEnd( linkable: Linkable ): Boolean
  def isEnd( token: Token ): Boolean

  def +( other: Linkable ): Link
}
case class Token( col: Color, coordinate: Coordinate ) extends Linkable
{
  def position = coordinate
  def color = col
  def subLinkables = List(this)
  def subLinks = Nil
  def isEnd( linkable: Linkable ) = linkable.isEnd( this )
  def isEnd( token: Token ) = this != token && token.color == col

  def +( other: Linkable ) = other + this

  override def toString = col.toString + " " + coordinate.toString
}
case class Link( from: Linkable, direction: Direction ) extends Linkable
{
  def position = from.position + direction
  def color = from.color
  def subLinkables = this :: from.subLinkables
  def subLinks = this :: from.subLinks
  def isEnd( linkable: Linkable ) = from.isEnd( linkable )
  def isEnd( token: Token ) = from.isEnd( token )
  override def toString = from.toString + " " + direction.toString

  def +( other: Linkable ) =
  {
    other match
    {
      case a: Token => this
      case Link( f, d ) => Link( this, d.neg ) + f
    }
  }
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
  def neg : Direction
}
case object Up extends Direction
{
  def neg = Down
  def + ( coordinate: Coordinate ) = coordinate.copy( y = coordinate.y - 1 )
  override def toString = "^"
}
case object Down extends Direction
{
  def neg = Up
  def + ( coordinate: Coordinate ) = coordinate.copy( y = coordinate.y + 1 )
  override def toString = "v"
}
case object Left extends Direction
{
  def neg = Right
  def + ( coordinate: Coordinate ) = coordinate.copy( x = coordinate.x - 1 )
  override def toString = "<"
}
case object Right extends Direction
{
  def neg = Left
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

  override def equals( other: Any ) =
  {
    if( other == null ) false
    else
    {
      other match
      {
        case FastGrid( otherGrid, _ ) => grid == otherGrid
        case _ => false
      }
    }
  }
  override def toString = grid.toString
}