package ca.polymtl.inf8405.model

/*
 * 0,0 is the bottom left corner
 *
 * @param width
 * @param height
 */
import UnsafeSet._

object UnsafeSet
{
  implicit def toUnsafeSet( set: Set[Link] ) = new UnsafeSet( set )
}

class UnsafeSet( set: Set[Link] )
{
  def -( other: Linkable ):Set[Link] =
  {
    other match
    {
      case link: Link => set - link
      case token: Token => set.filter( !_.isOrigin( token ) )
    }
  }

  def +( other: Linkable ):Set[Link] =
  {
    other match
    {
      case link: Link => set + link
      case _ => set
    }
  }
}

case class Grid( tokens: Set[Token], links: Set[Link], size: Int )
{
  def colorOf(coord: Coordinate): Color = tokens.groupBy(_.coordinate).get(coord).get.head.col

  def isTokenPosition(coord: Coordinate)= tokens.groupBy(_.coordinate).get(coord) != None

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
        // There is three cases:
        // * A link contains a Token
        // * A token is alone
        // * There is nothing

        val intersectionToken =
          tokens.
            filter( _.position == position ).
            map( t => ( t, t ) )

        if ( intersectionToken.isEmpty )
        {
          val intersectionLinks =
            for {
              link <- links
              subLink <- link.subLinkables if subLink.position == position
            } yield ( link, subLink )
          intersectionLinks.headOption
        }
        else
        {
          intersectionToken.headOption
        }
      }

      ( findLinkableAndIntersection( from ), findLinkableAndIntersection( to ) ) match
      {
        case ( Some( ( fromLink, fromIntersect ) ), Some( ( toLink, toIntersect ) ) ) =>
        {
          if( fromLink == toLink ||         // self breaking link on a link
            fromLink.isOrigin( toLink ) )   // self breaking link on a token
          {
            this.copy( links = links - fromLink + toIntersect )
          }
          else
          {
            val newLink = Link( fromIntersect, direction )
            if ( toIntersect.isEnd( newLink ) )
            {
              // put two links together
              val newLink2 = newLink + toIntersect
              this.copy( links = links - fromLink - toLink + newLink2 )
            }
            else
            {
              // it's a cut
              toIntersect match
              {
                case a: Token => this       // Hit different token > Nothing happens
                case Link( linkable, _ ) =>
                {
                  this.copy( links = ( links - fromLink - toLink + Link( fromIntersect, direction ) ) + linkable )
                }
                case _ => this
              }
            }
          }
        }
        case ( Some( ( fromLink, fromIntersect ) ), None ) =>
        {
          this.copy( links = ( links - fromLink ) + Link( fromIntersect, direction ) )
        }
        case _ => this
      }
    }
  }

  def isFinished = isFull && isAllLinked

  def isFull: Boolean =
  {
    val allLinkables = tokens ++ links.map( _.subLinkables).flatten
    coords.flatten.forall(
      coordinate => ( allLinkables.exists( _.position == coordinate ) )
    )
  }

  def isAllLinked = tubesDone == tokens.groupBy( _.color ).size

  def tubesDone =
  {
    tokens.groupBy( _.color ).count{ case ( _, tokensPair ) => {
      tokensPair.exists( t => links.exists( _.isEnd( t ) ) )
    }}
  }

  def coords = List.tabulate(size,size){ case (x,y) => Coordinate(x, y) }

  private def isInvalidLink( from: Coordinate, to: Direction ): Boolean =
  {
    val toCoord = from + to
    val bounds = 0 until size

    ! ( bounds.contains( toCoord.x ) &&
        bounds.contains( toCoord.y ) )
  }
}


trait Linkable
{
  def position: Coordinate
  def color: Color
  def subLinkables: List[Linkable]
  def subLinks: List[Link]
  def isEnd( linkable: Linkable ): Boolean
  def isOrigin( linkable: Linkable ): Boolean

  def +( other: Linkable ): Link
}
case class Token( col: Color, coordinate: Coordinate ) extends Linkable
{
  def position = coordinate
  def color = col
  def subLinkables = List(this)
  def subLinks = Nil
  def isEnd( linkable: Linkable ) = linkable.isEnd( this )
  def isEnd( token: Token ) = false
  def isOrigin( linkable: Linkable ) = this == linkable

  def +( other: Linkable ) = other + this

  override def toString = col.toString + " " + coordinate.toString
}
case class Link( from: Linkable, direction: Direction ) extends Linkable
{
  def position = from.position + direction
  def color = from.color
  def subLinkables = this :: from.subLinkables
  def subLinks = this :: from.subLinks

  def isEnd( linkable: Linkable  ) =
  {
    !isOrigin( linkable ) &&
    linkable.color == color &&
    linkable.position == position
  }

  def isOrigin( linkable: Linkable ) = from.isOrigin( linkable )

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
  def >>( direction: Direction ) = {
    val newGrid = grid.link( from, direction )
    val newPosition = if (newGrid == grid) from else from + direction
    FastGrid( newGrid, newPosition )
  }
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