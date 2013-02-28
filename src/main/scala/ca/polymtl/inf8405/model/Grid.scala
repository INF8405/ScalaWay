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

/**
 * A set which define add/remove operations our specific context
 * @param set
 */
class UnsafeSet( set: Set[Link] )
{
  def -( other: Linkable ):Set[Link] =
  {
    other match
    {
      case link: Link => set - link // If link, normal remove
      case token: Token => set.filter( !_.isOrigin( token ) ) // if token, remove the link which has this token as origin
    }
  }

  def +( other: Linkable ):Set[Link] =
  {
    other match
    {
      case link: Link => set + link // If link, normal add
      case _ => set // If not link, dont add
    }
  }
}

case class Grid( tokens: Set[Token], links: Set[Link], size: Int )
{
  // Create a new link from 2 coordinates
  def link( from: Coordinate, to: Coordinate ): Grid =
  {
    DirectionSet.values.find( from + _ == to ).
      map( link( from, _ ) ).
      getOrElse( this )
  }

  // Perform cleanup when user clicks on "from" coordinate
  def cleanupLink(from: Coordinate):Grid= {
    getTokenFromCoordinate(from) match
    {
      case Some( token: Token ) =>
        // If token, remove all links starting from and going to this position
        val newLinks = links.filter( l => l.position != from && !l.subLinkables.contains(token) )
        Grid(tokens, newLinks.toSet, size)
      case None =>
        getLinkFromCoordinate( from ) match
        {
          case Some(link: Link)=>
            // Remove all following links from this position
            val newLinks = links - links.find( l => l.subLinks.contains(link)).get + link
            Grid(tokens, newLinks.toSet, size)
          case None =>
            this
        }
    }
  }

  def getLinkFromCoordinate(coord: Coordinate) =
  {
    links.map( _.subLinks ).flatten.find( t => t.position == coord )
  }

  def getTokenFromCoordinate(coord: Coordinate) =
  {
    tokens.find( t => t.position == coord )
  }

  def allLinkables = tokens ++ links.map( _.subLinks ).flatten

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
            find( _.position == position ).
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
          intersectionToken
        }
      }

      ( findLinkableAndIntersection( from ), findLinkableAndIntersection( to ) ) match
      {
        // we linking into something
        case ( Some( ( fromLink, fromIntersect ) ), Some( ( toLink, toIntersect ) ) ) =>
        {
          // something is our own link
          if( fromLink == toLink ||         // self breaking link on a link
            fromLink.isOrigin( toLink ) )   // self breaking link on a token
          {
            this.copy( links = links - fromLink + toIntersect )
          }
          else // something is another link
          {
            val newLink = Link( fromIntersect, direction )
            if ( toIntersect.isEnd( newLink ) ) // other link has the same color as our
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
        // We linking into empty position
        case ( Some( ( fromLink, fromIntersect ) ), None ) =>
        {
          this.copy( links = ( links - fromLink ) + Link( fromIntersect, direction ) )
        }
        // Nothing to link from
        case _ => this
      }
    }
  }

  def isFinished = isFull && isAllLinked

  // Every position in the grid contains at least a token/link
  def isFull: Boolean =
  {
    allLinkables.map( _.position ).toSet.size == size*size
  }

  // All tokens pairs are linked between each other
  def isAllLinked = tubesDone == tokens.groupBy( _.color ).size

  // Return the number of token pairs which have been linked
  def tubesDone =
  {
    tokens.groupBy( _.color ).count{ case ( _, tokensPair ) => {
      tokensPair.exists( t => links.exists( _.isEnd( t ) ) )
    }}
  }

  def allCoords = List.tabulate(size,size){ case (x,y) => Coordinate(x, y) }

  // If direction point to outside of grid
  private def isInvalidLink( from: Coordinate, to: Direction ): Boolean =
  {
    val toCoord = from + to
    val bounds = 0 until size

    ! ( bounds.contains( toCoord.x ) &&
        bounds.contains( toCoord.y ) )
  }
}

// Can link from this object
trait Linkable
{
  def position: Coordinate
  def color: Color
  // All childrens of this linkable object. If token, itself. If Link, itself + sublinks + originToken
  def subLinkables: List[Linkable]
  // All childrens which are link (not token)
  def subLinks: List[Link]
  // Do these linkables create a link between a token pair ?
  def isEnd( linkable: Linkable ): Boolean
  // If this token is the token from which we start the link
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

/*
 * Starting from a position we can compose ( >> ) and set a new starting position ( <*> )
 * This structure allow to avoid to repeating the "from" position by some chaining callls
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