package ca.polymtl.inf8405
package controller

import model.{Coordinate, Grid}

sealed trait GridEvent
case object Complete extends GridEvent
case class LinkChanged( amount: Int ) extends GridEvent

object GridState
{
  def apply( grid: Grid ): GridState = GridState( grid.isFull, grid.isAllLinked, grid.tubesDone )
  def empty = GridState( false, false, 0 )
}
case class GridState( full: Boolean, allLinked: Boolean, tubesDone: Int )
{
  def delta( previous: GridState ): List[GridEvent] =
  {
    val eventComplete =
      if ( allLinked  & full) Some( Complete )
      else None

    val eventLinks =
      if ( this.tubesDone != previous.tubesDone ) Some( LinkChanged( this.tubesDone ) )
      else None

    List( eventComplete, eventLinks ).foldRight( List.empty[GridEvent] ){
      case ( Some( e ), acc ) => e :: acc
      case ( _, acc ) => acc
    }
  }
}

trait GridListener
{
  def apply( event: GridEvent )
}

case class GridObserver( var grid: Grid, listener: GridListener, var state: GridState = GridState.empty)
{
  def cleanupGrid(from: Coordinate)=
  {
    grid = grid.cleanupLink(from)
  }

  def link( last: Coordinate, current: Coordinate )
  {
    grid = grid.link( last, current )
    val newState = GridState( grid )
    newState.delta( state ).foreach( listener.apply _ )
  }

}