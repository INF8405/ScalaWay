package ca.polymtl.inf8405
package controller

import model._

object GridFactory
{
  val red = Color(1)
  val blue = Color(2)
  val green = Color(3)
  val yellow = Color(4)
  val orange = Color(5)
  val cyan = Color(6)
  val brown = Color(7)
  val purple = Color(8)
  val lightGreen = Color(9)
  val redWine = Color(10)

  def getGrid(size: Int, level: Int) =
    grids.
      find( l => level == l.level && size == l.grid.size ).
      map( _.grid )



  def numberOfLevels( size: Int ) =
    grids.count( _.grid.size == size )

  val grids = List(
    Level( 1, Grid( Set(
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
    ), Set(), 7 )),

    Level( 2, Grid( Set(
      Token( red,     Coordinate( 6, 6 ) ),
      Token( red,     Coordinate( 6, 4 ) ),
      Token( blue,    Coordinate( 6, 3 ) ),
      Token( blue,    Coordinate( 0, 5 ) ),
      Token( green,   Coordinate( 5, 6 ) ),
      Token( green,   Coordinate( 5, 4 ) ),
      Token( yellow,  Coordinate( 6, 2 ) ),
      Token( yellow,  Coordinate( 1, 5 ) ),
      Token( orange,  Coordinate( 3, 5 ) ),
      Token( orange,  Coordinate( 5, 2 ) ),
      Token( cyan,    Coordinate( 1, 1 ) ),
      Token( cyan,    Coordinate( 2, 5 ) ),
      Token( brown,   Coordinate( 2, 2 ) ),
      Token( brown,   Coordinate( 5, 1 ) )
    ), Set(), 7 ) ),

    Level( 3, Grid( Set(
      Token( red,     Coordinate( 6, 6 ) ),
      Token( red,     Coordinate( 3, 5 ) ),
      Token( blue,    Coordinate( 0, 5 ) ),
      Token( blue,    Coordinate( 3, 4 ) ),
      Token( green,   Coordinate( 1, 3 ) ),
      Token( green,   Coordinate( 4, 4 ) ),
      Token( yellow,  Coordinate( 2, 2 ) ),
      Token( yellow,  Coordinate( 4, 2 ) ),
      Token( orange,  Coordinate( 1, 5 ) ),
      Token( orange,  Coordinate( 4, 5 ) ),
      Token( cyan,    Coordinate( 1, 2 ) ),
      Token( cyan,    Coordinate( 5, 4 ) )
    ), Set(), 7 ) ),

    Level( 1, Grid( Set(
      Token( red,         Coordinate( 4, 0 ) ),
      Token( red,         Coordinate( 4, 5 ) ),
      Token( blue,        Coordinate( 5, 1 ) ),
      Token( blue,        Coordinate( 7, 1 ) ),
      Token( green,       Coordinate( 0, 1 ) ),
      Token( green,       Coordinate( 2, 2 ) ),
      Token( yellow,      Coordinate( 0, 3 ) ),
      Token( yellow,      Coordinate( 6, 3 ) ),
      Token( orange,      Coordinate( 3, 4 ) ),
      Token( orange,      Coordinate( 2, 5 ) ),
      Token( cyan,        Coordinate( 7, 2 ) ),
      Token( cyan,        Coordinate( 7, 7 ) ),
      Token( brown,       Coordinate( 5, 2 ) ),
      Token( brown,       Coordinate( 6, 1 ) ),
      Token( lightGreen,  Coordinate( 2, 4 ) ),
      Token( lightGreen,  Coordinate( 5, 3 ) ),
      Token( redWine,     Coordinate( 0, 0 ) ),
      Token( redWine,     Coordinate( 0, 2 ) )
    ), Set(), 8 ) ),

    Level( 2, Grid( Set(
      Token( red,         Coordinate( 1, 6 ) ),
      Token( red,         Coordinate( 3, 4 ) ),
      Token( blue,        Coordinate( 2, 6 ) ),
      Token( blue,        Coordinate( 5, 5 ) ),
      Token( green,       Coordinate( 6, 1 ) ),
      Token( green,       Coordinate( 6, 3 ) ),
      Token( yellow,      Coordinate( 5, 0 ) ),
      Token( yellow,      Coordinate( 5, 3 ) ),
      Token( orange,      Coordinate( 3, 6 ) ),
      Token( orange,      Coordinate( 4, 1 ) ),
      Token( cyan,        Coordinate( 4, 0 ) ),
      Token( cyan,        Coordinate( 6, 0 ) ),
      Token( lightGreen,  Coordinate( 2, 2 ) ),
      Token( lightGreen,  Coordinate( 2, 4 ) )
    ), Set(), 8 ) ),

    Level( 3, Grid( Set(
      Token( red,         Coordinate( 2, 5 ) ),
      Token( red,         Coordinate( 4, 4 ) ),
      Token( blue,        Coordinate( 1, 1 ) ),
      Token( blue,        Coordinate( 2, 6 ) ),
      Token( green,       Coordinate( 0, 3 ) ),
      Token( green,       Coordinate( 3, 0 ) ),
      Token( yellow,      Coordinate( 5, 1 ) ),
      Token( yellow,      Coordinate( 3, 5 ) ),
      Token( orange,      Coordinate( 3, 1 ) ),
      Token( orange,      Coordinate( 4, 3 ) ),
      Token( cyan,        Coordinate( 0, 4 ) ),
      Token( cyan,        Coordinate( 4, 1 ) ),
      Token( lightGreen,  Coordinate( 5, 2 ) ),
      Token( lightGreen,  Coordinate( 4, 5 ) ),
      Token( redWine,     Coordinate( 2, 1 ) ),
      Token( redWine,     Coordinate( 3, 3 ) )
    ), Set(), 8 ))
  )
}

case class Level( level: Int, grid: Grid )