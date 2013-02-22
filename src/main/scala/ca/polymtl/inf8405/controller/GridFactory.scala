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

  object SevenBySeven
  {
    def level1: Grid =
    {
      Grid( Set(
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
      ), Set(), 7 )
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

case class Size( )
case class Level( name: String, grid: Grid )