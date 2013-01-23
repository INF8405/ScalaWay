package ca.polymtl.inf8405
package controller

import view.GameView

import android.view.View.OnTouchListener
import android.view.{MotionEvent, View}


class GameController( gameView: GameView ) extends OnTouchListener
{
  def onTouch( view: View, event: MotionEvent ): Boolean =
  {
    false
  }
}
