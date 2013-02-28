package ca.polymtl.inf8405
package view

import android.app.Activity
import android.os.Bundle
import android.view.{View, Gravity, Menu}
import android.view.View.OnClickListener
import android.widget.{Button, LinearLayout}
import android.content.{Context, Intent}

import controller._

object SelectLevelActivity
{
  val LEVEL_MESSAGE = "LevelMessage"
  val PREFS_NAME = "UserData"
}

class SelectLevelActivity
  extends CustomWindowTitle
  with TypedActivity
  with ActivityScreenSize
  with ButtonLayout
{ self =>

  var size: Option[Int] = None
  var mainLayout: Option[LinearLayout] = None

  protected override def onResume()
  {
    super.onResume()
    // Update the button status when user re-opens the windows
    val lastLevel = getSharedPreferences(SelectLevelActivity.PREFS_NAME, Context.MODE_PRIVATE).getInt(size.get.toString, 1)
    for ( i <- 0 until mainLayout.get.getChildCount)
    {
      val button = mainLayout.get.getChildAt(i)
      button.setEnabled(lastLevel > i)
    }
  }

  protected override def onCreate( savedInstanceState: Bundle )
  {
    super.onCreate( savedInstanceState )

    val NOT_SET = -1
    val intent = getIntent
    val size_ = intent.getIntExtra( SelectSizeActivity.SIZE_MESSAGE, NOT_SET )
    size = if ( size_ == NOT_SET ) None else Some( size_ )

    title.foreach( _.setText("Select Level" ) )
    // Dynamically create a layout for this activity based on the number of levels
    for { s <- size }
    {
      mainLayout = Some(new LinearLayout( this ))
      mainLayout.get.setOrientation( LinearLayout.VERTICAL )
      mainLayout.get.setGravity( Gravity.CENTER_VERTICAL )
      mainLayout.get.setBackgroundColor( GameScreenActivity.BACKGROUND_COLOR )

      GridFactory.grids.filter( _.grid.size == size_ ).foreach{ case Level( level, _ ) => {
        val button = new Button( this )
        button.setText( s"level $level" )
        button.setLayoutParams( buttonLayout )
        button.setOnClickListener( new OnClickListener {
          def onClick(p1: View)
          {
            val newLevelIntent = new Intent( self, classOf[GameScreenActivity] )
            newLevelIntent.putExtra( SelectSizeActivity.SIZE_MESSAGE, s )
            newLevelIntent.putExtra( SelectLevelActivity.LEVEL_MESSAGE, level )
            startActivity( newLevelIntent )
          }
        })
        mainLayout.get.addView( button )
      }}

      setContentView( mainLayout.get )
    }
  }
}