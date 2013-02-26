package ca.polymtl.inf8405
package view

import android.app.Activity
import android.os.Bundle
import android.view.{View, Gravity, Menu}
import android.view.View.OnClickListener
import android.widget.{Button, LinearLayout}
import android.content.Intent

import controller._

object SelectLevelActivity
{
  val LEVEL_MESSAGE = "LevelMessage"
}

class SelectLevelActivity
  extends Activity
  with TypedActivity
  with ActivityScreenSize
  with ButtonLayout
{ self =>

  override def onCreateOptionsMenu( menu: Menu ) =
  {
    getMenuInflater.inflate( R.menu.select_level, menu )
    true
  }

  protected override def onCreate( savedInstanceState: Bundle )
  {
    super.onCreate( savedInstanceState )
    setContentView( R.layout.select_level )

    val NOT_SET = -1
    val intent = getIntent
    val size_ = intent.getIntExtra( SelectSizeActivity.SIZE_MESSAGE, NOT_SET )
    val size = if ( size_ == NOT_SET ) None else Some( size_ )

    for { s <- size }
    {
      val mainLayout = new LinearLayout( this )
      mainLayout.setOrientation( LinearLayout.VERTICAL )
      mainLayout.setGravity( Gravity.CENTER_VERTICAL )
      mainLayout.setBackgroundColor( GameScreenActivity.BACKGROUND_COLOR )

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
        mainLayout.addView( button )
      }}

      setContentView( mainLayout )
    }
  }
}