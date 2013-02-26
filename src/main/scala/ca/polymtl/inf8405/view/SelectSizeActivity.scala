package ca.polymtl.inf8405
package view

import controller.GridFactory

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.{Gravity, View, Menu}
import android.view.ViewGroup.LayoutParams
import android.view.View.OnClickListener
import android.widget.{RelativeLayout, LinearLayout, Button, TextView}

object SelectSizeActivity
{
  val SIZE_MESSAGE = "SizeMessage"
}

class SelectSizeActivity extends Activity
  with TypedActivity
  with ActivityScreenSize
  with ButtonLayout
{ self =>

  protected override def onCreate( savedInstanceState: Bundle )
  {
    super.onCreate( savedInstanceState )

    val  mainLayout = new LinearLayout( this )
    mainLayout.setOrientation( LinearLayout.VERTICAL )
    mainLayout.setGravity( Gravity.CENTER_VERTICAL )

    val label = new TextView( this )
    label.setText( "Size" )
    mainLayout.addView( label )

    GridFactory.grids.groupBy( _.grid.size ).toSeq.sortBy(_._1).map{ case ( size, _ ) => {
      val button = new Button( this )
      button.setText( s"$size x $size" )
      button.setLayoutParams( buttonLayout )
      button.setOnClickListener( new OnClickListener()
      {
        override def onClick( v: View )
        {
          val intent = new Intent( self, classOf[SelectLevelActivity] )
          intent.putExtra( SelectSizeActivity.SIZE_MESSAGE, size )
          startActivity(intent)
        }
      })

      button
    }}.foreach( mainLayout.addView _ )

    setContentView( mainLayout )
  }
}