package ca.polymtl.inf8405.view

import android.content.Context
import android.graphics.Color
import android.view.Gravity
import android.view.View.OnClickListener
import android.view.ViewGroup.LayoutParams
import android.widget.{LinearLayout, TextView,Button}

case class ButtonH( text: String, listener: OnClickListener )
case class Dimension( width: Int, height: Int )

class TransparentPanelLayout(
  context: Context,
  header: String,
  text: String,
  buttons: List[ButtonH],
  dimension: Dimension ) extends LinearLayout( context )
{
  val BACKGROUND_PANEL_COLOR = Color.BLACK
  val TEXT_PANEL_COLOR = Color.WHITE

  def init()
  {
    setOrientation( LinearLayout.VERTICAL )
    setGravity( Gravity.CENTER_HORIZONTAL )
    setAlpha( 0.7f )
    setBackgroundColor( BACKGROUND_PANEL_COLOR )

    // == Header ==
    val headerView = new TextView( context )
    headerView.setTextSize( dimension.height / 8 )
    headerView.setTextColor( TEXT_PANEL_COLOR )
    headerView.setGravity( Gravity.CENTER )
    headerView.setLayoutParams( new LinearLayout.LayoutParams(
        LayoutParams.WRAP_CONTENT,
        LayoutParams.WRAP_CONTENT
    ))
    headerView.setText( header )
    addView( headerView )

    // == Text ==
    val textView = new TextView( context )
    textView.setTextSize( dimension.height / 12 )
    textView.setTextColor( TEXT_PANEL_COLOR )
    textView.setGravity( Gravity.CENTER )
    textView.setLayoutParams( new LinearLayout.LayoutParams(
      LayoutParams.WRAP_CONTENT,
      dimension.height / 6
    ))
    textView.setText( text )
    addView( textView )

    // == Buttons ==
    val buttonLayout = new LinearLayout.LayoutParams(
      LayoutParams.MATCH_PARENT,
      LayoutParams.WRAP_CONTENT
    )
    buttonLayout.gravity = Gravity.CENTER

    buttons.map{ case ButtonH( buttonText, listener ) => {

      val button = new Button( context )
      button.setLayoutParams( buttonLayout )
      button.setOnClickListener( listener )
      button.setTextColor( TEXT_PANEL_COLOR )
      button.setText( buttonText )
      button.setTextSize(dimension.height / 12 )
      button
    }}.foreach( addView _ )
  }
}



