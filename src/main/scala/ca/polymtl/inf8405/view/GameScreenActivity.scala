package ca.polymtl.inf8405
package view

import android.os.Bundle
import android.content.Context
import android.content.Intent
import android.graphics.Color
import android.view.Gravity
import android.view.View
import android.view.View.OnClickListener
import android.view.ViewGroup.LayoutParams
import android.widget.Button
import android.widget.LinearLayout
import android.widget.PopupWindow

import controller.GridFactory

object GameScreenActivity
{
  val BACKGROUND_COLOR = Color.BLACK
}

class GameScreenActivity extends CustomWindowTitle with ActivityScreenSize
{ self =>
  import GameScreenActivity.BACKGROUND_COLOR

  private var popup: Option[PopupWindow] = None
  private var sizeMap: Option[Int] = None
  private var level: Option[Int] = None

  protected override def onCreate( savedInstanceState: Bundle )
  {
    super.onCreate( savedInstanceState )

    // Get the size message from the intent
    val intent = getIntent()
    val sizeMap_ = intent.getIntExtra( SelectSizeActivity.SIZE_MESSAGE, 7 )
    val level_ = intent.getIntExtra( SelectLevelActivity.LEVEL_MESSAGE, 1 )
    sizeMap = Some( sizeMap_ )
    level = Some( level_ )

    title.foreach( _.setText("Map " + sizeMap_ + "x" + sizeMap_ + " - Level " + level_) )

    // Set the background color for activity - has the same color as canvas background
    getWindow.getDecorView.setBackgroundColor(BACKGROUND_COLOR)

    // Create popupwindow
    val popup_ = new PopupWindow( this )
    popup = Some( popup_ )
    val dim = screenSize
    val ( width, height ) = ( dim.height, dim.width )

    val popupWidth = 3 * width / 4
    val popupHeigth = height / 4

    val replayGame = new NewGameListener( this , level_ )
    val dimension = Dimension( popupWidth, popupHeigth )

    if ( level_ < GridFactory.numberOfLevels( sizeMap_ ) )
    {
      val nextGame = new NewGameListener(this, level_ + 1)
      val transparentLayout = new TransparentPanelLayout(
        this,
        "Level Complete!",
        s"You completed the level $level_",
        List(
          ButtonH( "Next level", nextGame ),
          ButtonH( "Replay", replayGame )
        ),
        dimension
      )
      transparentLayout.init()
      popup_.setContentView( transparentLayout )
    }
    else {
      val cancelListener = new CancelListener()
      val transparentLayout = new TransparentPanelLayout(
        this,
        "Congrats!",
        s"You completed the last level of size ${sizeMap_}x${sizeMap_}",
        List(
          ButtonH( "Replay", replayGame ),
          ButtonH( "Cancel", cancelListener )
        ),
        dimension
      )
      transparentLayout.init()
      popup_.setContentView( transparentLayout )
    }

    // Create activity dynamic
    val mainLayout = new LinearLayout( this )
    mainLayout.setOrientation( LinearLayout.VERTICAL )
    mainLayout.setGravity( Gravity.CENTER_VERTICAL )

    val drawView_ = new DrawView( this, screenSize, sizeMap_, level_, popup_, mainLayout )
    drawView_.setFocusable( true )
    drawView_.setFocusableInTouchMode( true )

    val resetButton = new Button(this)
    resetButton.setText("RESET")
    val buttonLayout = new LinearLayout.LayoutParams(
      LayoutParams.WRAP_CONTENT,
      LayoutParams.WRAP_CONTENT
    )
    buttonLayout.gravity = Gravity.CENTER
    resetButton.setLayoutParams( buttonLayout )
    resetButton.setEnabled( true )
    resetButton.setOnClickListener( new NewGameListener(this, level_))

    mainLayout.addView( drawView_ )
    mainLayout.addView( resetButton )
    setContentView( mainLayout )
  }

  class NewGameListener( contextParent: Context, nextLevel: Int ) extends OnClickListener
  {
    override def onClick( v: View )
    {
      val editor = getSharedPreferences(SelectLevelActivity.PREFS_NAME, Context.MODE_PRIVATE).edit()
      editor.putInt(sizeMap.get.toString, nextLevel)
      editor.commit()

      val intent = new Intent(contextParent, classOf[GameScreenActivity])
      sizeMap.foreach( intent.putExtra( SelectSizeActivity.SIZE_MESSAGE, _ ) )
      intent.putExtra(SelectLevelActivity.LEVEL_MESSAGE, nextLevel)
      startActivity(intent)

      self.finish()
      popup.foreach( _.dismiss() )
    }
  }

  class CancelListener extends OnClickListener
  {
    override def onClick( v: View )
    {
      popup.foreach( _.dismiss() )
    }
  }

}