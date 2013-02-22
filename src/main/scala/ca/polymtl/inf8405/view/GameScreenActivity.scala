package ca.polymtl.inf8405.view

import android.os.Bundle
import android.content.Context
import android.content.Intent
import android.graphics.Color
import android.view.Gravity
import android.view.Menu
import android.view.View
import android.view.View.OnClickListener
import android.view.ViewGroup.LayoutParams
import android.widget.Button
import android.widget.LinearLayout
import android.widget.PopupWindow

class GameScreenActivity extends CustomWindowTitle
{
  val BACKGROUND_COLOR = Color.BLACK

  private var popup: Option[PopupWindow] = None
  private var sizeMap: Option[Int] = None
  private var level: Option[Int] = None

  override def onCreateOptionsMenu( menu: Menu ) =
  {
    getMenuInflater.inflate( R.menu.gamescreen, menu )
    true
  }

  protected override def onCreate( savedInstanceState: Bundle )
  {
    super.onCreate( savedInstanceState )

    // Get the size message from the intent
    val intent = getIntent()
    val sizeMap_ = intent.getIntExtra( SelectSizeActivity.SIZE_MESSAGE, 7 )
    val level_ = intent.getIntExtra( SelectSizeActivity.LEVEL_MESSAGE, 1 )
    sizeMap = Some( sizeMap_ )
    level = Some( level_ )

    title.foreach( _.setText("Map " + sizeMap_ + "x" + sizeMap_ + " - Level " + level_) )

    // Set the background color for activity - has the same color as canvas background
    getWindow.getDecorView.setBackgroundColor(BACKGROUND_COLOR)

    // Create activity dynamic
    val mainLayout = new LinearLayout( this )
    mainLayout.setOrientation( LinearLayout.VERTICAL )
    mainLayout.setGravity( Gravity.CENTER_VERTICAL )

    val drawView_ = new DrawView( this, sizeMap_, level_ )
    drawView_.setFocusable( true )
    drawView_.setFocusableInTouchMode( true )

    // Test - Create popupwindow
    val popup_ = new PopupWindow( this )
    popup = Some( popup_ )
    val ( width, height ) = getDimention

    val popupWidth = 3 * width / 4
    val popupHeigth = height / 4

    val replayGame = new NewGameListener( this , level_ )
    val dimension = Dimension( popupWidth, popupHeigth )

    if ( level_ < SelectLevelActivity.NUMBER_OF_LEVEL )
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

    val resetButton = new Button(this)
    resetButton.setText("RESET")
    val buttonLayout = new LinearLayout.LayoutParams(
      LayoutParams.WRAP_CONTENT,
      LayoutParams.WRAP_CONTENT
    )
    buttonLayout.gravity = Gravity.CENTER
    resetButton.setLayoutParams( buttonLayout )
    resetButton.setEnabled( true )
    resetButton.setOnClickListener( new OnClickListener() {
      override def onClick( view: View ) {
        popup_.showAtLocation( mainLayout, Gravity.CENTER, 0, 0 )
        popup_.update( 0, 0, dimension.width, dimension.height )
      }
    })

    mainLayout.addView( drawView_ )
    mainLayout.addView( resetButton )
    setContentView( mainLayout )
  }

  class NewGameListener( contextParent: Context, nextLevel: Int ) extends OnClickListener
  {
    override def onClick( v: View )
    {
      val intent = new Intent(contextParent, classOf[GameScreenActivity])
      intent.putExtra(SelectSizeActivity.SIZE_MESSAGE, sizeMap)
      intent.putExtra(SelectSizeActivity.LEVEL_MESSAGE, nextLevel)
      startActivity(intent)
      finishGameScreen()

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

  def finishGameScreen()
  {
    this.finish()
  }
}