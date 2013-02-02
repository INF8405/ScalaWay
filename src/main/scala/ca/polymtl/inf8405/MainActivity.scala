package ca.polymtl.inf8405

// us
import controller.GameController

// android
import android.app.Activity
import android.os.Bundle

class MainActivity extends Activity with TypedActivity
{
  override def onCreate(bundle: Bundle)
  {
    // install jdk 1.6 ( yep Java 6 because of ProGuard )
    // start android-sdk/bin/monitor ( android device monitor )
    // android:start-device
    // attach remote debugger port 8600
    // uncomment next two lines

    // import android.os.Debug
    // Debug.waitForDebugger()

    super.onCreate(bundle)
    setContentView( TR.layout.main.id )

    val gameview = findView( TR.gameview )
    gameview.setOnTouchListener( new GameController( gameview ) )
  }
}
