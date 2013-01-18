package ca.polymtl.inf8405

import _root_.model.Cat

import android._
import app.Activity
import os.Bundle
import view.View
import view.View.OnClickListener

class MainActivity extends Activity with TypedActivity
{
  override def onCreate(bundle: Bundle)
  {
    // install jdk 1.6 ( yep Java 6 because of ProGuard )
    // monitor
    // android:start-device
    // attach remote debugger port 8600
    // uncomment next two lines

    // import android.os.Debug
    // Debug.waitForDebugger()

    super.onCreate(bundle)
    setContentView(R.layout.main)

    findView(TR.button).setOnClickListener( new OnClickListener         // TR is like R but with type information
    {                                                                   // otherwise this would be like
      def onClick(p1: View)                                             //
      {                                                                 // findViewById(R.id.button_send).asInstanceOf[Button].
        val cat = Cat("bob")                                            //  setOnClickListener(new View.OnClickListener() {
        findView(TR.textview).setText( cat.pur )
      }
    })
  }
}
