package ca.polymtl.inf8405;

import android.app.Activity;
import android.os.Bundle;

import ca.polymtl.inf8405.model.*;

public class MainActivity extends Activity
{
    @Override
    protected void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);

        Grid grid = GridFactory.heightByHeight().level1();

        for( Link link : grid.jlinks() )
        {

        }
    }
}
