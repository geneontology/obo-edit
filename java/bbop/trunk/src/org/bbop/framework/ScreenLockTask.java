package org.bbop.framework;

import java.awt.Frame;

import org.bbop.swing.BackgroundEventQueue;
import org.bbop.swing.ScreenLockRunnable;

import org.apache.log4j.*;

public class ScreenLockTask extends ScreenLockRunnable implements GUITask {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ScreenLockTask.class);

	public ScreenLockTask(BackgroundEventQueue queue, Frame frame, boolean modal) {
		super(queue, frame, modal);
	}
}
