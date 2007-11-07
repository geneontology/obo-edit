package org.bbop.framework;

import java.awt.Frame;

import org.bbop.swing.BackgroundEventQueue;
import org.bbop.swing.ScreenLockRunnable;

public class ScreenLockTask extends ScreenLockRunnable implements GUITask {

	public ScreenLockTask(BackgroundEventQueue queue, Frame frame, boolean modal) {
		super(queue, frame, modal);
	}
}
