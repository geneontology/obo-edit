package org.oboedit.gui.tasks;

import java.awt.Frame;

import org.bbop.framework.GUITask;
import org.bbop.swing.BackgroundEventQueue;
import org.bbop.swing.ScreenLockRunnable;

public class ScreenLockTask extends ScreenLockRunnable implements GUITask {

	public ScreenLockTask(BackgroundEventQueue queue, Frame frame, boolean modal) {
		super(queue, frame, modal);
	}
}
