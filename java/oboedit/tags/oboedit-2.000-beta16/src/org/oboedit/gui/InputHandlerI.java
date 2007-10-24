package org.oboedit.gui;

import java.awt.Point;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.event.*;

import javax.swing.JComponent;
import javax.swing.KeyStroke;

import org.bbop.swing.KeyRecorder;

public interface InputHandlerI {

	public static int REJECT_DROP = 0;

	public static int ALMOST_ACCEPT_DROP = 1;

	public static int ACCEPT_DROP = 2;

	public int allowDrop(JComponent dropPanel, Object o, GestureTarget dest,
			Point p, KeyRecorder.KeyChecker keyChecker);

	public boolean drop(JComponent dropPanel, Object o, GestureTarget dest,
			Point p, KeyRecorder.KeyChecker keyChecker);

	public boolean click(JComponent panel,
			GestureTarget dest, MouseEvent e, KeyRecorder.KeyChecker keyChecker);

	public boolean press(JComponent panel, KeyEvent e,
			KeyRecorder.KeyChecker keyChecker);
	
	public String getName();
	
	public KeyStroke getShortcut();
	
	public String getDragDesc();
	
	public String getID();
}
