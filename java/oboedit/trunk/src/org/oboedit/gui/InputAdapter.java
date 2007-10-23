package org.oboedit.gui;

import java.awt.Point;
import java.awt.event.*;

import javax.swing.JComponent;

import org.bbop.swing.KeyRecorder.KeyChecker;

public abstract class InputAdapter implements InputHandlerI {

	public int allowDrop(JComponent dropPanel, Object o, GestureTarget dest, Point p, KeyChecker keyChecker) {
		return InputHandlerI.REJECT_DROP;
	}

	public boolean click(JComponent panel, GestureTarget dest, MouseEvent e, KeyChecker keyChecker) {
		return false;
	}

	public boolean drop(JComponent dropPanel, Object o, GestureTarget dest, Point p, KeyChecker keyChecker) {
		return false;
	}

	public boolean press(JComponent panel, KeyEvent e, KeyChecker keyChecker) {
		return false;
	}

	public String getDragDesc() {
		return "";
	}
	
}
