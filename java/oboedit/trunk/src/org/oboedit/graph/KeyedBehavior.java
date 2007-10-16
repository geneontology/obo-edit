package org.oboedit.graph;

import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import org.oboedit.gui.components.LinkDatabaseCanvas;

public abstract class KeyedBehavior implements ViewBehavior {

	protected int keyCode = KeyEvent.VK_UNDEFINED;

	protected int modifiers = 0;
	
	protected LinkDatabaseCanvas canvas;

	protected KeyListener keyListener = new KeyAdapter() {
		@Override
		public void keyPressed(KeyEvent e) {
			if ((modifiers == 0 || ((e.getModifiers() & modifiers) != 0))
					&& keyCode == e.getKeyCode())
				action();
		}
	};

	public void install(LinkDatabaseCanvas canvas) {
		this.canvas = canvas;
		canvas.addKeyListener(keyListener);
	}

	public void uninstall(LinkDatabaseCanvas canvas) {
		canvas.removeKeyListener(keyListener);
		this.canvas = null;
	}

	protected abstract void action();

	public int getKeyCode() {
		return keyCode;
	}

	public void setKeyCode(int keyCode) {
		this.keyCode = keyCode;
	}

	public int getModifiers() {
		return modifiers;
	}

	public void setModifiers(int modifiers) {
		this.modifiers = modifiers;
	}

}
