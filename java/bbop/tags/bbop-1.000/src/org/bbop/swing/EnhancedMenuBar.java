package org.bbop.swing;

import java.awt.Component;
import java.awt.PopupMenu;

import javax.swing.Box;
import javax.swing.JMenu;
import javax.swing.JMenuBar;

public class EnhancedMenuBar extends JMenuBar {

	protected JMenu helpMenu;

	protected Component spacer;

	public EnhancedMenuBar() {
		spacer = Box.createHorizontalGlue();
	}

	@Override
	public void setHelpMenu(JMenu menu) {
		helpMenu = menu;
		rearrange();
	}

	@Override
	public JMenu getHelpMenu() {
		return helpMenu;
	}

	@Override
	public Component add(Component comp) {
		Component out = super.add(comp);
		rearrange();
		return out;
	}

	protected void rearrange() {
		remove(spacer);
		if (helpMenu != null)
			remove(helpMenu);
		super.add(spacer);
		if (helpMenu != null)
			super.add(helpMenu);
	}

	@Override
	public Component add(Component comp, int index) {
		Component out = super.add(comp, index);
		rearrange();
		return out;
	}

	@Override
	public void add(Component comp, Object constraints) {
		super.add(comp, constraints);
		rearrange();
	}

	@Override
	public void add(Component comp, Object constraints, int index) {
		super.add(comp, constraints, index);
		rearrange();
	}

	@Override
	public JMenu add(JMenu c) {
		JMenu out = super.add(c);
		rearrange();
		return out;
	}

	@Override
	public synchronized void add(PopupMenu popup) {
		super.add(popup);
		rearrange();
	}

	@Override
	public Component add(String name, Component comp) {
		Component out = super.add(name, comp);
		rearrange();
		return out;
	}

}
