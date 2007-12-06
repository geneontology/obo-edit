package org.bbop.swing;

import java.awt.Component;
import java.awt.MenuComponent;
import java.awt.PopupMenu;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import javax.swing.Action;
import javax.swing.JMenuItem;
import javax.swing.JSeparator;

public class DynamicMenu extends AbstractDynamicMenuItem {

	protected List<Component> items = new LinkedList<Component>();

	public DynamicMenu(String s, boolean merge, boolean bracketTop,
			boolean bracketBottom) {
		super(s, merge, bracketTop, bracketBottom);
	}

	public DynamicMenu(String s) {
		super(s, false, false, false);
	}

	public List<Component> getItems() {
		return items;
	}

	public List<Component> getDynamicItems() {
		return null;
	}

	@Override
	public void addSeparator() {
		items.add(new JSeparator());
	}

	@Override
	public JMenuItem add(Action a) {
		JMenuItem item = new JMenuItem(a);
		items.add(item);
		return item;
	}

	@Override
	public Component add(Component c) {
		items.add(c);
		return c;
	}

	@Override
	public Component add(Component c, int index) {
		if (index == -1)
			index = items.size();
		items.add(index, c);
		return c;
	}

	@Override
	public void add(Component comp, Object constraints) {
		items.add(comp);
		super.add(comp, constraints);
	}

	@Override
	public void add(Component comp, Object constraints, int index) {
		items.add(comp);
		super.add(comp, constraints, index);
	}

	@Override
	public JMenuItem add(JMenuItem menuItem) {
		items.add(menuItem);
		return menuItem;
	}

	@Override
	public synchronized void add(PopupMenu popup) {
		throw new UnsupportedOperationException(
				"Cannot add non-components to dynamic menu");
	}

	@Override
	public Component add(String name, Component comp) {
		return add(comp);
	}

	@Override
	public JMenuItem add(String s) {
		JMenuItem item = new JMenuItem(s);
		items.add(item);
		return item;
	}

	@Override
	public void remove(Component c) {
		items.remove(c);
		super.remove(c);
	}

	@Override
	public void remove(int pos) {
		items.remove(pos);
		super.remove(pos);
	}

	@Override
	public void remove(JMenuItem item) {
		items.remove(item);
		super.remove(item);
	}

	@Override
	public synchronized void remove(MenuComponent popup) {
		items.remove(popup);
		super.remove(popup);
	}

	@Override
	public void removeAll() {
		items.clear();
		super.removeAll();
	}
}
