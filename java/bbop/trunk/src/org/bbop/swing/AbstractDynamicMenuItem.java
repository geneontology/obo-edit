package org.bbop.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.MenuComponent;
import java.awt.PopupMenu;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.Serializable;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import javax.swing.Action;
import javax.swing.ButtonModel;
import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JSeparator;
import javax.swing.KeyStroke;
import javax.swing.MenuElement;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;

import org.bbop.util.ObjectUtil;

public abstract class AbstractDynamicMenuItem extends JMenu implements
		DynamicMenuItem {

	protected boolean bracketTop;

	protected boolean bracketBottom;

	protected boolean merge;
	
	protected boolean addSep = false;

	public AbstractDynamicMenuItem(String s) {
		this(s, true, false, false);
	}

	public AbstractDynamicMenuItem(String s, boolean merge, boolean bracketTop,
			boolean bracketBottom) {
		super(s, true);
		setName(escapeID(s));
		this.bracketTop = bracketTop;
		this.bracketBottom = bracketBottom;
		this.merge = merge;
		addMenuListener(new MenuListener() {
			public void menuSelected(MenuEvent e) {
				buildMenu();
			}

			public void menuCanceled(MenuEvent e) {
				AbstractDynamicMenuItem.super.removeAll();
			}

			public void menuDeselected(MenuEvent e) {
				AbstractDynamicMenuItem.super.removeAll();
			}
		});
	}

	protected void addItemToMenu(Component o, boolean isFirst, boolean isLast) {
		if (o instanceof DynamicMenuItem && (((DynamicMenuItem) o).getMerge())) {
			DynamicMenuItem dmi = (DynamicMenuItem) o;
			if (dmi.bracketTop() && !isFirst) {
				Component last = getMenuComponent(getMenuComponentCount() - 1);
				if (!(last instanceof JSeparator)) {
					addSep = true;
				}
			}
			if (addSep) {
				super.addSeparator();
				addSep = false;
			}
			int j = 0;
			List<? extends Component> items = dmi.getItems();
			for (Component d : items) {
				addItemToMenu(d, j == 0, j >= items.size() - 1);
				j++;
			}
			if (dmi.bracketBottom()) {
				if (getMenuComponentCount() > 0 && !isLast) {
					addSep = true;
				}
			}
		} else if (o instanceof JMenuItem) {
			if (addSep) {
				super.addSeparator();
				addSep = false;
			}
			super.add((JMenuItem) o);
		} else if (o instanceof Component) {
			if (addSep) {
				if (!(o instanceof JSeparator))
					super.addSeparator();
				addSep = false;
			}

			super.add((Component) o);
		} else
			throw new RuntimeException();
	}
	
	@Override
	protected boolean processKeyBinding(KeyStroke ks, KeyEvent e,
			int condition, boolean pressed) {
		if (!pressed)
			return false;
		Collection<? extends Component> items = getItems();
		for(Component c : items) {
			if (processComponent(ks, c))
				return true;
		}
		return false;
	}
	
	protected boolean processComponent(KeyStroke ks, Component c) {
		boolean match = false;
		if (!c.isEnabled())
			return false;
		if (c instanceof JMenuItem) {
			if (ObjectUtil.equals(((JMenuItem) c).getAccelerator(), ks)) {
				((JMenuItem) c).doClick();
				return true;
			}
		}
		if (c instanceof DynamicMenuItem) {
			for(Component child : ((DynamicMenuItem) c).getItems()) {
				if (processComponent(ks, child))
					return true;
			}
		} else if (c instanceof Container) {
			for(Component child : ((Container) c).getComponents()) {
				if (processComponent(ks, c))
					return true;
			}
		}
		return false;
	}

	public void buildMenu() {
		super.removeAll();
		addSep = false;
		int index = 0;
		Collection<? extends Component> items = getItems();
		for (Component c : items) {
			addItemToMenu(c, index == 0, index >= items.size() - 1);
			index++;
		}
		setEnabled(items.size() > 0);
	}

	protected static String escapeID(String s) {
		return s.replace(':', '_').replace('|', '_');
	}

	public boolean getMerge() {
		return merge;
	}

	public boolean bracketBottom() {
		return bracketBottom;
	}

	public boolean bracketTop() {
		return bracketTop;
	}

	@Override
	public void addSeparator() {
	}

	@Override
	public JMenuItem add(Action a) {
		return null;
	}

	@Override
	public Component add(Component c) {
		return null;
	}

	@Override
	public Component add(Component c, int index) {
		return null;
	}

	@Override
	public void add(Component comp, Object constraints) {
	}

	@Override
	public void add(Component comp, Object constraints, int index) {
	}

	@Override
	public JMenuItem add(JMenuItem menuItem) {
		return null;
	}

	@Override
	public synchronized void add(PopupMenu popup) {
	}

	@Override
	public Component add(String name, Component comp) {
		return null;
	}

	@Override
	public JMenuItem add(String s) {
		return null;
	}

	@Override
	public void remove(Component c) {
	}

	@Override
	public void remove(int pos) {
	}

	@Override
	public void remove(JMenuItem item) {
	}

	@Override
	public synchronized void remove(MenuComponent popup) {
	}

	@Override
	public void removeAll() {
	}

	@Override
	public Component getMenuComponent(int n) {
		return getItems().get(n);
	}

	@Override
	public int getMenuComponentCount() {
		return getItems().size();
	}

	@Override
	public Component[] getMenuComponents() {
		Collection<? extends Component> items = getItems();
		return items.toArray(new Component[items.size()]);
	}

	public boolean isBracketTop() {
		return bracketTop;
	}

	public void setBracketTop(boolean bracketTop) {
		this.bracketTop = bracketTop;
	}

	public boolean isBracketBottom() {
		return bracketBottom;
	}

	public void setBracketBottom(boolean bracketBottom) {
		this.bracketBottom = bracketBottom;
	}

	public void setMerge(boolean merge) {
		this.merge = merge;
	}
	

}
