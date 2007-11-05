package org.bbop.swing;

import java.awt.Component;
import java.util.List;

import javax.swing.Action;
import javax.swing.JMenuItem;

import org.bbop.util.CollectionUtil;

public class DynamicActionMenuItem extends AbstractDynamicMenuItem {

	protected Action action;
	
	public DynamicActionMenuItem(Action action) {
		super(action.toString()+" wrapper", true, false, false);
		this.action = action;
	}

	public List<? extends Component> getItems() {
		return CollectionUtil.list(new JMenuItem(action));
	}
}
