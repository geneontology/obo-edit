package org.bbop.swing;

import java.awt.Component;
import java.util.List;

import javax.swing.Action;
import javax.swing.JMenuItem;

import org.bbop.util.CollectionUtil;

import org.apache.log4j.*;

public class DynamicActionMenuItem extends AbstractDynamicMenuItem {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DynamicActionMenuItem.class);

	protected Action action;
	
	public DynamicActionMenuItem(Action action) {
		super(action.toString()+" wrapper", true, false, false);
		this.action = action;
	}

	public List<? extends Component> getItems() {
		return CollectionUtil.list(new JMenuItem(action));
	}
}
