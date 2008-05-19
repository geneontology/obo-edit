package org.bbop.swing;

import javax.swing.*;

import org.bbop.swing.plaf.*;

import org.apache.log4j.*;

public class JDragList extends JList {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(JDragList.class);

    /**
	 * 
	 */
	private static final long serialVersionUID = 6959466557775104334L;

	public JDragList() {
	super();
	setUI(new DragFriendlyListUI());
    }

    public JDragList(ListModel model) {
	super(model);
	setUI(new DragFriendlyListUI());
    }
}
