package org.bbop.swing.dragtree.event;

import java.awt.event.*;
import java.awt.Component;
import javax.swing.tree.TreePath;

public class DragTreeEvent extends ComponentEvent {
    /**
	 * 
	 */
	private static final long serialVersionUID = -7333998414602691177L;
	private static int id = 0;
    protected TreePath target;
    protected Component component;
    protected MouseEvent mouseEvent;

    public DragTreeEvent(Component component,
			 TreePath targetPath,			 
			 MouseEvent event) {
	super(component, id++);
	this.target = targetPath;
	this.mouseEvent = event;
    }

    public MouseEvent getMouseEvent() {
	return mouseEvent;
    }

    public TreePath getTarget() {
	return target;
    }
}
