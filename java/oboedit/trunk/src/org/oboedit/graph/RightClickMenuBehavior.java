package org.oboedit.graph;

import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.piccolo.PiccoloUtil;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
import edu.umd.cs.piccolo.event.PInputEvent;

import org.apache.log4j.*;

public class RightClickMenuBehavior implements ViewBehavior {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(RightClickMenuBehavior.class);

	protected LinkDatabaseCanvas canvas;
	protected List<RightClickMenuFactory> menuFactories = new ArrayList<RightClickMenuFactory>();

	protected PBasicInputEventHandler handler = new PBasicInputEventHandler() {
		public void mouseClicked(PInputEvent event) {
			if (event.isRightMouseButton() && event.getClickCount() == 1) {
				doClick(event);
			}
		}
	};

	public RightClickMenuBehavior() {
	}

	public void install(LinkDatabaseCanvas canvas) {
		this.canvas = canvas;
		canvas.addInputEventListener(handler);
	}

	public void uninstall(LinkDatabaseCanvas canvas) {
		canvas.removeInputEventListener(handler);
		this.canvas = canvas;
	}

	protected void doClick(PInputEvent event) {
		JPopupMenu menu = new JPopupMenu();
		for (RightClickMenuFactory factory : menuFactories) {
			Collection<JMenuItem> factories = factory.getMenuItems(canvas,
					event);
			if (factories == null)
				continue;
			for (JMenuItem item : factories) {
				if (item == null)
					continue;
				if (item == RightClickMenuFactory.SEPARATOR_ITEM)
					menu.addSeparator();
				else
					menu.add(item);
			}
		}
		MouseEvent e = (MouseEvent) event.getSourceSwingEvent();
		if (menu.getComponentCount() > 0)
			menu.show(canvas, (int) e.getX(), (int) e.getY());
	}
}
