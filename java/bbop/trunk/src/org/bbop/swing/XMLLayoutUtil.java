package org.bbop.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.Frame;
import java.awt.Window;
import java.util.Collection;
import java.util.LinkedList;

import org.bbop.expression.ExpressionException;

import org.apache.log4j.*;

public class XMLLayoutUtil {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(XMLLayoutUtil.class);
	private XMLLayoutUtil() {

	}

	public static Collection<ExpressionException> guiupdateAll() {
		Collection<ExpressionException> out = new LinkedList<ExpressionException>();
		for (Frame frame : Frame.getFrames()) {
			guiupdateTree(frame, out);
		}
		return out;
	}

	protected static void guiupdateTree(Window w,
			Collection<ExpressionException> exceptions) {
		for(Window child : w.getOwnedWindows()) {
			guiupdateTree(child);
		}
		guiupdateTree((Component) w, exceptions);
	}

	public static Collection<ExpressionException> guiupdateTree(Component c) {
		Collection<ExpressionException> out = new LinkedList<ExpressionException>();
		guiupdateTree(c, out);
		c.repaint();
		return out;
	}

	protected static void guiupdateTree(Component c,
			Collection<ExpressionException> exceptions) {
		if (c instanceof XMLLayoutComponent) {
			try {
				((XMLLayoutComponent) c).guiupdate();
			} catch (ExpressionException e) {
				exceptions.add(e);
			}
		}
		if (c instanceof Container) {
			for (Component child : ((Container) c).getComponents()) {
				guiupdateTree(child, exceptions);
			}
		}
	}

}
