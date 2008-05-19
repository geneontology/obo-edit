package org.bbop.swing;

import java.awt.Component;
import java.awt.Container;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.StringTokenizer;

import javax.swing.JMenu;

import org.bbop.util.ObjectUtil;

import org.apache.log4j.*;

public class ComponentPath {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ComponentPath.class);

	protected List<ComponentPathElement> components;

	protected ComponentPath(List<ComponentPathElement> components) {
		this.components = Collections.unmodifiableList(components);
	}

	public List<ComponentPathElement> getPathElements() {
		return components;
	}

	public static ComponentPath parse(String str) {
		if (str == null)
			return null;
		List<ComponentPathElement> out = new LinkedList<ComponentPathElement>();
		StringTokenizer tokenizer = new StringTokenizer(str, ":");
		while (tokenizer.hasMoreTokens()) {
			String token = tokenizer.nextToken();
			out.add(ComponentPathElement.parse(token));
		}
		return new ComponentPath(out);
	}

	public static boolean addComponent(String path, Component root,
			Component addMe) {
		return addComponent(path, root, addMe, null, -1);
	}

	public static boolean addComponent(String path, Component root,
			Component addMe, Object constraints, int index) {
		Component parent = getComponent(path, root);
		if (parent != null && parent instanceof Container) {
			if (constraints == null)
				((Container) parent).add(addMe, index);
			else
				((Container) parent).add(addMe, constraints, index);
			parent.validate();
			parent.repaint();
			return true;
		} else {
			logger.info("failed");
			return false;
		}
	}

	public static boolean removeComponent(String path, Component root,
			Component killMe) {
		Component parent = getComponent(path, root);
		if (parent != null && parent instanceof Container) {
			((Container) parent).remove(killMe);
			parent.validate();
			parent.repaint();
			return true;
		} else
			return false;
	}

	public static Component getComponent(String path, Component root) {
		return getComponent(ComponentPath.parse(path), root, null);
	}
	
	protected static Component[] getChildren(Container c) {
		if (c instanceof JMenu) {
			return ((JMenu) c).getMenuComponents();
		} else
			return c.getComponents();
	}

	public static Component getComponent(ComponentPath path, Component root,
			ComponentFactory factory) {
		int i = 0;
		Component match = root;
		if (path != null) {
			for (ComponentPathElement e : path.getPathElements()) {
				match = null;
				if (root instanceof Container) {
					for (Component c : getChildren((Container) root)) {
						if (ObjectUtil.equals(c.getName(), e.getID())) {
							match = c;
							break;
						}
					}
					if (match == null) {
						if (factory == null)
							return null;
						else {
							match = factory.getComponent(e.getID());
							Object constraint = factory.getConstraint(e
									.getConstraint());
							if (constraint == null)
								((Container) root).add(match, e.getIndex());
							else
								((Container) root).add(match, constraint, e
										.getIndex());
						}
					}
				}
				if (match == null)
					return null;
				root = match;
				i++;
			}
		}
		return match;
	}
}
