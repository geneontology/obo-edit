package org.oboedit.gui;

import javax.swing.JComponent;
import javax.swing.tree.TreePath;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.RootAlgorithm;
import org.oboedit.gui.Selection.PathCalcMode;
import org.oboedit.util.PathUtil;

public class DefaultGestureTarget implements GestureTarget {
	protected Link link;
	protected LinkedObject term;
	protected JComponent component;
	protected PathCalcMode mode;
	protected TreePath path;
	protected RootAlgorithm rootAlgorithm;
	protected LinkDatabase linkDatabase;

	public DefaultGestureTarget(JComponent component, Link link,
			LinkedObject term, TreePath path, RootAlgorithm rootAlgorithm,
			LinkDatabase linkDatabase,
			PathCalcMode mode) {
		this.component = component;
		this.link = link;
		this.term = term;
		this.path = path;
		this.mode = mode;
		this.rootAlgorithm = rootAlgorithm;
		this.linkDatabase = linkDatabase;
	}
	
	public TreePath getPath() {
		if (path == null)
			path = getPath(rootAlgorithm, linkDatabase);
		return path;
	}
	
	public TreePath getPath(RootAlgorithm rootAlgorithm,
			LinkDatabase linkDatabase) {

		if (mode.equals(PathCalcMode.DONT_CALCULATE))
			return path;

		if (mode.equals(PathCalcMode.CALCULATE_FROM_LINKS))
			return (link == null ? null : PathUtil.getBestPath(link, rootAlgorithm,
					linkDatabase));
		else if (mode.equals(PathCalcMode.CALCULATE_FROM_TERMS))
			return (term == null ? null : PathUtil.getBestPath(term, rootAlgorithm,
					linkDatabase));
		else
			return null;
	}
		
	public boolean isEmpty() {
		return link == null && term == null;
	}

	public Link getLink() {
		return link;
	}

	public LinkedObject getTerm() {
		return term;
	}

	public JComponent getComponent() {
		return component;
	}

    public String toString() {
	if (getTerm() == null)
	    return "";
	return "GestureTarget: " + getTerm().getName();
    }
}
