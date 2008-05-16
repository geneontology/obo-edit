package org.oboedit.gui;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

import javax.swing.JComponent;
import javax.swing.tree.TreePath;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.PathCapable;
import org.obo.datamodel.RootAlgorithm;
import org.oboedit.util.PathUtil;

import org.apache.log4j.*;

public class DefaultSelection implements Selection {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefaultSelection.class);

	protected JComponent component;

	protected Collection<Link> links;

	protected Collection<LinkedObject> terms;

	protected TreePath[] paths;

	protected PathCalcMode mode;

	protected RootAlgorithm rootAlgorithm;

	protected LinkDatabase linkDatabase;

	protected Link linkSubSelection;

	protected LinkedObject subSelection;

	public DefaultSelection(JComponent component,
			Collection<Link> links,
			Collection<? extends LinkedObject> terms, TreePath[] paths,
			RootAlgorithm rootAlgorithm, LinkDatabase linkDatabase,
			PathCalcMode mode, Link linkSubSelection, LinkedObject subSelection) {
		this.component = component;
		this.links = links;
		this.terms = new LinkedList<LinkedObject>(terms);
		this.paths = paths;
		this.mode = mode;
		this.rootAlgorithm = rootAlgorithm;
		this.linkDatabase = linkDatabase;
		if (subSelection == null && terms.size() > 0)
			subSelection = terms.iterator().next();
		this.subSelection = subSelection;
		if (linkSubSelection == null && links.size() > 0)
			linkSubSelection = links.iterator().next();
		this.linkSubSelection = linkSubSelection;
	}

	public JComponent getComponent() {
		return component;
	}

	public Collection<PathCapable> getAllSelectedObjects() {
		Collection<PathCapable> out = new LinkedList<PathCapable>();
		out.addAll(getLinks());
		out.addAll(getTerms());
		return out;
	}

	public Collection<Link> getLinks() {
		return links;
	}

	public TreePath[] getPaths() {
		if (paths == null)
			paths = getPaths(rootAlgorithm, linkDatabase);
		return paths;
	}

	public TreePath getPath() {
		if (paths == null)
			return getPaths(rootAlgorithm, linkDatabase)[0];
		else
			return paths[0];
	}

	public boolean selectBestOnly() {
		return true;
	}

	public TreePath[] getPaths(RootAlgorithm rootAlgorithm,
			LinkDatabase linkDatabase) {
		if (mode.equals(PathCalcMode.DONT_CALCULATE)) {
			return paths;
		} else if (mode.equals(PathCalcMode.CALCULATE_FROM_LINKS))
			paths = PathUtil.getPaths(links, rootAlgorithm, linkDatabase,
					selectBestOnly());
		else if (mode.equals(PathCalcMode.CALCULATE_FROM_TERMS))
			paths = PathUtil.getPaths(terms, rootAlgorithm, linkDatabase,
					selectBestOnly());
		else if (mode.equals(PathCalcMode.CALCULATE_FROM_TERMS_AND_LINKS)) {
			Set<TreePath> temp = new HashSet<TreePath>();
			for(TreePath path : PathUtil.getPaths(links, rootAlgorithm, linkDatabase,
					selectBestOnly())) {
				temp.add(path);
			}
			for(TreePath path : PathUtil.getPaths(terms, rootAlgorithm, linkDatabase,
					selectBestOnly())) {
				temp.add(path);
			}
			paths = new TreePath[temp.size()];
			int i = 0;
			for(TreePath path : temp) {
				paths[i++] = path;
			}
		}
		return paths;
	}

	public TreePath getPath(RootAlgorithm rootAlgorithm,
			LinkDatabase linkDatabase) {

		if (mode.equals(PathCalcMode.DONT_CALCULATE))
			return null;

		if (mode.equals(PathCalcMode.CALCULATE_FROM_LINKS))
			return PathUtil.getBestPath(linkSubSelection, rootAlgorithm,
					linkDatabase);
		else if (mode.equals(PathCalcMode.CALCULATE_FROM_TERMS))
			return PathUtil.getBestPath(subSelection, rootAlgorithm,
					linkDatabase);
		else
			return null;
	}

	public ObjectSelector getSelector() {
		if (component instanceof ObjectSelector)
			return (ObjectSelector) component;
		return null;
	}

	public Collection<LinkedObject> getTerms() {
		return terms;
	}

	public Link getLinkSubSelection() {
		return linkSubSelection;
	}

	public LinkedObject getTermSubSelection() {
		return subSelection;
	}

	public boolean isEmpty() {
		return getLinks().size() == 0 && getTerms().size() == 0;
	}

	public PathCalcMode getMode() {
		return mode;
	}

	public RootAlgorithm getRootAlgorithm() {
		return rootAlgorithm;
	}

	public LinkDatabase getLinkDatabase() {
		return linkDatabase;
	}
	
	public boolean equals(Object o) {
		if (o instanceof Selection) {
			Selection s = (Selection) o;
			return s.getAllSelectedObjects().equals(getAllSelectedObjects());
		} else
			return false;
	}


    public String toString() {
	String s = "Selection with " + getAllSelectedObjects().size() + " objects: ";
	for (PathCapable pc : getAllSelectedObjects()) {
	    if (pc instanceof LinkedObject) {
		LinkedObject lo = (LinkedObject) pc;
		s += lo.getName() + " ";
	    }
	    else if (pc instanceof Link) {
		Link link = (Link) pc;
		s += "link:" + link + " ";
	    }
	    else
		s += pc;
	}
	return s;
    }

}
