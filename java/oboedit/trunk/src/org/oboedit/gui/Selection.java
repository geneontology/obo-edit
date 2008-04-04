package org.oboedit.gui;

import java.util.Collection;

import javax.swing.JComponent;
import javax.swing.tree.TreePath;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.PathCapable;
import org.obo.datamodel.RootAlgorithm;

public interface Selection {
	public static enum PathCalcMode {
		DONT_CALCULATE, CALCULATE_FROM_LINKS, CALCULATE_FROM_TERMS,
		CALCULATE_FROM_TERMS_AND_LINKS
	};

	public boolean isEmpty();

	public Collection<Link> getLinks();

	public Collection<LinkedObject> getTerms();

	public TreePath[] getPaths();

	public ObjectSelector getSelector();

	public JComponent getComponent();

	public LinkedObject getTermSubSelection();
	
	public LinkDatabase getLinkDatabase();

	public Link getLinkSubSelection();

	public TreePath getPath();

	public TreePath[] getPaths(RootAlgorithm rootAlgorithm,
			LinkDatabase linkDatabase);

	public TreePath getPath(RootAlgorithm rootAlgorithm,
			LinkDatabase linkDatabase);

	public Collection<PathCapable> getAllSelectedObjects();

	public PathCalcMode getMode();

	public RootAlgorithm getRootAlgorithm();

    public String toString();
}