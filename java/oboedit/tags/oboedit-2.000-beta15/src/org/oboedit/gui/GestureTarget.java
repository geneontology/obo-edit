package org.oboedit.gui;

import javax.swing.JComponent;
import javax.swing.tree.TreePath;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.RootAlgorithm;

public interface GestureTarget {
	public LinkedObject getTerm();
	public Link getLink();
	public TreePath getPath();
	public TreePath getPath(RootAlgorithm rootAlgorithm, LinkDatabase linkDatabase);
	public JComponent getComponent();
	public boolean isEmpty();
}
