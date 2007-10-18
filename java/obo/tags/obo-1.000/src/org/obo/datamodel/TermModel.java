package org.obo.datamodel;

import javax.swing.tree.*;

import org.obo.filters.*;

public interface TermModel extends TreeModel {

	public static final Object CLASSES = "CLASSES";
	public static final Object OBSOLETE = "OBSOLETE";
	public static final Object ROOT = "ROOT";
	public static final Object TYPES = "PROPERTIES";
	public static final Object INSTANCES = "INSTANCES";

	public void reload();

	public void setTermFilter(Filter filter);

	public void setLinkFilter(Filter filter);

	public Filter getTermFilter();

	public Filter getLinkFilter();

	public RootAlgorithm getRootAlgorithm();

	public LinkDatabase getLinkDatabase();

	public void setShowTerms(boolean showTerms);

	public void setShowTypes(boolean showTypes);

	public void setShowObsoletes(boolean showObsoletes);

	public void setShowInstances(boolean showInstances);

	public boolean getShowTerms();

	public boolean getShowTypes();

	public boolean getShowObsoletes();
}
