package org.oboedit.gui;

import javax.swing.tree.*;

import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.RootAlgorithm;
import org.obo.filters.*;

public interface TermModel extends TreeModel {

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

	public void reloadFilters();
}
