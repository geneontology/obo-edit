package org.oboedit.gui;

import java.awt.event.MouseEvent;
import java.util.Collection;

import org.obo.datamodel.*;
import org.oboedit.gui.event.ExpandCollapseListener;
import org.oboedit.gui.event.SelectionListener;

public interface ObjectSelector {

	public void select(Selection selection);

	/**
	 * Most object selectors will just use this method as a delegate to
	 * {@link getSelection()}. This method should be overridden if a special
	 * selection should be used for right-clicks, drags, etc based on mouse position
	 */
	public Selection getSelection(MouseEvent e);
	
	public Selection getSelection();

	public LinkDatabase getLinkDatabase();

	public RootAlgorithm getRootAlgorithm();
	
	public void setLive(boolean isLive);
	
	public boolean isLive();
	
	public void addSelectionListener(SelectionListener listener);
	
	public void removeSelectionListener(SelectionListener listener);
	
	public boolean hasCombinedTermsAndLinks();
	
	public void addExpansionListener(ExpandCollapseListener listener);
	
	public void removeExpansionListener(ExpandCollapseListener listener);
	
	public Collection<PathCapable> getVisibleObjects();
	
}
