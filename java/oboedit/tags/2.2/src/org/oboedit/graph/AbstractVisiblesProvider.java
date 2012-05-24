package org.oboedit.graph;

import java.util.Collection;
import java.util.LinkedList;

import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.PathCapable;
import org.oboedit.gui.Selection;

public abstract class AbstractVisiblesProvider implements VisiblesProvider {

	protected String label;

	protected boolean checkEnabledAgainstVisibles = true;

	protected Collection<EnabledCheckDelegate> enabledCheckDelegates = new LinkedList<EnabledCheckDelegate>();
	
	protected SelectionProvider provider = null;

	public AbstractVisiblesProvider(String label) {
		this(label, true, null);
	}

	public AbstractVisiblesProvider(String label,
			boolean checkEnabledAgainstVisibles,
			SelectionProvider selectionProvider,
			EnabledCheckDelegate... delegates) {
		this.label = label;
		this.checkEnabledAgainstVisibles = checkEnabledAgainstVisibles;
		this.provider = selectionProvider;
		for(EnabledCheckDelegate delegate : delegates)
			addEnabledCheckDelegate(delegate);
	}
	
	public void setSelectionProvider(SelectionProvider provider) {
		this.provider = provider;
	}
	
	public SelectionProvider getSelectionProvider() {
		return provider;
	}

	public String getLabel() {
		return label;
	}

	public Collection<? extends LinkedObject> getHidden(Selection selection) {
		return null;
	}

	public Collection<? extends LinkedObject> getShown(Selection selection) {
		return null;
	}

	protected boolean isEnabled(Selection selection) {
		for (EnabledCheckDelegate d : enabledCheckDelegates) {
			if (!d.isEnabled(selection))
				return false;
		}
		return true;
	}

	public void addEnabledCheckDelegate(EnabledCheckDelegate d) {
		enabledCheckDelegates.add(d);
	}

	public void removeEnabledCheckDelegate(EnabledCheckDelegate d) {
		enabledCheckDelegates.remove(d);
	}

	public boolean isEnabled(Selection selection,
			Collection<? extends PathCapable> currentlyVisible) {
		if (!checkEnabledAgainstVisibles) {
			return isEnabled(selection);
		} else {
			boolean enabled = false;
			Collection<? extends LinkedObject> hidden = getHidden(selection);
			Collection<? extends LinkedObject> shown = getShown(selection);
			if (hidden != null) {
				for (PathCapable pc : currentlyVisible) {
					if (getHidden(selection).contains(pc)) {
						enabled = true;
						break;
					}
				}
			}
			if (shown != null) {
				for (LinkedObject lo : shown) {
					if (!currentlyVisible.contains(lo)) {
						enabled = true;
						break;
					}
				}
			}
			return enabled && isEnabled(selection);
		}
	}

}
