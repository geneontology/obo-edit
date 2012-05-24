package org.oboedit.graph;

import java.util.Collection;

import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.PathCapable;
import org.oboedit.gui.Selection;

public interface VisiblesProvider {
	
	public static interface EnabledCheckDelegate {
		public boolean isEnabled(Selection s);
	}
	
	public static interface SelectionProvider {
		public Selection getSelection();
	}
	
	public static class DefaultSelectionProvider implements SelectionProvider {
		
		protected Selection selection;

		public void setSelection(Selection selection) {
			this.selection = selection;
		}

		public Selection getSelection() {
			// TODO Auto-generated method stub
			return selection;
		}
		
	}
	
	public Collection<? extends LinkedObject> getHidden(Selection selection);

	public Collection<? extends LinkedObject> getShown(Selection selection);

	public String getLabel();

	public boolean isEnabled(Selection selection,
			Collection<? extends PathCapable> currentlyVisible);
	
	public SelectionProvider getSelectionProvider();
	
	public void addEnabledCheckDelegate(EnabledCheckDelegate d);
	public void removeEnabledCheckDelegate(EnabledCheckDelegate d);
}
