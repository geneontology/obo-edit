package org.obo.filters;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

import javax.swing.JPanel;

public abstract class AbstractFilterEditor extends JPanel implements FilterEditor {

	protected Collection filterEditUpdateListeners = new LinkedList();

	public void addFilterEditUpdateListener(FilterEditUpdateListener listener) {
		filterEditUpdateListeners.add(listener);
	}

	public void removeFilterEditUpdateListener(FilterEditUpdateListener listener) {
		filterEditUpdateListeners .remove(listener);
	}
	
	protected void fireFilterEditUpdate() {
		acceptEdits();
		Iterator it = filterEditUpdateListeners.iterator();
		while(it.hasNext()) {
			FilterEditUpdateListener listener = (FilterEditUpdateListener) it.next();
			listener.update();
		}
	}

}
