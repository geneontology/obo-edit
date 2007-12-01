package org.oboedit.gui.factory;

import java.util.HashMap;
import java.util.Map;

import javax.swing.JComponent;

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.ComponentManager;
import org.bbop.framework.GUIComponent;
import org.bbop.framework.dock.LayoutListener;
import org.oboedit.gui.components.SearchResultsComponent;

public class SearchResultsComponentFactory extends
		AbstractComponentFactory<SearchResultsComponent> {

	public static class TemporaryRestoreStruct {
		protected String title;
		protected JComponent component;

		public TemporaryRestoreStruct(String title, JComponent component) {
			this.title = title;
			this.component = component;
		}

		public String getTitle() {
			return title;
		}

		public void setTitle(String title) {
			this.title = title;
		}

		public JComponent getComponent() {
			return component;
		}

		public void setComponent(JComponent component) {
			this.component = component;
		}
	}

	protected Map<String, TemporaryRestoreStruct> temporaryMap = new HashMap<String, TemporaryRestoreStruct>();

	protected LayoutListener listener = new LayoutListener() {

		public void add(GUIComponent parent, GUIComponent child) {
		}

		public void close(GUIComponent c) {
			if (c instanceof SearchResultsComponent) {
				uncache(c.getID());
			}
		}

		public void docked(GUIComponent component) {
		}

		public void focusChanged(GUIComponent old, GUIComponent newComponent) {
		}

		public void maximized(GUIComponent component) {
		}

		public void minimized(GUIComponent component) {
		}

		public void restored(GUIComponent component) {
		}

		public void undocked(GUIComponent component) {
		}
		
		public void titleChanged(GUIComponent component, String newTitle) {
			if (component instanceof SearchResultsComponent) {
				TemporaryRestoreStruct struct = temporaryMap.get(component.getID());
				if (struct != null) {
					struct.setTitle(newTitle);
				}
			}
		}

	};

	protected boolean listenerInstalled = false;

	public SearchResultsComponentFactory() {

	}

	@Override
	public boolean isRestoreOnStartup() {
		return false;
	}

	public void store(String id, String title, JComponent component) {
		temporaryMap.put(id, new TemporaryRestoreStruct(title, component));
	}

	public boolean alreadyStored(String id) {
		return temporaryMap.containsKey(id);
	}

	public void uncache(String id) {
		temporaryMap.remove(id);
	}

	public JComponent getComponent(String id) {
		TemporaryRestoreStruct struct = temporaryMap.get(id);
		return struct.getComponent();
	}

	public String getTitle(String id) {
		TemporaryRestoreStruct struct = temporaryMap.get(id);
		return struct.getTitle();
	}

	public String getID() {
		return "SEARCH_RESULTS";
	}

	@Override
	public SearchResultsComponent doCreateComponent(String id) {
		if (!listenerInstalled) {
			listenerInstalled = true;
			ComponentManager.getManager().addLayoutListener(listener);
		}
		return new SearchResultsComponent(id);
	}

	public FactoryCategory getCategory() {
		return FactoryCategory.INFO;
	}

	@Override
	public boolean showInMenus() {
		return false;
	}

	public String getName() {
		return "Search Results";
	}

}
