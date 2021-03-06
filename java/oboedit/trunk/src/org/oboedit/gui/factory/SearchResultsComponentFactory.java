package org.oboedit.gui.factory;

import java.util.HashMap;
import java.util.Map;

import javax.swing.JComponent;

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.ComponentManager;
import org.bbop.framework.GUIComponent;
import org.bbop.framework.dock.LayoutAdapter;
import org.bbop.framework.dock.LayoutListener;
import org.oboedit.gui.components.SearchResultsComponent;

import org.apache.log4j.*;

public class SearchResultsComponentFactory extends
	AbstractComponentFactory<SearchResultsComponent> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SearchResultsComponentFactory.class);

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

	protected LayoutListener listener = new LayoutAdapter() {

		public boolean closing(GUIComponent c) {
			if (c instanceof SearchResultsComponent) {
				uncache(c.getID());
			}
			return true;
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
		return FactoryCategory.SEARCH;
	}

	@Override
	public boolean showInMenus() {
		return false;
	}

	public String getName() {
		return "Search Results";
	}

}
