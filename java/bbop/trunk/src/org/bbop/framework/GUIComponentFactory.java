package org.bbop.framework;

import java.util.Collection;
import java.util.List;
import java.util.Properties;

import org.bbop.util.StringUtil;

public interface GUIComponentFactory<T extends GUIComponent> {

	public static enum FactoryCategory {
		TOOLBARS, ONTOLOGY, ANNOTATION, SEARCH, INFO, REASONER, HISTORY, METADATA, CONFIG, TOOLS, MISC;

		public String toString() {
			return StringUtil.toTitleCase(name().toLowerCase());
		}
	};

	public boolean showInMenus();

	public List<String> getIDs();

	public String getName();

	public T createComponent(String id);

	public boolean getPreferSeparateWindow();

	public boolean isSingleton();

	public String getDefaultID();

	public FactoryCategory getCategory();
}
