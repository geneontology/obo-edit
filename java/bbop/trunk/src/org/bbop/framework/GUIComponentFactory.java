package org.bbop.framework;

import org.bbop.util.StringUtil;

public interface GUIComponentFactory<T extends GUIComponent> {

	public static enum FactoryCategory {
//		TOOLBARS, ONTOLOGY, ANNOTATION, SEARCH, INFO, REASONER, HISTORY, METADATA, CONFIG, TOOLS, MISC;
		TOOLBARS, ONTOLOGY, ANNOTATION, EDITORS, VIEWERS, SEARCH, TOOLS, METADATA, INFO, REASONER, HISTORY, SUMMARY, CONFIG, MISC;


		@Override
		public String toString() {
			return StringUtil.toTitleCase(name().toLowerCase());
		}
	};

	public boolean showInMenus();

	public String getID();

	public String getName();

	public T createComponent(String id);

	public boolean getPreferSeparateWindow();

	public boolean isSingleton();

	public FactoryCategory getCategory();
	
	public boolean isRestoreOnStartup();
	
	public String getHelpTopicID();
}
