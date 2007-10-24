package org.oboedit.gui.tasks;

import org.bbop.dataadapter.DataAdapter;
import org.bbop.framework.ComponentManager;
import org.bbop.framework.GUIManager;
import org.bbop.framework.GUITask;
import org.bbop.framework.GUIComponentFactory;
import org.obo.filters.SearchCriterion;
import org.oboedit.controller.FilterManager;
import org.oboedit.controller.IOManager;
import org.oboedit.controller.PluginManager;
import org.oboedit.gui.AbstractSingleActionTask;

public class PluginInitStartupTask extends AbstractSingleActionTask {

	public void run() {
		for (GUITask task : PluginManager.getManager().instantiateAll(
				GUITask.class)) {
			GUIManager.getManager().addStartupTask(task);
		}
		for (GUIComponentFactory<?> factory : PluginManager.getManager()
				.instantiateAll(GUIComponentFactory.class)) {
			ComponentManager.getManager().install(factory);
		}
		for (DataAdapter adapter : PluginManager.getManager().instantiateAll(
				DataAdapter.class)) {
			IOManager.getManager().installDataAdapter(adapter);
		}
		for (SearchCriterion<?, ?> criterion : PluginManager.getManager()
				.instantiateAll(SearchCriterion.class)) {
			FilterManager.getManager().addCriterion(criterion);
		}
	}

}
