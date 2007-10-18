package org.oboedit.gui.components;

import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.ComponentManager;
import org.bbop.framework.GUIManager;
import org.bbop.util.SwingUpdateTask;
import org.bbop.util.TaskDelegate;
import org.obo.datamodel.PathCapable;
import org.obo.filters.Filter;
import org.obo.query.QueryEngine;
import org.obo.query.impl.FilterQuery;
import org.obo.query.impl.SearchHit;
import org.obo.util.FilterUtil;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.FilterComponent;
import org.oboedit.gui.SearchPanel;
import org.oboedit.gui.TermFilterEditorFactory;
import org.oboedit.gui.factory.SearchResultsComponentFactory;

public class SearchComponent extends AbstractGUIComponent {

	protected FilterComponent component = new FilterComponent(
			new TermFilterEditorFactory());

	protected int id = 1;

	public SearchComponent(String id) {
		super(id);
	}

	@Override
	public void init() {
		component.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				search();
			}
		});
		removeAll();
		setLayout(new GridLayout(1, 1));
		add(component);
		validate();
	}

	@Override
	public void cleanup() {
		component.cleanup();
		super.cleanup();
	}

	protected void search() {
		QueryEngine engine = SessionManager.getManager().getQueryEngine();
		final Filter filter = component.getFilter();
		final TaskDelegate<Collection<SearchHit<PathCapable>>> task = engine
				.query(SessionManager.getManager().getSession(),
						new FilterQuery(filter, PathCapable.class));
		Runnable r = new Runnable() {

			public void run() {
				String id = ComponentManager.getManager().showComponent(
						new SearchResultsComponentFactory(),
						SearchComponent.this,
						"Search results: "
								+ FilterUtil.getOBOFilterExpression(filter));
				SearchResultsComponent src = (SearchResultsComponent) ComponentManager
						.getManager().getActiveComponent(id);
				src.setResults(task.getResults());
			}

		};
		GUIManager
				.getManager()
				.scheduleTask(
						new SwingUpdateTask<Collection<SearchHit<PathCapable>>>(
								task, r), true);
	}

	@Override
	public String getName() {
		return "Search Component";
	}
}
