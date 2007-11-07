package org.oboedit.gui.components;

import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;

import javax.swing.BoxLayout;
import javax.swing.JCheckBox;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.ComponentManager;
import org.bbop.framework.ConfigurationPanel;
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
import org.oboedit.gui.SearchComponentFactory;
import org.oboedit.gui.SearchPanel;
import org.oboedit.gui.TermFilterEditorFactory;
import org.oboedit.gui.factory.SearchResultsComponentFactory;

public class SearchComponent extends AbstractGUIComponent {

	protected FilterComponent component;
	protected SearchComponentFactory factory;
	protected boolean shortenResultsDesc;

	protected int id = 1;

	public static class SearchConfig implements ComponentConfiguration {
		protected boolean shortenResultsDesc;

		public SearchConfig(boolean shortenResultsDesc) {
			super();
			this.shortenResultsDesc = shortenResultsDesc;
		}

		public SearchConfig() {
		}

		public boolean isShortenResultsDesc() {
			return shortenResultsDesc;
		}

		public void setShortenResultsDesc(boolean shortenResultsDesc) {
			this.shortenResultsDesc = shortenResultsDesc;
		}
	}

	@Override
	public ConfigurationPanel getConfigurationPanel() {
		ConfigurationPanel p = new ConfigurationPanel() {

			protected JCheckBox shortenResultsBox = new JCheckBox(
					"Start title of search results panels with \"Search results:\"",
					true);

			{
				setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
				add(shortenResultsBox);
			}

			@Override
			public void commit() {
				SearchConfig config = (SearchConfig) getComponent()
						.getConfiguration();
				config.setShortenResultsDesc(!shortenResultsBox.isSelected());
				getComponent().setConfiguration(config);
			}

			@Override
			public void init() {
				SearchConfig config = (SearchConfig) getComponent()
						.getConfiguration();
				shortenResultsBox.setSelected(!config.isShortenResultsDesc());
			}
		};
		return p;
	}

	@Override
	public ComponentConfiguration getConfiguration() {
		return new SearchConfig(isShortenResultsDesc());
	}

	public void setConfiguration(ComponentConfiguration config) {
		if (config instanceof SearchConfig) {
			setShortenResultsDesc(((SearchConfig) config)
					.isShortenResultsDesc());
		}
	}

	public SearchComponent(String id, SearchComponentFactory factory) {
		super(id);
		this.factory = factory;
		component = new FilterComponent(factory);
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
		Class<?> resultType = factory.getResultType();
		final TaskDelegate<Collection<SearchHit<?>>> task = engine.query(
				SessionManager.getManager().getSession(), new FilterQuery(
						filter, resultType));
		Runnable r = new Runnable() {

			public void run() {
				String title = (isShortenResultsDesc() ? ""
						: "Search results: ")
						+ FilterUtil.getOBOFilterExpression(filter);
				String id = ComponentManager.getManager().showComponent(
						new SearchResultsComponentFactory(),
						SearchComponent.this, title);
				SearchResultsComponent src = (SearchResultsComponent) ComponentManager
						.getManager().getActiveComponent(id);
				src.setFactory(factory);
				src.setResults(task.getResults());
			}

		};
		GUIManager.getManager().scheduleTask(
				new SwingUpdateTask<Collection<SearchHit<?>>>(task, r), true);
	}

	@Override
	public String getName() {
		return "Search Component";
	}

	protected boolean isShortenResultsDesc() {
		return shortenResultsDesc;
	}

	protected void setShortenResultsDesc(boolean shortenResultsDesc) {
		this.shortenResultsDesc = shortenResultsDesc;
	}
}
