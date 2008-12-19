package org.oboedit.gui.components;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.ComponentManager;
import org.bbop.framework.ConfigurationPanel;
import org.bbop.framework.GUIManager;
import org.bbop.util.EnumPersistenceDelegate;
import org.bbop.util.SwingUpdateTask;
import org.bbop.util.TaskDelegate;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.PathCapable;
import org.obo.filters.Filter;
import org.obo.filters.ParentSearchCriterion;
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

import org.apache.log4j.*;

public class SearchComponent extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SearchComponent.class);

	public static enum ResultLabelType {
		RESULT_COUNT {
			@Override
			public String toString() {
				return "Result Count";
			}
		},
		QUERY_NAME {
			@Override
			public String toString() {
				return "Query Expression";
			}
		},
		NONE {
			@Override
			public String toString() {
				return "Nothing";
			}
		};

		static {
			EnumPersistenceDelegate.installFor(values()[0].getClass());
		}
	}

	protected FilterComponent component;
	protected SearchComponentFactory factory;
	protected boolean shortenResultsDesc;
	protected ResultLabelType labelType = ResultLabelType.QUERY_NAME;

	protected int id = 1;

	public static class SearchConfig implements ComponentConfiguration {
		protected boolean shortenResultsDesc;
		protected String labelType = ResultLabelType.QUERY_NAME.name();
		protected Filter<?> filter;

		public SearchConfig(boolean shortenResultsDesc, ResultLabelType labelType,
				Filter<?> filter) {
			super();
			this.shortenResultsDesc = shortenResultsDesc;
			this.labelType = labelType.name();
			this.filter = filter;
		}

		public SearchConfig() {
		}

		public boolean isShortenResultsDesc() {
			return shortenResultsDesc;
		}

		public void setShortenResultsDesc(boolean shortenResultsDesc) {
			this.shortenResultsDesc = shortenResultsDesc;
		}

	        public String getLabelType() {
			return labelType;
		}

		public ResultLabelType getRealizedType() {
			return ResultLabelType.valueOf(labelType);
		}

		public void setLabelType(String labelType) {
			if (labelType == null)
				this.labelType = ResultLabelType.QUERY_NAME.name();
			this.labelType = labelType;
		}

		public Filter<?> getFilter() {
			return filter;
		}

		public void setFilter(Filter<?> filter) {
			this.filter = filter;
		}
	}

	@Override
	public ConfigurationPanel getConfigurationPanel() {
		ConfigurationPanel p = new ConfigurationPanel() {

			protected JCheckBox shortenResultsBox = new JCheckBox(
					"Start title of search results panels with \"Search results:\"",
					true);
			protected JComboBox resultLabelDropdown = new JComboBox();

			    {
				for (ResultLabelType type : ResultLabelType.values()) {
					resultLabelDropdown.addItem(type);
				}
				setLayout(new BorderLayout());
				add(shortenResultsBox, "North");
				resultLabelDropdown.setMaximumSize(
				    new Dimension(Integer.MAX_VALUE, resultLabelDropdown.getPreferredSize().height));

				Box resultPanel = Box.createHorizontalBox();
				resultPanel.add(new JLabel(
						"End title of search results panels with: "));
				resultPanel.add(resultLabelDropdown);
				add(resultPanel, "Center");
			    }

			@Override
			public void commit() {
				SearchConfig config = (SearchConfig) getComponent()
						.getConfiguration();
				config.setShortenResultsDesc(!shortenResultsBox.isSelected());
				config.setLabelType(((ResultLabelType) resultLabelDropdown
						.getSelectedItem()).name());
				getComponent().setConfiguration(config);
			}

			@Override
			public void init() {
				SearchConfig config = (SearchConfig) getComponent()
						.getConfiguration();
				shortenResultsBox.setSelected(!config.isShortenResultsDesc());
				resultLabelDropdown.setSelectedItem(config.getRealizedType());
			}
		};
		return p;
	}

	@Override
	public ComponentConfiguration getConfiguration() {
		return new SearchConfig(isShortenResultsDesc(), getLabelType(),
					component.getFilter());
	}

	public void setConfiguration(ComponentConfiguration config) {
		if (config instanceof SearchConfig) {
			SearchConfig searchConfig = (SearchConfig) config;
			setShortenResultsDesc((searchConfig).isShortenResultsDesc());
			setLabelType(searchConfig.getRealizedType());
			if (searchConfig.getFilter() != null)
				component.setFilter(searchConfig.getFilter());
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
		LinkDatabase linkDatabase = SessionManager.getManager()
				.getCurrentLinkDatabase();
		final TaskDelegate<Collection<SearchHit<?>>> task = engine.query(
				linkDatabase, new FilterQuery(filter, resultType,
						SessionManager.getManager()
						.getReasoner()));
		Runnable r = new Runnable() {

			public void run() {
				String desc = "";
				if (labelType.equals(ResultLabelType.QUERY_NAME))
				    desc = " " + FilterUtil.getOBOFilterExpression(filter) +
					// By request, putting the # of matches back in the search results description
					" (" + task.getResults().size() + " matches)";
				else if (labelType.equals(ResultLabelType.RESULT_COUNT)) {
					desc = " " + task.getResults().size() + " matches";
				}
				String title = (isShortenResultsDesc() ? ""
						: "Search results: ")
						+ desc;
				String id = ComponentManager.getManager().showComponent(
						ComponentManager.getManager().getFactory(
								"SEARCH_RESULTS"), SearchComponent.this, title,
						false);
				SearchResultsComponent src = (SearchResultsComponent) ComponentManager
						.getManager().getActiveComponent(id);
				src.setFactory(factory);
				src.setResults(task.getResults());
				ComponentManager.getManager().focusComponent(src);
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

	protected ResultLabelType getLabelType() {
		return labelType;
	}

	protected void setLabelType(ResultLabelType labelType) {
		if (labelType == null)
			this.labelType = ResultLabelType.QUERY_NAME;
		else
			this.labelType = labelType;
	}
}
