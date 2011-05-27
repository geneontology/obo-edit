package org.oboedit.gui.components;

import java.awt.GridLayout;
import java.util.Collection;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.ComponentManager;
import org.bbop.framework.GUIComponent;
import org.obo.query.impl.SearchHit;
import org.oboedit.gui.SearchComponentFactory;
import org.oboedit.gui.factory.SearchResultsComponentFactory;

import org.apache.log4j.*;

public class SearchResultsComponent extends AbstractGUIComponent implements
	GUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SearchResultsComponent.class);

	protected SearchComponentFactory factory;

	public SearchResultsComponent(String id) {
		super(id);
	}

	public void setFactory(SearchComponentFactory factory) {
		this.factory = factory;
	}

	public void addNotify() {
		// TODO Auto-generated method stub
		super.addNotify();
		SearchResultsComponentFactory componentFactory = (SearchResultsComponentFactory) ComponentManager
				.getManager().getFactory(this);
		if (componentFactory.alreadyStored(getID())) {
			String title = componentFactory.getTitle(getID());
			ComponentManager.getManager().setLabel(this, title);
		}
	}

	@Override
	public void init() {
		setLayout(new GridLayout(1, 1));
		SearchResultsComponentFactory componentFactory = (SearchResultsComponentFactory) ComponentManager
				.getManager().getFactory(this);
		if (componentFactory.alreadyStored(getID())) {
			removeAll();
			add(new JScrollPane(componentFactory.getComponent(getID()),
					JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
					JScrollPane.HORIZONTAL_SCROLLBAR_NEVER));
			String title = componentFactory.getTitle(getID());
			ComponentManager.getManager().setLabel(this, title);
		}
	}

	public void setResults(Collection<SearchHit<?>> results) {
		JComponent c = factory.getResultsDisplay(results);
		SearchResultsComponentFactory componentFactory = (SearchResultsComponentFactory) ComponentManager
				.getManager().getFactory(this);
		componentFactory.store(getID(), ComponentManager.getManager().getLabel(
				this), c);
		removeAll();
		add(new JScrollPane(c, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER));
		repaint();
	}

}
