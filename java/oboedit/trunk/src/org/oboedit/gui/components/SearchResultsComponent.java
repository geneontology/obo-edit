package org.oboedit.gui.components;

import java.awt.Component;
import java.awt.GridLayout;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.plaf.basic.BasicTableUI;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.ComponentManager;
import org.bbop.framework.GUIComponent;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.PathCapable;
import org.obo.query.impl.SearchHit;
import org.oboedit.controller.SelectionManager;
import org.oboedit.gui.AbstractSearchResultsTableModel;
import org.oboedit.gui.SearchComponentFactory;
import org.oboedit.gui.SearchResultsTableModel;
import org.oboedit.gui.Selection;
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
