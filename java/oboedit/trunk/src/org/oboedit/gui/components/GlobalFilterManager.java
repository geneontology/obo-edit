package org.oboedit.gui.components;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.swing.*;
import org.obo.filters.Filter;
import org.oboedit.gui.*;
import org.oboedit.gui.filter.RenderedFilter;

import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

import org.apache.log4j.*;

public class GlobalFilterManager extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GlobalFilterManager.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected ListEditor linkFilterList;

	protected ListEditor termFilterList;

	protected ListEditor linkSpecFilterList;

	protected ListEditor termSpecFilterList;

	protected JPanel filterPanel = new JPanel();

	protected JPanel rendererPanel = new JPanel();

	protected JTabbedPane tabbedPane = new JTabbedPane();

	protected JLabel noObjectLabel = new JLabel("Click a filter to edit it.");

	protected JButton saveChangesButton = new JButton("Save Changes");

	private class FilterEditor extends JPanel implements GenericEditorComponent {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		protected FilterComponent builder; 

		protected ListEditor editor;

		public void setMasterComponent(Component c) {
			if (c instanceof ListEditor)
				editor = (ListEditor) c;
		}

		public FilterEditor(final boolean linkFilter) {
			setLayout(new BorderLayout());
			if (linkFilter)
				builder = new FilterComponent(
						new LinkFilterEditorFactory());
			else
				builder = new FilterComponent(
						new TermFilterEditorFactory());

			add(builder, "Center");
		}

		public void load(Object o) {
			if (o instanceof Filter)
				builder.setFilter((Filter) o);
		}

		public void store(Object o) {
		}

		public Object createNewValue() {
			builder.clear();
			return builder.getFilter();
		}
	}

	private class RenderEditor extends JPanel implements GenericEditorComponent {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		protected FilterComponent builder;

		protected ListEditor editor;

		public void setMasterComponent(Component c) {
			if (c instanceof ListEditor)
				editor = (ListEditor) c;
		}

		public RenderEditor(final boolean linkFilter) {
			setLayout(new BorderLayout());
			if (linkFilter)
				builder = new FilterComponent(
						new LinkFilterEditorFactory());
			else
				builder = new FilterComponent(
						new TermFilterEditorFactory());
			builder.setButtonVisible(false);
			builder.showRendererControls(true);

			add(builder, "Center");
		}

		public void load(Object o) {
			if (o instanceof RenderedFilter)
				builder.setRenderedFilter((RenderedFilter) o);
		}

		public void store(Object o) {
		}

		public Object createNewValue() {
			builder.clear();
			return builder.getFilter();
		}
	}

	public GlobalFilterManager(String id) {
		super(id);
	}

	@Override
	public String getName() {
		return "Global Filter Plugin";
	}

	@Override
	public void init() {
		setPreferredSize(new Dimension(400, 300));

		setLayout(new BorderLayout());

		filterPanel.removeAll();
		filterPanel.setLayout(new GridLayout(2, 1));

		rendererPanel.removeAll();
		rendererPanel.setLayout(new GridLayout(2, 1));

		linkFilterList = new ListEditor(new FilterEditor(true), noObjectLabel,
				new Vector<Object>(), true, true, true, true, false);

		termFilterList = new ListEditor(new FilterEditor(false), noObjectLabel,
				new Vector<Object>(), true, true, true, true, false);

		linkSpecFilterList = new ListEditor(new RenderEditor(true),
				noObjectLabel, new Vector<Object>(), true, true, true, true, false);

		termSpecFilterList = new ListEditor(new RenderEditor(false),
				noObjectLabel, new Vector<Object>(), true, true, true, true, false);

		Box commitBox = new Box(BoxLayout.X_AXIS);
		commitBox.add(Box.createHorizontalGlue());
		commitBox.add(saveChangesButton);
		commitBox.add(Box.createHorizontalGlue());

		saveChangesButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				saveFilters();
			}
		});

		removeAll();

		TitledBorder termSpecBorder = new TitledBorder("Term renderers");
		termSpecFilterList.setBorder(termSpecBorder);

		JPanel termSpecFilterButtonPanel = new JPanel();
		termSpecFilterButtonPanel.setOpaque(false);
		termSpecFilterButtonPanel.setLayout(new BoxLayout(
				termSpecFilterButtonPanel, BoxLayout.Y_AXIS));

		JPanel termSpecFilterPanel = new JPanel();
		termSpecFilterPanel.setLayout(new BorderLayout());
		termSpecFilterPanel.add(termSpecFilterList, "Center");
		termSpecFilterPanel.add(termSpecFilterButtonPanel, "East");

		rendererPanel.add(termSpecFilterPanel);

		TitledBorder linkSpecBorder = new TitledBorder("Link renderers");
		linkSpecFilterList.setBorder(linkSpecBorder);

		rendererPanel.add(linkSpecFilterList);

		tabbedPane.add(filterPanel, "Filters");
		tabbedPane.add(rendererPanel, "Renderers");

		add(tabbedPane, "Center");
		add(commitBox, "South");
		validate();
	}

	protected void saveFilters() {
		termFilterList.commit();
		linkFilterList.commit();
		linkSpecFilterList.commit();
		termSpecFilterList.commit();
	}
}
