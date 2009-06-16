package org.oboedit.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.GridLayout;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;

import org.bbop.framework.GUIManager;
import org.bbop.swing.MultiIcon;
import org.bbop.swing.SwingUtil;
import org.bbop.swing.dropbox.DropBoxContents;
import org.bbop.swing.dropbox.DropBoxPanel;
import org.bbop.swing.dropbox.DropBoxWrapper;
import org.obo.datamodel.PathCapable;
import org.obo.filters.CompoundFilter;
import org.obo.filters.CompoundFilterImpl;
import org.obo.filters.Filter;
import org.obo.filters.PathCapableFilter;
import org.obo.util.FilterUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.event.GUIUpdateEvent;
import org.oboedit.gui.event.GUIUpdateListener;

import org.apache.log4j.*;

public class SearchPanel extends JPanel {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SearchPanel.class);

	protected DropBoxPanel contentPanel;
	protected SearchComponentFactory factory;
	protected JPanel conditionPanel;
	protected JPanel statusPanel;
	protected JPanel buttonPanel;
	protected Color highlightColor;
	protected JRadioButton allButton = new JRadioButton("Matches all", true);
	protected JRadioButton anyButton = new JRadioButton("Matches any");
	protected JCheckBox lightbulbButton;
	protected GUIUpdateEvent updateEvent = new GUIUpdateEvent(this);
	protected ActionEvent actionEvent = new ActionEvent(this, 0, "commit");
	protected Collection<GUIUpdateListener> updateListeners = new LinkedList<GUIUpdateListener>();
	protected Collection<ActionListener> actionListeners = new LinkedList<ActionListener>();
	protected static final Color[] colorArray = { Color.black, Color.blue,
			Color.green };
	protected static int colorIndex = 0;

	protected JPanel actionPanel;
	protected GUIUpdateListener broadcastUpdateListener = new GUIUpdateListener() {

		public void guiupdated(GUIUpdateEvent e) {
			fireUpdateEvent();
		}

	};
	protected ActionListener broadcastActionListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			fireActionEvent();
		}
	};

	protected SearchComponentFactory searchPanelFactory;
	protected boolean lightbulbsVisible = false;

	public void setActionComponent(JComponent actionComponent) {
		actionPanel.removeAll();
		actionPanel.add(actionComponent);
		validate();
	}

	public void setStatusComponent(JComponent statusComponent) {
		statusPanel.removeAll();
		statusPanel.add(statusComponent);
		validate();
	}

	@Override
	public void validate() {
		super.validate();
	}

	protected static Color getNextColor() {
		int index = colorIndex;
		colorIndex = (colorIndex + 1) % colorArray.length;
		return colorArray[index];
	}

	/*The lightbulb switches on when the currently selected term in the OTE, Graph Editor or Search Results Panel match the corresponding filter.
	This can be useful when multiple filters are in use and the Search Results are designed to show the "Matches all" case and 
	would be interesting to get an indication of "Matches any" terms with the lightbulb swiching on while browsing an ontology.*/
	protected ActionListener lightbulbActionListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			setLightbulbsVisible(lightbulbButton.isSelected());
		}
	};

	protected WrapperPanel getWrapperPanel() {
		return (WrapperPanel) SwingUtilities.getAncestorOfClass(
				WrapperPanel.class, this);
	}

	public SearchPanel(SearchComponentFactory factory) {
		removeAll();
		setHighlightColor(getNextColor());
		this.factory = factory;
		this.searchPanelFactory = new SearchPanelFactory(factory);
		setLayout(new BorderLayout());
		contentPanel = new DropBoxPanel() {

			{
				addHierarchyListener(new HierarchyListener() {
					public void hierarchyChanged(HierarchyEvent e) {
						ensureButtonState();
					}
				});
			}

			@Override
			public void remove(int index) {
				Component comp = getComponent(index);
				super.remove(index);
				List<DropBoxContents> contents = getContents();
				if (contents.size() == 0) {
					SearchPanel sp = (SearchPanel) SwingUtilities
							.getAncestorOfClass(SearchPanel.class, this);
					if (sp != null) {
						SearchPanel match = sp.getOuterSearchPanel();
						if (match != null)
							match.removeInnerSearchPanel(sp);
					}
				}
				ensureButtonState(contents);
				topmostValidate();
			}

			protected void ensureButtonState() {
				ensureButtonState(getContents());
			}

			protected void ensureButtonState(List<DropBoxContents> contents) {

				if (conditionPanel != null)
					conditionPanel.setVisible(contents.size() > 1);

				if (getWrapperPanel() != null)
					getWrapperPanel().updateMatchLabel();

				boolean on = false;
				for (DropBoxContents c : contents) {
					if (c instanceof WrapperPanel) {
						WrapperPanel wp = (WrapperPanel) c;
						wp.ensureButtonState(contents);
						wp.setIsHighlighted(on);
						wp.updateMatchLabel();
						on = !on;
					}
				}
			}

			@Override
			protected void addImpl(Component comp, Object constraints, int index) {

				super.addImpl(comp, constraints, index);
				List<DropBoxContents> contents = getContents();
				ensureButtonState(contents);
				topmostValidate();
			}
		};

		JButton addButton = new JButton(Preferences.loadLibraryIcon("plus.gif"));
		addButton.setToolTipText("Add additional search filter");
		MultiIcon doublePlusIcon = new MultiIcon();
		doublePlusIcon.addIcon(Preferences.loadLibraryIcon("plus.gif"));
		doublePlusIcon.addIcon(Preferences
				.loadLibraryIcon("right_indent_icon.gif"));
		JButton addRecursiveButton = new JButton(doublePlusIcon);
		addButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				add();
			}
		});
		addRecursiveButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addSubSearch();
			}
		});

		ActionListener actionListener = new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				fireUpdateEvent();
			}

		};

		allButton.addActionListener(actionListener);
		anyButton.addActionListener(actionListener);

		conditionPanel = new JPanel();
		conditionPanel
				.setLayout(new BoxLayout(conditionPanel, BoxLayout.X_AXIS));
		conditionPanel.setOpaque(false);
		ButtonGroup bg = new ButtonGroup();
		bg.add(allButton);
		bg.add(anyButton);
		allButton.setOpaque(false);
		anyButton.setOpaque(false);
		conditionPanel.add(allButton);
		conditionPanel.add(Box.createHorizontalStrut(10));
		conditionPanel.add(anyButton);
		conditionPanel.add(Box.createHorizontalGlue());
		conditionPanel.setVisible(false);

		actionPanel = new JPanel();
		actionPanel.setOpaque(false);
		actionPanel.setLayout(new GridLayout(1, 1));

		statusPanel = new JPanel();
		statusPanel.setOpaque(false);
		statusPanel.setLayout(new GridLayout(1, 1));

		lightbulbButton = new JCheckBox("Indicate if selected term matches filter", true);
		lightbulbButton.addActionListener(lightbulbActionListener);

		buttonPanel = new JPanel();
		buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.X_AXIS));
		buttonPanel.add(addButton);
		// buttonPanel.add(addRecursiveButton);
		buttonPanel.add(Box.createHorizontalStrut(20));
		buttonPanel.add(lightbulbButton);
		buttonPanel.add(Box.createHorizontalGlue());
		buttonPanel.setOpaque(false);

		JScrollPane pane = new JScrollPane(contentPanel,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

		JPanel controlPanel = new JPanel();
		controlPanel.setOpaque(false);
		controlPanel.setLayout(new BorderLayout());
		controlPanel.add(buttonPanel, "West");
		controlPanel.add(actionPanel, "Center");

		JPanel southPanel = new JPanel();
		southPanel.setOpaque(false);
		southPanel.setLayout(new BoxLayout(southPanel, BoxLayout.Y_AXIS));
		southPanel.add(statusPanel);
		southPanel.add(controlPanel);

		add(conditionPanel, "North");
		add(pane, "Center");
		add(southPanel, "South");
		setLightbulbsVisible(true);
		add();
	}

	@Override
	public void addNotify() {
		super.addNotify();
		lightbulbButton.setVisible(getOuterSearchPanel() == null);
		topmostValidate();
	}

	public void setLightbulbsVisible(boolean visible) {
		this.lightbulbsVisible = visible;
		for (DropBoxContents panel : contentPanel.getContents()) {
			if (panel instanceof WrapperPanel) {
				WrapperPanel wp = (WrapperPanel) panel;
				wp.setLightbulbsVisible(visible);
			}
		}
		lightbulbButton.removeActionListener(lightbulbActionListener);
		lightbulbButton.setSelected(visible);
		lightbulbButton.addActionListener(lightbulbActionListener);
		topmostValidate();
	}

	public void updateMatchLabels() {
		for (DropBoxContents panel : contentPanel.getContents()) {
			if (panel instanceof WrapperPanel) {
				WrapperPanel wp = (WrapperPanel) panel;
				wp.updateMatchLabel();
			}
		}
	}

	public Filter getFilter() {
		org.obo.filters.CompoundFilter out = new CompoundFilterImpl();
		out.setBooleanOperation(anyButton.isSelected() ? CompoundFilter.OR
				: CompoundFilter.AND);
		for (DropBoxContents c : contentPanel.getContents()) {
			if (c instanceof WrapperPanel) {
				Component wrappedItem = ((WrapperPanel) c).getContents();
				Filter f = getFilter(factory, wrappedItem);
				if (f != null)
					out.addFilter(f);
			}
		}
		if (out.getFilters().size() == 0)
			return null;
		else if (out.getFilters().size() == 1)
			return out.getFilters().get(0);
		else
			return out;
	}

	public void load(File file) {
		try {
			setFilter(FilterUtil.loadFilter(file.toString()));
		} catch (IOException ex) {
			ex.printStackTrace();
		}
	}

	public void load() {
		Filter pair = loadFilter();
		if (pair != null)
			setFilter(pair);
	}

	public void save() {
		save(getFilter());
	}

	public static Filter loadFilter() {
		JFileChooser chooser = new JFileChooser();
		int returnVal = chooser.showOpenDialog(GUIManager.getManager()
				.getFrame());
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			File file = chooser.getSelectedFile();
			try {
				return FilterUtil.loadFilter(file.toString());
			} catch (IOException ex) {
				return null;
			}
		} else
			return null;
	}

	public void load(String filename) {
		load(new File(filename));
	}

	public static void save(Filter filterPair) {
		JFileChooser chooser = new JFileChooser();
		chooser.setDialogTitle("Save this filter");
		int returnVal = chooser.showSaveDialog(GUIManager.getManager()
						       .getFrame());
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			File file = chooser.getSelectedFile();
			FilterUtil.save(file.toString(), filterPair);
		}
	}

	protected SearchPanel addSubSearch() {
		return addSubSearch(-1);
	}

	protected SearchPanel addSubSearch(int index) {
		WrapperPanel c = createWrapperPanel(searchPanelFactory,
				new TitledBorder("Subquery"));
		SearchPanel p = (SearchPanel) c.getContents();
		p.addUpdateListener(broadcastUpdateListener);
		p.setOpaque(false);
		contentPanel.add(c, index);
		c.setLightbulbsVisible(lightbulbsVisible);
		topmostValidate();
		return p;
	}

	@Override
	public void setName(String name) {
		super.setName(name);
	}

	public JComponent add() {
		WrapperPanel c = createWrapperPanel(factory, null);
		factory.addUpdateListener(c.getContents(), broadcastUpdateListener);
		factory.addActionListener(c.getContents(), broadcastActionListener);
		contentPanel.add(c);
		topmostValidate();
		fireUpdateEvent();
		return (JComponent) c.getContents();
	}

	public void setFilter(Filter<?> filter) {
		contentPanel.removeAll();
		if (filter == null) {
			return;
		} else if (filter instanceof CompoundFilter) {
			CompoundFilter cf = (CompoundFilter) filter;
			if (cf.getBooleanOperation() == CompoundFilter.AND)
				allButton.setSelected(true);
			else if (cf.getBooleanOperation() == CompoundFilter.OR)
				anyButton.setSelected(true);
			for (Filter<?> f : cf.getFilters()) {
				JComponent c;
				if (f instanceof CompoundFilter) {
					c = addSubSearch();
					searchPanelFactory.setFilter(c, f);
				} else {
					c = add();
					factory.setFilter(c, f);
				}
			}
		} else {
			JComponent c = add();
			factory.setFilter(c, filter);
		}
	}

	public SearchPanel getTopmostSearchPanel() {
		SearchPanel panel = this;
		while (panel.getOuterSearchPanel() != null) {
			panel = panel.getOuterSearchPanel();
		}
		return panel;
	}

	public void clear() {
		contentPanel.removeAll();
		add();
	}

	public void topmostValidate() {
		SearchPanel p = getTopmostSearchPanel();
		if (p != null) {
			p.validate();
			p.repaint();
		} else {
			validate();
			repaint();
		}
	}

	public SearchPanel getOuterSearchPanel() {
		return (SearchPanel) SwingUtilities.getAncestorOfClass(
				SearchPanel.class, this);
	}

	public void removeInnerSearchPanel(SearchPanel panel) {
		List<DropBoxContents> contents = contentPanel.getContents();
		for (DropBoxContents c : contents) {
			if (c instanceof DropBoxWrapper) {
				DropBoxWrapper dbw = (DropBoxWrapper) c;
				if (SwingUtilities.isDescendingFrom(panel, dbw)) {
					contentPanel.remove(dbw);
					topmostValidate();
					return;
				}
			}
		}
	}

	public static Filter getFilter(SearchComponentFactory factory,
			Component wrappedItem) {
		if (wrappedItem instanceof SearchPanel) {
			return ((SearchPanel) wrappedItem).getFilter();
		} else if (wrappedItem instanceof JComponent) {
			return factory.getFilter((JComponent) wrappedItem);
		} else
			return null;
	}

	protected class WrapperPanel extends DropBoxWrapper {
		protected JButton leftButton = new JButton(Preferences
				.loadLibraryIcon("left_indent_icon.gif"));
		protected JButton rightButton = new JButton(Preferences
				.loadLibraryIcon("right_indent_icon.gif"));
		protected JButton removeButton = new JButton(Preferences
				.loadLibraryIcon("minus.gif"));
		protected JLabel matchLabel;
//		protected JSeparator sep = new JSeparator(SwingConstants.VERTICAL);

		protected JComponent component;
		protected boolean highlighted;
		protected Color highlightColor;

		public boolean isSubQuery() {
			return component instanceof SearchPanel;
		}

		public SearchPanel getSearchPanel() {
			return (SearchPanel) SwingUtilities.getAncestorOfClass(
					SearchPanel.class, this);
		}

		protected void updateMatchLabel() {
			SearchPanel panel = getSearchPanel();
			if (panel != null) {
				SearchComponentFactory factory = panel.factory;
				Filter f = getFilter(factory, component);
				if (f instanceof PathCapableFilter
						&& SessionManager.getManager().getUseReasoner()) {
					((PathCapableFilter) f)
							.setReasoner(SessionManager
									.getManager().getReasoner());
				}
				boolean matches = false;
				if (f != null) {
					Collection<PathCapable> pcs = factory
							.getRelevantValues(SelectionManager.getManager()
									.getSelection().getAllSelectedObjects());
					matches = pcs.size() > 0;
					for (PathCapable pc : pcs) {
						try {
							if (!f.satisfies(pc)) {
								matches = false;
								break;
							}
						} catch (Throwable t) {
							matches = false;
							break;
						}
					}
				}
				if (matches)
					matchLabel.setIcon(Preferences
							.loadLibraryIcon("bulb_on.png"));
				else
					matchLabel.setIcon(Preferences
							.loadLibraryIcon("bulb_off.png"));
				if (component instanceof SearchPanel) {
					((SearchPanel) component).updateMatchLabels();
				}
			}
		}

		@Override
		public Component getContents() {
			return component;
		}

		public void setIsHighlighted(boolean highlighted) {
			this.highlighted = highlighted;
		}

		public WrapperPanel(SearchComponentFactory factory, Border border) {
			super();
			matchLabel = new JLabel(Preferences.loadLibraryIcon("bulb_off.png")) {
				@Override
				public String getToolTipText() {
					String selectionWord = null;
					if (SelectionManager.getGlobalSelection()
							.getTermSubSelection() != null)
						selectionWord = SelectionManager.getGlobalSelection()
								.getTermSubSelection().getName();
					return "<html>Indicates whether the currently selected term "
							+ (selectionWord == null ? "" : "\""
									+ selectionWord + "\"")
							+ "<br>matches this part of the filter</html>";
				}
			};
			ToolTipManager.sharedInstance().registerComponent(matchLabel);
			matchLabel.setToolTipText("never visible");

			setHighlightColor(new Color(0, 0, 255, 50));
			component = factory.createSubEditor();
			factory.addUpdateListener(component, new GUIUpdateListener() {
				public void guiupdated(GUIUpdateEvent e) {
				    // Is this really necessary?  It gets updated anyway.
//					updateMatchLabel();
				}
			});
			JPanel p = new JPanel();
			p.setOpaque(false);
			setContents(p);
			p.setLayout(new BorderLayout());
			p.add(matchLabel, "West");
			p.add(component, "Center");
			if (border != null)
				p.setBorder(border);
			setMaximumSize(new Dimension(Integer.MAX_VALUE, Math.max(30,
					(int) component.getPreferredSize().getHeight())));
			DropBoxWrapper.DragButton button = new DropBoxWrapper.DragButton(
					"drag");
			button.setBackground(Preferences.defaultButtonColor());
			setDragHandle(button);

			JPanel buttonPanel = new JPanel();
			buttonPanel.setOpaque(false);
//			buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.Y_AXIS));
			 buttonPanel.setLayout(new FlowLayout());
			Box buttonBox = new Box(BoxLayout.X_AXIS);
//			buttonBox.add(sep);
			buttonBox.add(buttonPanel);
//			p.add(buttonBox, "East");
			p.add(buttonBox, "South");
			leftButton.setOpaque(false);
			rightButton.setOpaque(false);
			removeButton.setOpaque(false);

			removeButton.setToolTipText("Remove this filter");
			leftButton.setToolTipText("Move filter up a level");
			rightButton.setToolTipText("Move filter down a level");

			rightButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					SearchPanel myPanel = getSearchPanel();
					int index = SwingUtil.getIndex(myPanel.contentPanel,
							WrapperPanel.this);
					myPanel.contentPanel.remove(WrapperPanel.this);
					SearchPanel newPanel = myPanel.addSubSearch(index);
					SearchPanel newParentPanel = newPanel.getOuterSearchPanel();
					List<DropBoxContents> contents = newPanel.contentPanel
							.getContents();
					newPanel.contentPanel.add(WrapperPanel.this);
					for (DropBoxContents c : contents) {
						newPanel.contentPanel.remove((Component) c);
					}
					topmostValidate();
					fireUpdateEvent();
				}
			});

			leftButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					SearchPanel myPanel = getSearchPanel();
					Container outerPanel = myPanel.getParent();
					SearchPanel outerSearchPanel = myPanel
							.getOuterSearchPanel();

					int index = SwingUtil.getIndex(
							outerSearchPanel.contentPanel, outerPanel);
					myPanel.contentPanel.remove(WrapperPanel.this);
					outerSearchPanel.contentPanel.add(WrapperPanel.this, index);
					topmostValidate();
					fireUpdateEvent();
				}
			});

			removeButton.setVisible(!isSubQuery());
			rightButton.setVisible(!isSubQuery());
			removeButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					Container parent = getParent();
					parent.remove(WrapperPanel.this);
					topmostValidate();
					fireUpdateEvent();
				}
			});
			buttonPanel.add(leftButton);
			buttonPanel.add(rightButton);
			buttonPanel.add(removeButton);
			updateMatchLabel();
		}

		public void setHighlightColor(Color c) {
			highlightColor = c;
		}

		@Override
		protected void paintComponent(Graphics g) {
			super.paintComponent(g);
			if (highlighted) {
				g.setColor(highlightColor);
				Rectangle r = g.getClipBounds();
				g.fillRect(r.x, r.y, r.width, r.height);
			}
		}

		protected void ensureButtonState(List<DropBoxContents> contents) {
			rightButton.setVisible(!isSubQuery() && contents.size() > 1);
			SearchPanel searchPanel = getSearchPanel();
			final boolean hasOuterSearchPanel = searchPanel != null
					&& searchPanel.getOuterSearchPanel() != null;
			leftButton.setVisible(!isSubQuery() && hasOuterSearchPanel);
			removeButton.setVisible(!isSubQuery()
					&& (contents.size() > 1 || hasOuterSearchPanel));
//			sep.setVisible(rightButton.isVisible() || leftButton.isVisible()
//					|| removeButton.isVisible());
		}

		public void setLightbulbsVisible(boolean visible) {
			matchLabel.setVisible(visible);
			if (component instanceof SearchPanel) {
				((SearchPanel) component).setLightbulbsVisible(visible);
			}
			validate();
		}

	}

	protected WrapperPanel createWrapperPanel(SearchComponentFactory factory,
			Border border) {
		WrapperPanel out = new WrapperPanel(factory, border);
		out.setLightbulbsVisible(lightbulbsVisible);
		Color c = getHighlightColor();
		c = new Color(c.getRed(), c.getGreen(), c.getBlue(), 50);
		out.setHighlightColor(c);
		return out;
	}

	public Color getHighlightColor() {
		return highlightColor;
	}

	public void setHighlightColor(Color highlightColor) {
		this.highlightColor = highlightColor;
	}

	protected void fireUpdateEvent() {
		for (GUIUpdateListener listener : updateListeners) {
			listener.guiupdated(updateEvent);
		}
	}

	protected void fireActionEvent() {
		for (ActionListener listener : actionListeners) {
			listener.actionPerformed(actionEvent);
		}
	}

	public void addActionListener(ActionListener listener) {
		actionListeners.add(listener);
	}

	public void removeActionListener(ActionListener listener) {
		actionListeners.remove(listener);
	}

	public void addUpdateListener(GUIUpdateListener listener) {
		updateListeners.add(listener);
	}

	public void removeUpdateListener(GUIUpdateListener listener) {
		updateListeners.remove(listener);
	}
}
