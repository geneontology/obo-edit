package org.oboedit.gui.widget;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;
import java.util.*;

import org.obo.filters.*;
import org.oboedit.gui.DefaultFilterEditorFactory;
import org.oboedit.util.GUIUtil;

public class FilterBuilder extends AbstractFilterEditor implements CompoundEditable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 5242327828890927731L;

	protected static class FilterWrapper {
		protected Filter filter;

		public FilterWrapper(Filter filter) {
			this.filter = filter;
		}

		public Filter getFilter() {
			return filter;
		}

		@Override
		public String toString() {
			return filter.toString();
		}
	}

	protected CompoundFilter filter = new CompoundFilterImpl();
	protected FilterEditor currentEditor = null;
	protected JTree filterTree = new JTree();
	protected JPanel editorPanel = new JPanel();
	protected JScrollPane treeScroller = new JScrollPane(filterTree,
			JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

	JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true,
			treeScroller, editorPanel);

	protected FilterFactory compoundFilterFactory = new CompoundFilterFactory();
	protected FilterFactory filterFactory = new ObjectFilterFactory();
	protected FilterEditorFactory filterEditorFactory = new DefaultFilterEditorFactory();
	protected FilterTreeModel filterTreeModel = new FilterTreeModel();
	protected JLabel filterLabel = new JLabel("<just some label!>");

	protected Color buttonColor;
	protected JPopupMenu rightClickMenu = new JPopupMenu();

	protected boolean showCompoundFilter = false;

	protected JMenuItem addItem = new JMenuItem("Add Filter");
	protected JMenuItem addCompoundItem = new JMenuItem("Add Boolean Filter");
	protected JMenuItem delItem = new JMenuItem("Delete");

	protected static int idgen = 0;
	protected int id = idgen++;

	protected TreeSelectionListener treeListener = new TreeSelectionListener() {
		public void valueChanged(TreeSelectionEvent e) {
			updateTreeSelection();
		}
	};

	protected TreeCellRenderer compoundRenderer = new DefaultTreeCellRenderer() {
		/**
		 * 
		 */
		private static final long serialVersionUID = -4246196502391764806L;

		@Override
		public Component getTreeCellRendererComponent(JTree tree, Object value,
				boolean sel, boolean expanded, boolean leaf, int row,
				boolean hasFocus) {
			if (value instanceof FilterWrapper)
				value = ((FilterWrapper) value).getFilter();

			String label = value.toString();
			if (value instanceof CompoundFilter) {
				int op = ((CompoundFilter) value).getBooleanOperation();
				if (op == CompoundFilter.AND)
					label = "AND";
				else if (op == CompoundFilter.OR)
					label = "OR";
				else
					label = "??";
			}
			return super.getTreeCellRendererComponent(tree, label, sel,
					expanded, leaf, row, hasFocus);
		}
	};
	protected FilterEditUpdateListener updateListener = new FilterEditUpdateListener() {
		@Override
		public void update() {
			filterTree.repaint();
		}
	};

	@Override
	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);
		filterTree.setEnabled(enabled);
		if (currentEditor != null)
			((JComponent) currentEditor).setEnabled(enabled);
	}

	public void setCompoundFilterFactory(FilterFactory compoundFilterFactory) {
		this.compoundFilterFactory = compoundFilterFactory;
		createNewFilter();
	}

	public void setFilterFactory(FilterFactory filterFactory) {
		this.filterFactory = filterFactory;
		createNewFilter();
	}

	public void createNewFilter() {
		CompoundFilter cfilter = new CompoundFilterImpl();
		Filter filter = filterFactory.createNewFilter();
		Object[] os = { new FilterWrapper(cfilter), new FilterWrapper(filter) };
		TreePath path = new TreePath(os);

		cfilter.addFilter(filter);
		this.filter = cfilter;
		filterTreeModel.reload();
		filterTree.setSelectionPath(path);
		refresh();
	}

	public void setFilterEditorFactory(FilterEditorFactory filterEditorFactory) {
		this.filterEditorFactory = filterEditorFactory;
	}

	protected class FilterTreeModel implements TreeModel {

		protected java.util.List listeners = new ArrayList();

		public void addTreeModelListener(TreeModelListener l) {
			listeners.add(l);
		}

		public void removeTreeModelListener(TreeModelListener l) {
			listeners.remove(l);
		}

		public void reload() {
			fireTreeStructureChanged(new TreeModelEvent(this, new TreePath(
					new FilterWrapper(filter))));
		}

		protected void fireTreeStructureChanged(TreeModelEvent e) {
			for (int i = 0; i < listeners.size(); i++) {
				TreeModelListener tml = (TreeModelListener) listeners.get(i);
				tml.treeStructureChanged(e);
			}
		}

		public void valueForPathChanged(TreePath path, Object newValue) {
		}

		public Object getChild(Object parentWrapper, int index) {
			Filter parent = ((FilterWrapper) parentWrapper).getFilter();
			if (parent instanceof CompoundFilter) {
				return new FilterWrapper((Filter) ((CompoundFilter) parent)
						.getFilters().get(index));
			} else
				return null;
		}

		public int getChildCount(Object parentWrapper) {
			Filter parent = ((FilterWrapper) parentWrapper).getFilter();
			if (parent instanceof CompoundFilter) {
				CompoundFilter cf = (CompoundFilter) parent;
				return cf.getFilters().size();
			} else
				return 0;
		}

		public int getIndexOfChild(Object parentWrapper, Object child) {
			Filter parent = ((FilterWrapper) parentWrapper).getFilter();
			if (parent instanceof CompoundFilter) {
				return ((CompoundFilter) parent).getFilters().indexOf(child);
			} else
				return -1;
		}

		public Object getRoot() {
			return new FilterWrapper(filter);
		}

		public boolean isLeaf(Object node) {
			return !(((FilterWrapper) node).getFilter() instanceof CompoundFilter);
		}
	}

	public void setFilter(Filter filter) {
		if (filter != null) {
			this.filter = (CompoundFilter) filter;
			updateGUI();
		} else
			createNewFilter();
		refresh();
		doPathSelect();
	}

	protected void updateGUI() {
		filterTree.removeTreeSelectionListener(treeListener);
		filterTreeModel.reload();
		filterTree.setSelectionRow(0);
		filterTree.addTreeSelectionListener(treeListener);
	}

	public Filter getFilter() {
		return filter;
	}

	public void acceptEdits() {
		if (currentEditor != null)
			currentEditor.acceptEdits();
	}

	protected void updateTreeSelection() {
		if (currentEditor != null)
			currentEditor.acceptEdits();

		editorPanel.removeAll();
		Filter filter = getSelectedFilter();

		if (filter != null) {
			if (currentEditor != null)
				currentEditor.removeFilterEditUpdateListener(updateListener);
			currentEditor = filterEditorFactory.getFilterEditor(filter);
			currentEditor.addFilterEditUpdateListener(updateListener );
			((Component) currentEditor).setFont(getFont());
			(currentEditor).setButtonColor(buttonColor);
		} else
			currentEditor = null;

		if (currentEditor != null) {
			currentEditor.setFilter(filter);
			editorPanel.add((JComponent) currentEditor);
		}
		editorPanel.validate();
		editorPanel.repaint();
		updateLabel();
	}

	public void addActionListener(ActionListener listener) {
		currentEditor.addActionListener(listener);
	}

	public void removeActionListener(ActionListener listener) {
		currentEditor.removeActionListener(listener);
	}

	@Override
	public void setFont(Font font) {
		super.setFont(font);
		if (rightClickMenu != null)
			rightClickMenu.setFont(font);
		if (addItem != null)
			addItem.setFont(font);
		if (delItem != null)
			delItem.setFont(font);
		if (addCompoundItem != null)
			addCompoundItem.setFont(font);
		if (filterTree != null)
			filterTree.setFont(font);
		if (rightClickMenu != null)
			rightClickMenu.setFont(font);
		if (filterLabel != null)
			filterLabel.setFont(font);
	}

	public void setButtonColor(Color buttonColor) {
		this.buttonColor = buttonColor;
		if (currentEditor != null)
			currentEditor.setButtonColor(buttonColor);
	}

	public void addItem() {
		Filter filter = getSelectedFilter();
		CompoundFilter cf = getSelectedFilterParent();
		if (filter instanceof CompoundFilter) {
			cf = (CompoundFilter) filter;
		}
		System.err.println("Adding filter to cf = " + cf);
		if (cf != null) {
			cf.addFilter(filterFactory.createNewFilter());
			refresh();
		}
	}

	public void addBooleanItem() {
		Filter filter = getSelectedFilter();
		CompoundFilter cf = getSelectedFilterParent();
		if (filter instanceof CompoundFilter) {
			cf = (CompoundFilter) filter;
		}
		if (cf != null) {
			cf.addFilter(compoundFilterFactory.createNewFilter());
			refresh();
		}
	}

	public void delItem() {
		Filter filter = getSelectedFilter();
		CompoundFilter parentFilter = getSelectedFilterParent();

		if (parentFilter != null && filter != null) {
			parentFilter.removeFilter(filter);
			refresh();
		}
	}

	protected void refresh() {
		filterTree.removeTreeSelectionListener(treeListener);
		filterTreeModel.reload();
		updateLabel();
		filterTree.addTreeSelectionListener(treeListener);
	}

	protected Filter getSelectedFilter() {
		TreePath path = filterTree.getSelectionPath();
		if (path == null)
			return null;
		if (!(path.getLastPathComponent() instanceof FilterWrapper))
			System.err.println("!! BAD NEWS, path is " + path + " of class "
					+ path.getLastPathComponent().getClass());
		FilterWrapper fw = (FilterWrapper) path.getLastPathComponent();
		return fw.getFilter();
	}

	protected CompoundFilter getSelectedFilterParent() {
		TreePath path = filterTree.getSelectionPath();
		if (path == null)
			return null;
		TreePath parentPath = path.getParentPath();
		if (parentPath == null)
			return null;
		CompoundFilter parentFilter = null;
		FilterWrapper fw = (FilterWrapper) parentPath.getLastPathComponent();
		if (fw.getFilter() instanceof CompoundFilter) {
			parentFilter = (CompoundFilter) fw.getFilter();
		}
		return parentFilter;
	}

	protected void updateLabel() {
		filterLabel.setText(filterTree.getModel().getRoot().toString());
	}

	protected void formatPopup() {
		addItem.setEnabled(getSelectedFilter() instanceof CompoundFilter);
		addCompoundItem
				.setEnabled(getSelectedFilter() instanceof CompoundFilter);
		delItem.setEnabled(getSelectedFilterParent() != null);
	}

	protected TreePath getFirstValidPath(Filter filter) {
		if (filter instanceof CompoundFilter) {
			CompoundFilter cf = (CompoundFilter) filter;
			if (cf.getFilters().size() == 0)
				return new TreePath(new FilterWrapper(cf));
			else {
				Filter child = (Filter) cf.getFilters().iterator().next();
				TreePath childPath = getFirstValidPath(child);
				Object[] os = childPath.getPath();
				TreePath out = new TreePath(new FilterWrapper(cf));
				for (int i = 0; i < os.length; i++)
					out = out.pathByAddingChild(os[i]);
				return out;
			}
		} else
			return new TreePath(new FilterWrapper(filter));
	}

	protected void doPathSelect() {
		TreePath selectPath = getFirstValidPath(filter);
		Object[] os = selectPath.getPath();
		filterTree.setSelectionPath(selectPath);
	}

	public void setShowCompoundFilter(boolean showCompoundFilter) {
		if (this.showCompoundFilter != showCompoundFilter) {
			removeAll();

			if (this.showCompoundFilter) {
				splitPane.remove(editorPanel);
				add(editorPanel, "Center");
				remove(filterLabel);

				// If we're switching out of compound filter mode
				// we need to convert the compound filter into something
				// useable. So:
				// Search the compound filter for a non-compound subfilter.
				// If a non-compound subfilter is found, delete everything
				// else.
				// If no non-compound subfilter is found, clear out
				// the current filter entirely
				CompoundFilter cfilter = filter;
				if (cfilter.getFilters().size() > 0) {
					Filter keeper = null;
					Iterator it = cfilter.getFilters().iterator();
					while (it.hasNext()) {
						Filter filter = (Filter) it.next();
						if (!(filter instanceof CompoundFilter)) {
							keeper = filter;
							break;
						}
					}
					if (keeper != null) {
						cfilter.getFilters().clear();
						cfilter.addFilter(keeper);
					} else
						createNewFilter();
				}
			} else {
				splitPane.setRightComponent(editorPanel);
				add(splitPane, "Center");
				add(filterLabel, "South");
			}
			refresh();
			doPathSelect();

			validate();
			repaint();
		}
		this.showCompoundFilter = showCompoundFilter;
	}

	public FilterBuilder() {
		setLayout(new BorderLayout());
		filterTree.setMinimumSize(new Dimension(150, 150));
		treeScroller.setMinimumSize(new Dimension(150, 150));
		setOpaque(false);
		splitPane.setOpaque(false);
		// splitPane.
		splitPane.setDividerSize(5);
		editorPanel.setOpaque(false);
		editorPanel.setLayout(new BorderLayout());
		// editorPanel.setPreferredSize(new Dimension(200,200));

		rightClickMenu.add(addItem);
		rightClickMenu.add(addCompoundItem);
		rightClickMenu.addSeparator();
		rightClickMenu.add(delItem);

		filterTree.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseReleased(MouseEvent e) {
				if (GUIUtil.isPopupTrigger(e)) {
					formatPopup();
					rightClickMenu.show(filterTree, e.getX(), e.getY());
				}
			}
		});

		addItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addItem();
			}
		});
		addCompoundItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addBooleanItem();
			}
		});
		delItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				delItem();
			}
		});

		filterTree.setModel(filterTreeModel);
		filterTree.setCellRenderer(compoundRenderer);
		filterTree.getSelectionModel().setSelectionMode(
				TreeSelectionModel.SINGLE_TREE_SELECTION);
		filterTree.addTreeSelectionListener(treeListener);

		add(editorPanel, "Center");
		createNewFilter();
	}

	@Override
	public String toString() {
		return "FilterBuilder " + id;
	}
}
