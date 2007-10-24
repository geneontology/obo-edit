package org.oboedit.gui.components;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.EventObject;
import java.util.LinkedList;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.AbstractCellEditor;
import javax.swing.CellEditor;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.KeyStroke;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.border.EmptyBorder;
import javax.swing.event.CellEditorListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelListener;
import javax.swing.plaf.TableUI;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableModel;

import org.bbop.expression.ExpressionUtil;
import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.ComponentConfiguration;
import org.bbop.swing.MultiheightTable;
import org.bbop.swing.SwingUtil;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.impl.DanglingObjectImpl;
import org.obo.datamodel.impl.OBOClassImpl;
import org.obo.history.HistoryItem;
import org.obo.history.HistoryList;
import org.obo.history.TermMacroHistoryItem;
import org.obo.postcomp.PostcompUtil;
import org.obo.query.QueryEngine;
import org.obo.query.impl.NamespaceQuery;
import org.obo.util.ReasonerUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.OBOTextEditComponent;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.event.HistoryAppliedEvent;
import org.oboedit.gui.event.HistoryListener;
import org.oboedit.gui.event.RootChangeEvent;
import org.oboedit.gui.event.RootChangeListener;
import org.oboedit.gui.widget.IntersectionPanel;
import org.oboedit.gui.widget.SessionAutocompleteBox;
import org.oboedit.util.GUIUtil;

public class IntersectionEditor extends AbstractGUIComponent {

	protected static final String JUNK_ID = "_if_you_can_read_this_something_is_wrong_";

	protected RootChangeListener rootListener = new RootChangeListener() {
		public void changeRoot(RootChangeEvent e) {
			reset();
		}
	};

	protected HistoryListener historyListener = new HistoryListener() {

		public void applied(HistoryAppliedEvent event) {
			reset();
		}

		public void reversed(HistoryAppliedEvent event) {
			reset();
		}
	};

	protected DefaultTableModel tableModel = new DefaultTableModel();

	protected class IntersectionTable extends JTable {

		@Override
		public void setUI(TableUI ui) {
			super.setUI(ui);

		}

		@Override
		public Component prepareEditor(TableCellEditor editor, int row,
				int column) {
			Component out = super.prepareEditor(editor, row, column);
			int requiredHeight = out.getPreferredSize().height * 2;
			if (requiredHeight != getRowHeight(row)) {
				setRowHeight(row, requiredHeight);
			}
			intersectionTable.scrollRectToVisible(intersectionTable
					.getCellRect(row, 0, true));
			return out;
		}

		@Override
		public void removeEditor() {
			if (!completingEdit) {
				tableCellEditor.notifyCancel();
			}
			super.removeEditor();
		}

		@Override
		public void setCellEditor(TableCellEditor anEditor) {
			super.setCellEditor(anEditor);
			if (anEditor instanceof IntersectionTableEditor)
				((IntersectionTableEditor) anEditor).notifyActive();
		}
	}

	protected IntersectionTable intersectionTable = new IntersectionTable();

	protected class IntersectionTableEditor extends AbstractCellEditor
			implements TableCellEditor {

		public IntersectionTableEditor() {
			editorPanel.addHierarchyListener(new HierarchyListener() {

				public void hierarchyChanged(HierarchyEvent e) {
					editorPanel.requestFocus();
				}

			});
			editorPanel.init();
		}

		public void notifyCancel() {
			boolean isCreation = editorPanel.getObject().getID()
					.equals(JUNK_ID);
			if (isCreation) {
				int editorRow = intersectionTable.getEditingRow();
				tableModel.removeRow(editorRow);
			}
		}

		public void notifyActive() {
			editorPanel.requestFocus();
		}

		@Override
		public boolean isCellEditable(EventObject e) {
			if (e == null || e instanceof MouseEvent
					&& ((MouseEvent) e).getClickCount() == 2)
				return super.isCellEditable(e);
			else
				return false;
		}

		public Component getTableCellEditorComponent(JTable table,
				Object value, boolean isSelected, int row, int column) {
			if (value instanceof LinkedObject) {
				LinkedObject io = (LinkedObject) value;
				for (Link link : io.getParents()) {
					if (TermUtil.isDangling(link))
						return null;
				}
				System.err.println("setting object to " + value);
				editorPanel.setCreateNewObject(io.getID().equals(JUNK_ID));
				editorPanel.setObject(io);
				return editorPanel;
			}
			return null;
		}

		@Override
		public boolean stopCellEditing() {
			completingEdit = true;
			flushEdits();
			completingEdit = false;
			return super.stopCellEditing();
		}

		@Override
		public void cancelCellEditing() {
			super.cancelCellEditing();
		}

		public Object getCellEditorValue() {
			return editorPanel.getObject();
		}

	}

	protected class IntersectionTableRenderer extends DefaultTableCellRenderer {
		@Override
		public Component getTableCellRendererComponent(JTable table,
				Object value, boolean isSelected, boolean hasFocus, int row,
				int column) {
			JLabel out = (JLabel) super.getTableCellRendererComponent(table,
					value, isSelected, hasFocus, row, column);
			if (value instanceof LinkedObject)
				configureLabel(out, (LinkedObject) value, row, isSelected);
			if (out.getPreferredSize().height != table.getRowHeight(row)) {
				table.setRowHeight(row, out.getPreferredSize().height);
			}
			return out;
		}
	}

	protected void configureLabel(JLabel out, LinkedObject lo, int index,
			boolean isSelected) {
		out.setOpaque(true);
		out.setBorder(new EmptyBorder(10, 10, 10, 10));
		out.setMinimumSize(new Dimension(intersectionTable.getWidth(), 0));
		if (!isSelected) {
			if (index % 2 == 0)
				out.setBackground(Preferences.getPreferences()
						.getBackgroundColor());
			else
				out.setBackground(Color.white);
		}
		Collection<OBOClass> genae = ReasonerUtil.getGenae((OBOClass) lo);
		Collection<Link> differentia = ReasonerUtil
				.getDifferentia((OBOClass) lo);
		String s = "<html>\n"
				+ "<table cellspacing=0 cellpadding=0 valign=top align=left>\n"
				+ (TermUtil.hasDanglingIntersections(lo) ? "<tr><td colspan=3><font color='red'><b>Warning: This intersection contains dangling references in its definition. It cannot be edited and will be ignored by the reasoner.</b></font></td>"
						: "") + "<tr><td><b>id:</b></td><td rowspan="
				+ (4 + genae.size()) + " width=20>&nbsp;</td><td>" + lo.getID()
				+ "</td>\n" + "<tr><td><b>name:</b></td><td>" + lo.getName()
				+ "<br><br></td>\n";
		for (OBOClass genus : genae) {
			s += "<tr><td><b>genus:</b></td><td>" + genus + "</td>\n";
		}
		s += "<tr><td><b>differentia:</b></td>"
				+ "<td style='padding-left: 10px'>";
		for (Link link : differentia) {
			s += "<i>" + link.getType().getName() + "</i> " + link.getParent()
					+ "<br>";
		}
		s += "<br></td>\n";
		s += "<tr><td><b>exp:</b></td><td>"
				+ PostcompUtil.getNameExpression(lo, true) + "</td>";
		s += "</table>\n</html>";

		/*
		 * String s = "<html>" + "<b>id: </b>" + lo.getID() + "<br>\n" + "<b>name:
		 * </b>" + lo.getName() + "<br>\n" + "<b>exp: </b>" +
		 * PostcompUtil.getNameExpression(lo, true) + "<br>\n" + "</html>";
		 */
		out.setText(s);
	}

	protected IntersectionPanel editorPanel = new IntersectionPanel(true);

	protected IntersectionTableRenderer tableCellRenderer = new IntersectionTableRenderer();

	protected IntersectionTableEditor tableCellEditor = new IntersectionTableEditor();

	protected JCheckBox autoselectCheckbox = new JCheckBox(
			"Link this selection with main selection", false);

	protected JCheckBox keywordSearchCheckbox = new JCheckBox(
			"Use keyword search method", false);

	protected SessionAutocompleteBox termBox;

	protected QueryEngine queryEngine;

	public IntersectionEditor(String id) {
		super(id);

		editorPanel.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				editNext();
			}
		});
		intersectionTable.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(
				KeyStroke.getKeyStroke(KeyEvent.VK_N, Toolkit.getDefaultToolkit()
						.getMenuShortcutKeyMask()),
				"createNew");
		intersectionTable.getActionMap().put("createNew", new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				createNewRow();
			}
		});
		intersectionTable.setDefaultRenderer(Object.class, tableCellRenderer);
		intersectionTable.setDefaultEditor(Object.class, tableCellEditor);
		intersectionTable.setModel(tableModel);
		intersectionTable.getSelectionModel().addListSelectionListener(
				new ListSelectionListener() {
					public void valueChanged(ListSelectionEvent e) {
						if (!autoselectCheckbox.isSelected())
							return;
						forwardLocalSelectionToGlobal();
					}
				});
	}

	protected void forwardLocalSelectionToGlobal() {
		IdentifiedObject io = (IdentifiedObject) intersectionTable.getValueAt(
				intersectionTable.getSelectedRow(), 0);
		SelectionManager.selectTerm(IntersectionEditor.this, (LinkedObject) io);
	}

	protected boolean completingEdit = false;

	protected void editNext() {
		completingEdit = true;
		int row = intersectionTable.getEditingRow();
		flushEdits();
		editNext(row);
		completingEdit = false;
	}

	protected boolean flushEdits() {
		int row = intersectionTable.getEditingRow();
		boolean isCreation = editorPanel.getObject().getID().equals(JUNK_ID);
		String newID = editorPanel.getCurrentID();
		List<HistoryItem> pendingEdits = editorPanel.getChanges();
		if (pendingEdits != null && pendingEdits.size() > 0) {
			if (isCreation) {
				intersectionTable.removeEditor();
				tableModel.removeRow(row);
			}
			SessionManager.getManager().removeHistoryListener(historyListener);
			TermMacroHistoryItem item = new TermMacroHistoryItem(
					"Intersection definition changes", pendingEdits);
			pendingEdits = null;
			HistoryList historyList = item.forwardID(JUNK_ID, Collections
					.singletonList(editorPanel.getCurrentID()));
			if (historyList != null) {
				item = new TermMacroHistoryItem(
						"Intersection definition changes", historyList);
			}
			if (isCreation) {
				LinkedObject io = new DanglingObjectImpl(newID);
				GUIUtil.setPostSelection(item, SelectionManager
						.createSelectionFromTerms(IntersectionEditor.this,
								Collections.singleton(io), io, false));
			}
			SessionManager.getManager().apply(item);
			SessionManager.getManager().addHistoryListener(historyListener);
			if (isCreation) {
				OBOClass newItem = (OBOClass) SessionManager.getManager()
						.getSession().getObject(newID);
				if (TermUtil.isIntersection(newItem)) {
					Object[] rowData = { newItem };
					tableModel.insertRow(row, rowData);
				}
			}
			intersectionTable.repaint();
			return true;
		} else
			return false;
	}

	protected void editNext(int row) {
		if (row < intersectionTable.getRowCount() - 1) {
			intersectionTable.editCellAt(row + 1, 0);
		} else {
			createNewRow();
		}
	}

	protected void createNewRow() {
		Object[] newRow = { new OBOClassImpl(JUNK_ID) };
		tableModel.addRow(newRow);
		intersectionTable.editCellAt(tableModel.getRowCount() - 1, 0);
	}

	public void cleanup() {
		SessionManager.getManager().removeRootChangeListener(rootListener);
		SessionManager.getManager().removeHistoryListener(historyListener);
	}

	public JComponent getComponent() {
		return this;
	}

	public ComponentConfiguration getConfiguration() {
		return null;
	}

	public void init() {
		removeAll();
		setLayout(new BorderLayout());
		add(new JScrollPane(intersectionTable,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER), "Center");
		add(autoselectCheckbox, "South");
		autoselectCheckbox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (autoselectCheckbox.isSelected())
					forwardLocalSelectionToGlobal();
			}
		});
		reset();
		SessionManager.getManager().addRootChangeListener(rootListener);
		SessionManager.getManager().addHistoryListener(historyListener);
	}

	public void reset() {
		List<LinkedObject> intersections = new ArrayList<LinkedObject>();
		for (LinkedObject lo : TermUtil.getTerms(SessionManager.getManager()
				.getSession())) {
			if (TermUtil.isIntersection(lo))
				intersections.add(lo);
		}
		Comparator<LinkedObject> comparator = new Comparator<LinkedObject>() {
			public int compare(LinkedObject o1, LinkedObject o2) {
				return o1.getName().compareToIgnoreCase(o2.getName());
			}
		};
		Collections.sort(intersections, comparator);
		Object[] columnIdentifiers = { "Intersections" };
		Object[][] data = new Object[intersections.size()][1];
		int i = 0;
		for (LinkedObject lo : intersections) {
			data[i++][0] = lo;
		}
		tableModel.setDataVector(data, columnIdentifiers);
		queryEngine = new QueryEngine(SessionManager.getManager().getSession());
		repaint();
	}

	public boolean isSingleton() {
		return false;
	}

	public void setConfiguration(ComponentConfiguration config) {
	}

	public void setXML(String xml) {
	}

	public boolean isXMLSettable() {
		return false;
	}
}
