package org.oboedit.gui.components;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellEditor;

import org.bbop.framework.GUIManager;
import org.bbop.framework.UserEvent;
import org.bbop.framework.UserListener;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.IdentifiedObject;
import org.obo.history.HistoryItem;
import org.oboedit.controller.VerificationManager;
import org.oboedit.gui.AbstractTextEditComponent;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.RootTextEditComponent;

public abstract class AbstractDbxrefEditorComponent extends
		AbstractTextEditComponent {

	public static class DbxrefUpdateEvent extends UserEvent {
		protected Dbxref[] addThese;

		public DbxrefUpdateEvent(Object source, String type, Dbxref[] addThese) {
			super(source, type);
			this.addThese = addThese;
		}

		public Dbxref[] getDbxrefs() {
			return addThese;
		}
	}

	protected class DbxrefTableRenderer extends DefaultTableCellRenderer {
		@Override
		public Component getTableCellRendererComponent(JTable table,
				Object value, boolean isSelected, boolean hasFocus, int row,
				int column) {
			JLabel out = (JLabel) super.getTableCellRendererComponent(table,
					value, isSelected, hasFocus, row, column);
			if (value instanceof Dbxref)
				configureLabel(table, out, (Dbxref) value, row, isSelected);
			if (out.getPreferredSize().height != table.getRowHeight(row)) {
				table.setRowHeight(row, out.getPreferredSize().height);
			}
			return out;
		}
	}

	protected DefaultTableModel tableModel = new DefaultTableModel();

	protected JTable table = new JTable() {

		@Override
		public Component prepareEditor(TableCellEditor editor, int row,
				int column) {
			Component out = super.prepareEditor(editor, row, column);
			int requiredHeight = out.getPreferredSize().height * 2;
			if (requiredHeight != getRowHeight(row)) {
				setRowHeight(row, requiredHeight);
			}
			scrollRectToVisible(getCellRect(row, 0, true));
			return out;
		}

		@Override
		public void removeEditor() {
			if (getDefaultEditor(Object.class) instanceof DbxrefTableCellEditor) {
				((DbxrefTableCellEditor) getDefaultEditor(Object.class))
						.notifyCancel();
			}
			super.removeEditor();
		}

		@Override
		public void setCellEditor(TableCellEditor anEditor) {
			super.setCellEditor(anEditor);
			if (anEditor instanceof DbxrefTableCellEditor)
				((DbxrefTableCellEditor) anEditor).notifyActive();
		}
	};

	protected UserListener dbxrefEditListener = new UserListener() {
		public void userEventOccurred(UserEvent e) {
			if (e instanceof DbxrefUpdateEvent) {
				tableModel.addRow(((DbxrefUpdateEvent) e).getDbxrefs());
			}
		}

		public String getEventType() {
			return getUserEventType();
		}
	};

	protected JButton addButton = new JButton("+");

	protected JButton removeButton = new JButton("-");

	protected DbxrefTableCellEditor cellEditor = new DbxrefTableCellEditor();

	protected abstract String getUserEventType();

	public AbstractDbxrefEditorComponent() {
		super();

		table.setModel(tableModel);
		table.setDefaultEditor(Object.class, cellEditor);
		table.setDefaultRenderer(Object.class, new DbxrefTableRenderer());
		// TableColumn tc = table.getColumn(getDbxrefTitle());
		table.setTableHeader(null);
		table.getSelectionModel().addListSelectionListener(
				new ListSelectionListener() {

					public void valueChanged(ListSelectionEvent e) {
						removeButton
								.setEnabled(table.getSelectedRowCount() > 0);
					}

				});
		setLayout(new BorderLayout());
		JScrollPane pane = new JScrollPane(table,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		add(pane, "Center");
		pane.setRowHeader(null);
		pane.setColumnHeader(null);
		JPanel buttonPanel = new JPanel();
		buttonPanel.setOpaque(false);
		buttonPanel.setLayout(new GridLayout(1, 2));
		buttonPanel.add(addButton);
		buttonPanel.add(removeButton);
		addButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				addDbxref();
			}
		});
		removeButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				delDbxref();
			}
		});
		addButton.setToolTipText("Add new dbxref");
		removeButton.setToolTipText("Remove selected dbxrefs");

		add(buttonPanel, "South");
	}

	protected void addDbxref() {
		Object[] data = { createNewDbxref() };
		tableModel.addRow(data);
		cellEditor.setCreation(true);
		table.editCellAt(tableModel.getRowCount() - 1, 0);
	}

	protected void delDbxref() {
		int[] rows = table.getSelectedRows();
		for (int i = rows.length - 1; i >= 0; i--)
			tableModel.removeRow(rows[i]);
	}

	protected static final Color reallyLightGray = new Color(230, 230, 230);

	protected void configureLabel(JTable table, JLabel out, Dbxref dbxref,
			int index, boolean isSelected) {
		out.setOpaque(true);
		out.setBorder(new EmptyBorder(10, 10, 10, 10));
		out.setMinimumSize(new Dimension(table.getWidth(), 0));
		if (!isSelected) {
			if (index % 2 == 0)
				out.setBackground(reallyLightGray);
			else
				out.setBackground(Color.white);
		}
		String s = "<html>"
				+ dbxref.getDatabase()
				+ ":"
				+ dbxref.getDatabaseID()
				+ "<br>"
				+ (dbxref.getDesc() != null ? "<i>" + dbxref.getDesc() + "</i>"
						: "") + "</html>";
		out.setText(s);
	}

	@Override
	public void init() {
		super.init();
		GUIManager.getManager().addUserListener(dbxrefEditListener);
	}

	@Override
	public void cleanup() {
		super.cleanup();
		GUIManager.getManager().removeUserListener(dbxrefEditListener);
	}

	@Override
	protected void loadGUI() {
		Collection<Dbxref> dbxrefs;
		if (currentObject == null) {
			dbxrefs = Collections.emptySet();
		} else
			dbxrefs = getDbxrefs(currentObject);
		Object[] columnIdentifiers = { getDbxrefTitle() };
		Object[][] data = new Object[dbxrefs.size()][1];
		int i = 0;

		for (Dbxref dbxref : dbxrefs) {
			data[i++][0] = dbxref;
		}
		tableModel.setDataVector(data, columnIdentifiers);
	}

	protected abstract String getDbxrefTitle();

	protected Collection<Dbxref> getEditedDbxrefs() {
		Collection<Dbxref> out = new LinkedList<Dbxref>();
		for (int i = 0; i < tableModel.getRowCount(); i++) {
			out.add((Dbxref) tableModel.getValueAt(i, 0));
		}
		return out;
	}

	public List<HistoryItem> getChanges() {
		Collection<Dbxref> editedDbxrefs = getEditedDbxrefs();
		LinkedList<HistoryItem> out = new LinkedList<HistoryItem>();
		Iterator it = editedDbxrefs.iterator();
		while (it.hasNext()) {
			Dbxref ref = (Dbxref) it.next();
			boolean found = false;
			Collection<Dbxref> refs = getDbxrefs(currentObject);
			Iterator it2 = refs.iterator();
			while (it2.hasNext()) {
				Dbxref eref = (Dbxref) it2.next();
				if (ref.equals(eref)) {
					found = true;
					break;
				}
			}
			if (!found) {
				HistoryItem item = getAddDbxrefItem(ref);
				out.add(item);
			}
		}
		it = getDbxrefs(currentObject).iterator();
		while (it.hasNext()) {
			Dbxref ref = (Dbxref) it.next();
			boolean found = false;
			Iterator it2 = editedDbxrefs.iterator();
			while (it2.hasNext()) {
				Dbxref eref = (Dbxref) it2.next();
				if (ref.equals(eref)) {
					found = true;
					break;
				}
			}

			if (!found) {
				HistoryItem item = getDelDbxrefItem(ref);
				out.add(item);
			}
		}

		return out;
	}

	@Override
	public void setRoot(RootTextEditComponent root) {
		if (this.root != null) {
			this.root.removeMapping(getPathSpec(), table);
		}
		super.setRoot(root);
		getRoot().addMapping(getPathSpec(), this, table);
	}

	protected abstract HistoryItem getAddDbxrefItem(Dbxref ref);

	protected abstract HistoryItem getDelDbxrefItem(Dbxref ref);

	protected abstract Dbxref createNewDbxref();

	protected abstract FieldPath getPath(IdentifiedObject io);

	public abstract FieldPathSpec getPathSpec();

	protected Collection<Dbxref> getDbxrefs(IdentifiedObject io) {
		Collection<FieldPath> paths = getPath(io).resolve();
		Collection<Dbxref> out = new LinkedList<Dbxref>();
		for (FieldPath path : paths) {
			Object o = path.getLastValue();
			if (o instanceof Dbxref)
				out.add((Dbxref) o);
		}
		return out;
	}

}
