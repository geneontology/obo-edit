package org.bbop.swing.widget;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.EventObject;
import java.util.List;

import javax.swing.AbstractCellEditor;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;


import org.bbop.swing.FocusHierarchyListener;
import org.bbop.swing.FocusHierarchyManager;
import org.bbop.swing.HTMLTableRenderer;
import org.bbop.swing.tablelist.ListTableEditor;

import org.apache.log4j.*;

public class TableList<T> extends JComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TableList.class);

	protected boolean completingEdit = false;
	protected ListTableEditor<T> editor;
	protected TableCellRenderer renderer = new HTMLTableRenderer();
	protected List<T> data = new ArrayList<T>();
	
	// TextEditor commit listener for rendering table cells
	protected ActionListener commitListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			ListTableCellEditor tce = (ListTableCellEditor) table
					.getDefaultEditor(Object.class);
			tce.stopCellEditing();
//			editNext();  // NO--this creates a new blank dbxref, which we don't want to do.
		}
	};

	protected JTable table;
	protected int lastEditRow = -1;
	protected FocusHierarchyListener focusListener = new FocusHierarchyListener() {

		public void focusGained(FocusEvent e) {
			logger.debug(">>> TableList.focusGained");
		}

		public void focusLost(FocusEvent e) {
//			commitPendingEdits();
			logger.debug(">>> TableList.focusLost");
		}

	};

	protected class ListTableCellRenderer extends DefaultTableCellRenderer {
		@Override
		public Component getTableCellRendererComponent(JTable table,
				Object value, boolean isSelected, boolean hasFocus, int row,
				int column) {
			Component out = renderer.getTableCellRendererComponent(table,
					value, isSelected, hasFocus, row, column);

			if (out.getPreferredSize().height != table.getRowHeight(row)) {
				table.setRowHeight(row, out.getPreferredSize().height);
			}
			return out;
		}
	}

	protected class ListTableCellEditor extends AbstractCellEditor implements
			TableCellEditor {

		public ListTableCellEditor() {
		}

		public void notifyCancel() {
			editor.notifyCancel();
		}

		public void notifyActive() {
			editor.notifyActive();
			FocusHierarchyManager.addFocusHierarchyListener((Component) editor,
					focusListener);
		}

		@Override
		public boolean isCellEditable(EventObject e) {
			if (editor != null
					&& (e == null || e instanceof MouseEvent
							&& ((MouseEvent) e).getClickCount() == 2))
				return super.isCellEditable(e);
			else
				return false;
		}

		public Component getTableCellEditorComponent(JTable table,
				Object value, boolean isSelected, int row, int column) {
			try {
				editor.setValue((T) value);
				return (Component) editor;
			} catch (Throwable t) {
				return null;
			}
		}

		@Override
		public boolean stopCellEditing() {
			logger.debug("TableList.stopCellEditing");
			completingEdit = true;
			flushEdits();
			completingEdit = false;
			boolean b = super.stopCellEditing();
			FocusHierarchyManager.removeFocusHierarchyListener(
					(Component) editor, focusListener);
			return b;
		}

		@Override
		public void cancelCellEditing() {
			super.cancelCellEditing();
			FocusHierarchyManager.removeFocusHierarchyListener(
					(Component) editor, focusListener);
		}

		public Object getCellEditorValue() {
			return editor.getValue();
		}
	}

	protected class ListTableModel extends AbstractTableModel {

		public int getColumnCount() {
			return 1;
		}

		public int getRowCount() {
			return data.size();
		}

		public void addValue(T value) {
			data.add(value);
			fireTableRowsInserted(data.size() - 1, data.size() - 1);
		}

		public Object getValueAt(int rowIndex, int columnIndex) {
			return data.get(rowIndex);
		}

		public void setValueAt(int rowIndex, T value, boolean fireEvent) {
			if (rowIndex >= data.size())
				addValue(value);
			else {
				data.set(rowIndex, value);
				if (fireEvent)
					fireTableRowsUpdated(rowIndex, rowIndex);
			}
		}

		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex) {
			return true;
		}

		public int addRow() {
			data.add(editor.createNewValue());
			int row = data.size() - 1;
			fireTableRowsInserted(row, row);
			return row;
		}

		public void add(Iterable t) {
			int start = data.size();
			int count = 0;
			for (Object o : t) {
				data.add((T) o);
				count++;
			}
			if (count > 0) {
//				fireTableRowsInserted(start, count);
				// args are supposed to be firstrow, lastrow.
				// How did this ever work?  For some reason, it was only problematic when you changed the font size
				// (via the config manager)!
				fireTableRowsInserted(start, start+count-1);
			}
		}

		public void deleteSelectedRows() {
			int row = table.getEditingRow();
			if (row >= 0) {
				ListTableCellEditor te = (ListTableCellEditor) table
						.getDefaultEditor(Object.class);
				te.cancelCellEditing();
				data.remove(row);
			} else {
				int[] rows = table.getSelectedRows();
				for (int i = rows.length - 1; i >= 0; i--) {
					data.remove(rows[i]);
				}
			}
			fireTableStructureChanged();
		}

		@Override
		public String getColumnName(int column) {
			return null;
		}
	}

	protected class ListTable extends JTable {

		protected boolean marginAdjust = false;

		@Override
		public Component prepareEditor(TableCellEditor editor, int row,
				int column) {
			Component out = super.prepareEditor(editor, row, column);
			int requiredHeight = out.getPreferredSize().height * 2;  // + 5;   +5 didn't help
			if (requiredHeight != getRowHeight(row)) {
				setRowHeight(row, requiredHeight);
			}
			table.scrollRectToVisible(table.getCellRect(row, 0, true));
			return out;
		}

		@Override
		public boolean editCellAt(int row, int column, EventObject e) {
			lastEditRow = row;
			super.editCellAt(row, column, e);
			return false;
		}

		@Override
		public void removeEditor() {
			if (marginAdjust)
				return;
			if (!completingEdit) {
				editor.notifyCancel();
			}
			super.removeEditor();
		}

		@Override
		public void setCellEditor(TableCellEditor anEditor) {
			super.setCellEditor(anEditor);
			if (editor != null)
				editor.notifyActive();
		}

		@Override
		public void columnMarginChanged(ChangeEvent e) {
			marginAdjust = true;
			super.columnMarginChanged(e);
			marginAdjust = false;
		}

	}

	public TableList() {
		this(false, false);
	}

	public TableList(boolean wrapInScroller, boolean installButtons) {
		setOpaque(false);
		table = new ListTable();

		setLayout(new BorderLayout());
		table.setDefaultEditor(Object.class, new ListTableCellEditor());
		table.setDefaultRenderer(Object.class, new ListTableCellRenderer());
		table.setModel(new ListTableModel());
		if (wrapInScroller)
			add(new JScrollPane(table), "Center");
		else
			add(table, "Center");
		table.getModel().addTableModelListener(new TableModelListener() {

			public void tableChanged(TableModelEvent e) {
				List<T> l = getSelection();
				for (T t : l) {
					if (!data.contains(t)) {
						setSelection(l);
						return;
					}
				}
			}

		});

		if (installButtons)
			installDefaultButtons();
	}

	public void installDefaultButtons() {
		JPanel panel = new JPanel();
		panel.setOpaque(false);
		Icon plusIcon = new ImageIcon(getClass().getResource(
				"/org/bbop/swing/tablelist/resources/plus.gif"));
		Icon minusIcon = new ImageIcon(getClass().getResource(
				"/org/bbop/swing/tablelist/resources/minus.gif"));

		final JButton add = new JButton(plusIcon);
		final JButton remove = new JButton(minusIcon);
		panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
		panel.add(add);
		panel.add(remove);
		add(panel, "South");
		add.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				add();
				remove.setEnabled(getSelectedRowCount() > 0
						|| table.isEditing());
			}
		});
		remove.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				deleteSelectedRows();
			}
		});
		addSelectionListener(new ListSelectionListener() {

			public void valueChanged(ListSelectionEvent e) {
				remove.setEnabled(getSelectedRowCount() > 0
						|| table.isEditing());
			}

		});
		remove.setEnabled(getSelectedRowCount() > 0 || table.isEditing());
	}

	public void setRenderer(TableCellRenderer renderer) {
		this.renderer = renderer;
	}

	protected void editNext() {
		completingEdit = true;
		int row = lastEditRow;
		// flushEdits();
		editNext(row);
		completingEdit = false;
	}

	protected void editNext(int row) {
		if (row < table.getRowCount() - 1) {
			table.editCellAt(row + 1, 0);
		} else {
			createNewRow();
		}
	}

	public void commitPendingEdits() {
		logger.debug("TableList.commitPendingEdits");
		ListTableCellEditor tce = (ListTableCellEditor) table
				.getDefaultEditor(Object.class);
		if (!isEditing())
		tce.stopCellEditing();
	}

	public void setSelectionMode(int mode) {
		table.setSelectionMode(mode);
	}

	public void add() {
		createNewRow();
	}

	public void add(T... items) {
		add(Arrays.asList(items));
	}

	public void addTableModelListener(TableModelListener listener) {
		table.getModel().addTableModelListener(listener);
	}

	public void removeTableModelListener(TableModelListener listener) {
		table.getModel().removeTableModelListener(listener);
	}

	public int getSelectedRowCount() {
		return getSelectedRows().length;
	}

	public void add(Iterable<T> items) {
		((ListTableModel) table.getModel()).add(items);
	}

	public void deleteSelectedRows() {
		((ListTableModel) table.getModel()).deleteSelectedRows();
	}

	public int getSelectionMode() {
		return table.getSelectionModel().getSelectionMode();
	}

	public void addSelectionListener(ListSelectionListener listener) {
		table.getSelectionModel().addListSelectionListener(listener);
	}

	public void removeSelectionListener(ListSelectionListener listener) {
		table.getSelectionModel().removeListSelectionListener(listener);
	}

	protected void createNewRow() {
		int row = ((ListTableModel) table.getModel()).addRow();
		table.getSelectionModel().clearSelection();
		table.getSelectionModel().addSelectionInterval(row, row);
		table.editCellAt(row, 0);
	}

	public void setEditor(ListTableEditor<T> editor) {
		if (this.editor != null)
			this.editor.removeCommitListener(commitListener);
		this.editor = editor;
		editor.addCommitListener(commitListener);
	}

	public void cancelEditing() {
		logger.debug("TableList.cancelEditing");
		table.removeEditor();
	}

	public ListTableEditor<T> getEditor() {
		return editor;
	}

	public int[] getSelectedRows() {
		return table.getSelectedRows();
	}

	public List<T> getSelection() {
		List<T> out = new ArrayList<T>();
		int[] rows = getSelectedRows();
		for (int row : rows) {
			if (row < data.size())
				out.add(data.get(row));
		}
		return out;
	}

	public void setSelection(Collection<T> c) {
		logger.debug("TableList.setSelection");
		List<Integer> temp = new ArrayList<Integer>();
		for (T t : c) {
			int index = data.indexOf(t);
			if (index != -1)
				temp.add(index);
		}
		table.getSelectionModel().clearSelection();
		for (Integer i : temp) {
			table.getSelectionModel().addSelectionInterval(i, i);
		}
		repaint();
		validate();
	}

	@Override
	public synchronized void addMouseListener(MouseListener l) {
		table.addMouseListener(l);
	}

	@Override
	public synchronized void removeMouseListener(MouseListener l) {
		table.removeMouseListener(l);
	}

	public void setData(T... data) {
		setData(Arrays.asList(data));
	}

	public boolean isEditing() {
		return table.isEditing();
	}

	public T getItemAt(Point p) {
		int row = table.rowAtPoint(p);
		if (row >= getData().size())
			return null;
		return getData().get(row);
	}

	public void setData(Collection<T> data) {
		if (isEditing()) {
				cancelEditing();
		}
		this.data = new ArrayList<T>(data);
		((ListTableModel) table.getModel()).fireTableDataChanged();
//		repaint();
	}

	public List<T> getData() {
		flushEdits();
		return data;
	}

	protected void flushEdits() {
		if (table.isEditing()) {
			((ListTableModel) table.getModel()).setValueAt(table
					.getEditingRow(), editor.getValue(), false);
			table.repaint();
		}
	}

	public TableCellRenderer getRenderer() {
		return renderer;
	}

}
