package org.bbop.swing.widget;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EventObject;
import java.util.LinkedList;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.AbstractCellEditor;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import org.bbop.swing.HTMLTableRenderer;
import org.bbop.swing.tablelist.AbstractListTableEditor;
import org.bbop.swing.tablelist.ListTableEditor;

public class TableList<T> extends JComponent {

	protected JTable table;

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
			completingEdit = true;
			flushEdits();
			completingEdit = false;
			boolean b = super.stopCellEditing();
			return b;
		}

		@Override
		public void cancelCellEditing() {
			super.cancelCellEditing();
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

		public Object getValueAt(int rowIndex, int columnIndex) {
			return data.get(rowIndex);
		}

		public void setValueAt(int rowIndex, T value) {
			data.set(rowIndex, value);
			fireTableRowsUpdated(rowIndex, rowIndex);
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
			if (count > 0)
				fireTableRowsInserted(start, count);
		}

		public void deleteSelectedRows() {
			int[] rows = table.getSelectedRows();
			for (int i = rows.length - 1; i >= 0; i--) {
				data.remove(rows[i]);
			}
			fireTableStructureChanged();
		}
	}

	protected int lastEditRow = -1;

	protected class ListTable extends JTable {

		@Override
		public Component prepareEditor(TableCellEditor editor, int row,
				int column) {
			Component out = super.prepareEditor(editor, row, column);
			int requiredHeight = out.getPreferredSize().height * 2;
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

	}

	{
		table = new ListTable();

		setLayout(new GridLayout(1, 1));
		table.setDefaultEditor(Object.class, new ListTableCellEditor());
		table.setDefaultRenderer(Object.class, new ListTableCellRenderer());
		table.setModel(new ListTableModel());
		table.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(
				KeyStroke.getKeyStroke(KeyEvent.VK_ENTER,
						KeyEvent.CTRL_DOWN_MASK), "commit");
		table.getActionMap().put("commit", new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				System.err.println("It happened!");
			}
		});
		add(table);
	}

	protected boolean completingEdit = false;
	protected ListTableEditor<T> editor;
	protected TableCellRenderer renderer = new HTMLTableRenderer();
	protected List<T> data = Collections.emptyList();
	protected ActionListener commitListener = new ActionListener() {

		public void actionPerformed(ActionEvent e) {
			ListTableCellEditor tce = (ListTableCellEditor) table
					.getDefaultEditor(Object.class);
			tce.stopCellEditing();
			editNext();
		}
	};

	public TableList() {
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

	public void setSelectionMode(int mode) {
		table.setSelectionMode(mode);
	}

	public void add() {
		createNewRow();
	}
	
	public void add(T... items) {
		add(Arrays.asList(items));
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
		table.editCellAt(row, 0);
	}

	public void setEditor(ListTableEditor editor) {
		if (this.editor != null)
			this.editor.removeCommitListener(commitListener);
		this.editor = editor;
		editor.addCommitListener(commitListener);
	}

	public ListTableEditor getEditor() {
		return editor;
	}

	public int[] getSelectedRows() {
		return table.getSelectedRows();
	}

	public List<T> getSelection() {
		List<T> out = new ArrayList<T>();
		int[] rows = getSelectedRows();
		for (int row : rows) {
			out.add(data.get(row));
		}
		return out;
	}

	public void setData(T... data) {
		setData(Arrays.asList(data));
	}
	
	public void setData(Collection<T> data) {
		this.data = new ArrayList<T>(data);
		((ListTableModel) table.getModel()).fireTableDataChanged();
		repaint();
	}

	public List<T> getData() {
		flushEdits();
		return data;
	}

	protected void flushEdits() {
		int row = table.getEditingRow();
		if (row >= 0) {
			((ListTableModel) table.getModel()).setValueAt(row, (T) editor
					.getValue());
			table.repaint();
		}
	}

	private static class ListEditor extends AbstractListTableEditor<String> {

		protected JTextField field = new JTextField();

		public ListEditor() {
			setLayout(new BorderLayout());
			add(field, "North");
			field.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					commit();
				}
			});
		}

		public String createNewValue() {
			return "<new value>";
		}

		public String getValue() {
			return field.getText();
		}

		public void notifyActive() {
			field.requestFocus();
		}

		public void notifyCancel() {
		}

		public void setValue(String value) {
			field.setText(value);
		}
	}

	public static void main(String[] args) {
		JDialog dialog = new JDialog();
		dialog.getContentPane().setLayout(new GridLayout(1, 1));
		TableList<String> list = new TableList<String>();
		list.setEditor(new ListEditor());
		list.setRenderer(new HTMLTableRenderer() {
			@Override
			public String getHTML(JTable table, Object value,
					boolean isSelected, boolean hasFocus, int row, int column) {
				return "<html><i>" + value + "</i></html>";
			}
		});
		List<String> data = new LinkedList<String>();
		data.add("Cartman");
		data.add("Kenny");
		data.add("Stan");
		data.add("Kyle");
		list.setData(data);
		dialog.getContentPane().add(list);
		dialog.pack();
		dialog.show();
	}

}
