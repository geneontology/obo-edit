package org.oboedit.gui.components;

import info.clearthought.layout.TableLayout;

import java.awt.Component;
import java.awt.Container;
import java.awt.FocusTraversalPolicy;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.EventObject;

import javax.swing.AbstractAction;
import javax.swing.AbstractCellEditor;
import javax.swing.FocusManager;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.LayoutFocusTraversalPolicy;
import javax.swing.SwingUtilities;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellEditor;

import org.obo.datamodel.Dbxref;
import org.obo.datamodel.IdentifiedObject;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.widget.SessionAutocompleteBox;

public class DbxrefTableCellEditor extends AbstractCellEditor implements
		TableCellEditor {

	protected JPanel editorPanel = new JPanel();

	protected JTextField dbField = new JTextField();

	protected JTextField idField = new JTextField();

	protected JLabel dbLabel = new JLabel("Database");

	protected JLabel idLabel = new JLabel("ID");

	protected JLabel colonLabel = new JLabel(":");

	protected JLabel descLabel = new JLabel("Description");

	protected JTextField descField = new JTextField();

	protected boolean isCreation = false;

	public void setCreation(boolean isCreation) {
		this.isCreation = isCreation;
	}

	public DbxrefTableCellEditor() {
		editorPanel.addHierarchyListener(new HierarchyListener() {

			public void hierarchyChanged(HierarchyEvent e) {
				dbField.requestFocus();
			}

		});
		editorPanel.setFocusCycleRoot(true);
		editorPanel
				.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
				.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "tabForward");
		editorPanel.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
				.put(KeyStroke.getKeyStroke(KeyEvent.VK_TAB, 0), "tabForward");
		editorPanel.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
				.put(
						KeyStroke.getKeyStroke(KeyEvent.VK_TAB,
								KeyEvent.SHIFT_DOWN_MASK), "tabBackward");
		editorPanel.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
				.put(
						KeyStroke.getKeyStroke(KeyEvent.VK_ENTER,
								KeyEvent.CTRL_DOWN_MASK), "commit");

		editorPanel.getActionMap().put("tabForward", new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				tabToNext();
			}
		});
		editorPanel.getActionMap().put("commit", new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				commit();
			}
		});
	}

	protected void tabToNext() {
		Component lastComponent = editorPanel.getFocusTraversalPolicy()
				.getLastComponent(editorPanel);
		Component focused = FocusManager.getCurrentKeyboardFocusManager()
				.getFocusOwner();
		if (SwingUtilities.isDescendingFrom(focused, lastComponent)) {
			commit();
		} else
			focused.transferFocus();

	}

	protected void commit() {
		
		stopCellEditing();
	}

	@Override
	public boolean stopCellEditing() {
		isCreation = false;
		return super.stopCellEditing();
	}

	public void notifyCancel() {
		if (isCreation) {
			int editorRow = table.getEditingRow();
			if (table.getModel() instanceof DefaultTableModel) {
				((DefaultTableModel) table.getModel()).removeRow(editorRow);
			}
		}
	}

	public void notifyActive() {
		dbField.requestFocus();
	}

	@Override
	public boolean isCellEditable(EventObject e) {
		if (e == null || e instanceof MouseEvent
				&& ((MouseEvent) e).getClickCount() == 2)
			return super.isCellEditable(e);
		else
			return false;
	}

	protected JTable table;

	protected Dbxref dbxref;

	public Component getTableCellEditorComponent(JTable table, Object value,
			boolean isSelected, int row, int column) {
		this.table = table;
		if (value instanceof Dbxref) {
			dbxref = (Dbxref) ((Dbxref) value).clone();
			dbField.setText(dbxref.getDatabase());
			idField.setText(dbxref.getDatabaseID());
			descField.setText(dbxref.getDesc());

			editorPanel.removeAll();
			double[][] sizes = {
					{ TableLayout.PREFERRED, 10, TableLayout.FILL },
					{ TableLayout.PREFERRED, TableLayout.PREFERRED,
							TableLayout.PREFERRED, TableLayout.PREFERRED } };
			editorPanel.setLayout(new TableLayout(sizes));
			editorPanel.add(dbLabel, "0,0");
			editorPanel.add(idLabel, "2,0");
			editorPanel.add(dbField, "0,1");
			editorPanel.add(colonLabel, "1,1");
			editorPanel.add(idField, "2,1");
			editorPanel.add(descLabel, "0,2");
			editorPanel.add(descField, "0,3,2,3");

			return editorPanel;
		}
		return null;
	}

	public Object getCellEditorValue() {
		dbxref.setDatabase(dbField.getText());
		dbxref.setDatabaseID(idField.getText());
		dbxref.setDesc(descField.getText());
		return dbxref;
	}

}
