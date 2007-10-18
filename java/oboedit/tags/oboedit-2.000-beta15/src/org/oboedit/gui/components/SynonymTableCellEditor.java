package org.oboedit.gui.components;

import info.clearthought.layout.TableLayout;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.EventObject;

import javax.swing.AbstractAction;
import javax.swing.AbstractCellEditor;
import javax.swing.FocusManager;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.JTextPane;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;
import javax.swing.text.DocumentFilter.FilterBypass;

import org.obo.datamodel.Dbxref;
import org.obo.datamodel.Synonym;
import org.oboedit.gui.Preferences;

public class SynonymTableCellEditor extends AbstractCellEditor implements
		TableCellEditor {

	protected JTable table;

	protected Synonym synonym;

	protected JPanel editorPanel = new JPanel();

	protected JTextPane synonymField = new JTextPane();

	protected JLabel nameLabel = new JLabel("Name");
	protected JLabel scopeLabel = new JLabel("Scope");
	protected JComboBox typeList;

	public SynonymTableCellEditor() {
		if (synonymField.getDocument() instanceof AbstractDocument) {
			((AbstractDocument) synonymField.getDocument())
					.setDocumentFilter(new DocumentFilter() {
						@Override
						public void insertString(FilterBypass fb, int offset,
								String string, AttributeSet attr)
								throws BadLocationException {
							// TODO Auto-generated method stub
							super.insertString(fb, offset,
									replaceNewlines(string), attr);
						}

						@Override
						public void replace(FilterBypass fb, int offset,
								int length, String text, AttributeSet attrs)
								throws BadLocationException {
							super.replace(fb, offset, length,
									replaceNewlines(text), attrs);
						}
					});
		}
		typeList = new JComboBox(SynonymEditorComponent.TYPES);

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
		editorPanel.removeAll();
		double[][] sizes = {
				{ TableLayout.PREFERRED, 10, TableLayout.FILL },
				{ TableLayout.PREFERRED, TableLayout.PREFERRED,
						TableLayout.PREFERRED, TableLayout.PREFERRED } };
		editorPanel.setLayout(new TableLayout(sizes));
		editorPanel.add(nameLabel, "0,0");
		editorPanel.add(synonymField, "2,0");
		editorPanel.add(scopeLabel, "0,1");
		editorPanel.add(typeList, "2,1");
	}

	protected static String replaceNewlines(String str) {
		StringBuffer b = new StringBuffer();
		for (int i = 0; i < str.length(); i++) {
			char c = str.charAt(i);
			if (c != '\n')
				b.append(c);
		}
		return b.toString();
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

	public Component getTableCellEditorComponent(JTable table, Object value,
			boolean isSelected, int row, int column) {
		this.table = table;
		if (value instanceof Synonym) {
			synonym = (Synonym) ((Synonym) value).clone();
			synonymField.setText(synonym.getText());
			typeList.setSelectedIndex(synonym.getScope());
			editorPanel.validate();
			return editorPanel;
		}
		return null;
	}

	public Object getCellEditorValue() {
		synonym.setText(synonymField.getText());
		synonym.setScope(typeList.getSelectedIndex());
		return synonym;
	}

	protected boolean isCreation = false;

	public void setCreation(boolean isCreation) {
		this.isCreation = isCreation;
	}

	public void notifyCancel() {
		if (isCreation) {
			int editorRow = table.getEditingRow();
			if (table.getModel() instanceof DefaultTableModel) {
				((DefaultTableModel) table.getModel()).removeRow(editorRow);
			}
		}
	}

	@Override
	public boolean isCellEditable(EventObject e) {
		if (e == null || e instanceof MouseEvent
				&& ((MouseEvent) e).getClickCount() == 2)
			return super.isCellEditable(e);
		else
			return false;
	}

	public void notifyActive() {
		synonymField.requestFocus();
	}
}
