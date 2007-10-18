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

import org.bbop.swing.tablelist.AbstractListTableEditor;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.impl.SynonymImpl;
import org.oboedit.gui.Preferences;

public class SynonymTableCellEditor extends AbstractListTableEditor<Synonym> {


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

		setFocusCycleRoot(true);
		getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
				.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "tabForward");
		getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
				.put(KeyStroke.getKeyStroke(KeyEvent.VK_TAB, 0), "tabForward");
		getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
				.put(
						KeyStroke.getKeyStroke(KeyEvent.VK_TAB,
								KeyEvent.SHIFT_DOWN_MASK), "tabBackward");


		getActionMap().put("tabForward", new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				tabToNext();
			}
		});

		double[][] sizes = {
				{ TableLayout.PREFERRED, 10, TableLayout.FILL },
				{ TableLayout.PREFERRED, TableLayout.PREFERRED,
						TableLayout.PREFERRED, TableLayout.PREFERRED } };
		setLayout(new TableLayout(sizes));
		add(nameLabel, "0,0");
		add(synonymField, "2,0");
		add(scopeLabel, "0,1");
		add(typeList, "2,1");
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
		Component lastComponent = getFocusTraversalPolicy()
				.getLastComponent(this);
		Component focused = FocusManager.getCurrentKeyboardFocusManager()
				.getFocusOwner();
		if (SwingUtilities.isDescendingFrom(focused, lastComponent)) {
			commit();
		} else
			focused.transferFocus();

	}

	public void notifyActive() {
		synonymField.requestFocus();
	}

	public Synonym createNewValue() {
		return new SynonymImpl("<new synonym>");
	}

	public Synonym getValue() {
		Synonym out = createNewValue();
		out.setText(synonymField.getText());
		out.setScope(typeList.getSelectedIndex());
		return out;
	}

	public void setValue(Synonym value) {
		synonymField.setText(value.getText());
		typeList.setSelectedIndex(value.getScope());
		
	}
}
