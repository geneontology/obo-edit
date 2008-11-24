package org.oboedit.gui;

import info.clearthought.layout.TableLayout;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.Iterator;

import javax.swing.AbstractAction;
import javax.swing.FocusManager;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;

import org.bbop.framework.GUIManager;
import org.bbop.swing.tablelist.AbstractListTableEditor;
import org.bbop.swing.widget.TableList;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.SynonymType;
import org.obo.datamodel.impl.SynonymImpl;
import org.obo.filters.SynonymDbxrefSearchCriterion;
import org.obo.filters.SynonymTextSearchCriterion;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.components.SynonymEditorComponent;

import org.apache.log4j.*;

public class SynonymTableCellEditor extends AbstractListTableEditor<Synonym> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SynonymTableCellEditor.class);

	protected JTextPane synonymField = new JTextPane();

	protected JLabel nameLabel = new JLabel("Name");
	protected JLabel scopeLabel = new JLabel("Scope");
	protected JLabel typeLabel = new JLabel("Type");
	protected JLabel xrefLabel = new JLabel("Xrefs");
	protected JComboBox scopeList;
	protected JComboBox typeList;
	protected TableList<Dbxref> xrefList = new TableList<Dbxref>();
	protected JButton addDbxrefButton = new JButton("+");
	protected JButton removeDbxrefButton = new JButton("-");
	Object obj;

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
		scopeList = new JComboBox(SynonymEditorComponent.SYNSCOPES);
		typeList = new JComboBox();
		typeList.removeAllItems();
		typeList.addItem("<no synonym type>");
//		logger.debug("typeList.getSelectedIndex(): " + typeList.getSelectedIndex());

		Iterator it = SessionManager.getManager().getSession().getSynonymTypes().iterator();
		while (it.hasNext()){
			SynonymType syntype = (SynonymType) it.next();
			typeList.addItem(syntype);
		}

		typeList.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				if (typeList.getSelectedIndex() < 1){
					typeList.setSelectedItem(null);
					typeList.setSelectedItem("<no synonym type>");
					scopeList.setEnabled(isEnabled());
				}
				else {
					SynonymType type = (SynonymType) typeList.getSelectedItem();
					if (type == null)
						typeList.setSelectedIndex(0);

					if (type.getScope() != Synonym.UNKNOWN_SCOPE) {
						scopeList.setSelectedIndex(type.getScope());

					} else {
						scopeList.setEnabled(isEnabled());
						typeList.setEnabled(isEnabled());
					}
				}
			}
		});

		xrefList.setRenderer(new DbxrefTableRenderer());
		xrefList.setEditor(new DbxrefListTableEditor());

		addDbxrefButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				xrefList.add();
			}
		});
		removeDbxrefButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				xrefList.deleteSelectedRows();
			}
		});

		setFocusCycleRoot(true);
		getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(
				KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "tabForward");
		getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(
				KeyStroke.getKeyStroke(KeyEvent.VK_TAB, 0), "tabForward");
		getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(
				KeyStroke.getKeyStroke(KeyEvent.VK_TAB,
						KeyEvent.SHIFT_DOWN_MASK), "tabBackward");

		getActionMap().put("tabForward", new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				tabToNext();
			}
		});

		double[][] sizes = {
				{ TableLayout.PREFERRED, 10, .6, .4 },
				{ TableLayout.FILL, TableLayout.PREFERRED,
					TableLayout.PREFERRED, TableLayout.PREFERRED } };
		setLayout(new TableLayout(sizes));
		add(nameLabel, "0,0");
		add(synonymField, "2,0");
		add(scopeLabel, "0,1");
		add(scopeList, "2,1");
		add(typeLabel, "0,2");
		add(typeList, "2,2");


		JPanel panel = new JPanel();
		panel.setLayout(new BorderLayout());
		JPanel buttonPanel = new JPanel();
		JLabel xrefLabel = new JLabel("Xrefs");
		buttonPanel.setLayout(new GridLayout(1, 2));
		buttonPanel.add(addDbxrefButton);
		buttonPanel.add(removeDbxrefButton);
		panel.add(new JScrollPane(xrefList,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER), "Center");
		panel.add(xrefLabel, "North");
		panel.add(buttonPanel, "South");
		add(panel, "3,0,3,2");
	}

	protected static String replaceNewlines(String str) {
		// Why do it this painstaking way?
//		StringBuffer b = new StringBuffer();
//		for (int i = 0; i < str.length(); i++) {
//		char c = str.charAt(i);
//		if (c != '\n')
//		b.append(c);
//		}
//		return b.toString();
		return(str.replace('\n', ' '));
	}

	protected void tabToNext() {
		Component lastComponent = getFocusTraversalPolicy().getLastComponent(
				this);
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

	public void installMappings(SynonymEditorComponent parent) {
		parent.getRoot().addMapping(
				new FieldPathSpec(parent.spec,
						SynonymTextSearchCriterion.CRITERION), parent,
						synonymField);
		parent.getRoot().addMapping(
				new FieldPathSpec(parent.spec,
						SynonymDbxrefSearchCriterion.CRITERION), parent,
						xrefList);
	}

	public void uninstallMappings(SynonymEditorComponent parent) {
		parent.getRoot().removeMapping(
				new FieldPathSpec(parent.spec,
						SynonymTextSearchCriterion.CRITERION), synonymField);
		parent.getRoot().removeMapping(
				new FieldPathSpec(parent.spec,
						SynonymDbxrefSearchCriterion.CRITERION), xrefList);
	}

	public Synonym getValue() {
		Synonym out = createNewValue();
		out.setText(synonymField.getText());
		out.setScope(scopeList.getSelectedIndex());
		if(typeList.getSelectedItem() != null && typeList.getSelectedItem() != "<no synonym type>")
		out.setSynonymType((SynonymType) typeList.getSelectedItem());

		for (Dbxref ref : xrefList.getData()) {
			out.addXref(ref);
		}
		return out;
	}

	public void setValue(Synonym value) {
		synonymField.setText(value.getText());
		scopeList.setSelectedIndex(value.getScope());
//		if(typeList.getSelectedItem() != null)
		typeList.setSelectedItem(value.getSynonymType());
		xrefList.setData(value.getXrefs());
	}
}
