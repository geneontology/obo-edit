/**
 * 
 */
package org.oboedit.gui;

import info.clearthought.layout.TableLayout;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.FocusManager;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import org.bbop.swing.tablelist.AbstractListTableEditor;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.impl.DbxrefImpl;

public class DbxrefListTableEditor extends
		AbstractListTableEditor<Dbxref> {

	protected JTextField dbField = new JTextField();

	protected JTextField idField = new JTextField();

	protected JLabel dbLabel = new JLabel("Database");

	protected JLabel idLabel = new JLabel("ID");

	protected JLabel colonLabel = new JLabel(":");

	protected JLabel descLabel = new JLabel("Description");

	protected JTextField descField = new JTextField();

	protected Dbxref prototype;
	
	public DbxrefListTableEditor() {
		this(new DbxrefImpl("XX", "<new dbxref>"));
	}

	public DbxrefListTableEditor(Dbxref prototype) {
		this.prototype = prototype;
		double[][] sizes = {
				{ TableLayout.PREFERRED, 10, TableLayout.FILL },
				{ TableLayout.PREFERRED, TableLayout.PREFERRED,
						TableLayout.PREFERRED, TableLayout.PREFERRED } };
		setLayout(new TableLayout(sizes));
		add(dbLabel, "0,0");
		add(idLabel, "2,0");
		add(dbField, "0,1");
		add(colonLabel, "1,1");
		add(idField, "2,1");
		add(descLabel, "0,2");
		add(descField, "0,3,2,3");

		addHierarchyListener(new HierarchyListener() {

			public void hierarchyChanged(HierarchyEvent e) {
				dbField.requestFocus();
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
	
	@Override
	public void notifyCancel() {
		// TODO Auto-generated method stub
		super.notifyCancel();
	}

	public void notifyActive() {
		dbField.requestFocus();
	}

	public Dbxref createNewValue() {
		return new DbxrefImpl(prototype.getDatabase(), prototype
				.getDatabaseID(), prototype.getDesc(), prototype.getType());
	}

	public Dbxref getValue() {
		Dbxref out = createNewValue();
		out.setDatabase(dbField.getText());
		out.setDatabaseID(idField.getText());
		out.setDesc(descField.getText());
		return out;
	}

	public void setValue(Dbxref dbxref) {
		dbField.setText(dbxref.getDatabase());
		idField.setText(dbxref.getDatabaseID());
		descField.setText(dbxref.getDesc());
	}

}