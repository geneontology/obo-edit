package org.oboedit.gui.components;


import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellEditor;

import org.bbop.framework.GUIManager;
import org.bbop.framework.event.UserEvent;
import org.bbop.framework.event.UserListener;
import org.bbop.swing.widget.TableList;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.IdentifiedObject;
import org.obo.history.HistoryItem;
import org.oboedit.controller.VerificationManager;
import org.oboedit.gui.AbstractTextEditComponent;
import org.oboedit.gui.DbxrefListTableEditor;
import org.oboedit.gui.DbxrefTableRenderer;
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

	protected UserListener dbxrefEditListener = new UserListener() {
		public void userEventOccurred(UserEvent e) {
			if (e instanceof DbxrefUpdateEvent) {
				dbxrefList.add(((DbxrefUpdateEvent) e).getDbxrefs());
			}
		}

		public String getEventType() {
			return getUserEventType();
		}
	};

	protected JButton addButton = new JButton("+");

	protected JButton removeButton = new JButton("-");

	protected TableList<Dbxref> dbxrefList = new TableList<Dbxref>();

	protected abstract String getUserEventType();

	public AbstractDbxrefEditorComponent() {
		super();
		dbxrefList.setRenderer(new DbxrefTableRenderer());
		dbxrefList.setEditor(new DbxrefListTableEditor(createNewDbxref()));

		dbxrefList.addSelectionListener(new ListSelectionListener() {

			public void valueChanged(ListSelectionEvent e) {
				removeButton
						.setEnabled(dbxrefList.getSelectedRows().length > 0);
			}

		});
		setLayout(new BorderLayout());
		JScrollPane pane = new JScrollPane(dbxrefList,
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
		dbxrefList.add();
	}

	protected void delDbxref() {
		dbxrefList.deleteSelectedRows();
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
		List<Dbxref> dbxrefs;
		if (currentObject == null) {
			dbxrefs = Collections.emptyList();
		} else
			dbxrefs = getDbxrefs(currentObject);
		dbxrefList.setData(dbxrefs);
	}

	protected abstract String getDbxrefTitle();

	protected Collection<Dbxref> getEditedDbxrefs() {
		return dbxrefList.getData();
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
			this.root.removeMapping(getPathSpec(), dbxrefList);
		}
		super.setRoot(root);
		getRoot().addMapping(getPathSpec(), this, dbxrefList);
	}

	protected abstract HistoryItem getAddDbxrefItem(Dbxref ref);

	protected abstract HistoryItem getDelDbxrefItem(Dbxref ref);

	protected abstract Dbxref createNewDbxref();

	protected abstract FieldPath getPath(IdentifiedObject io);

	public abstract FieldPathSpec getPathSpec();

	protected List<Dbxref> getDbxrefs(IdentifiedObject io) {
		Collection<FieldPath> paths = getPath(io).resolve();
		List<Dbxref> out = new ArrayList<Dbxref>();
		for (FieldPath path : paths) {
			Object o = path.getLastValue();
			if (o instanceof Dbxref)
				out.add((Dbxref) o);
		}
		return out;
	}

}
