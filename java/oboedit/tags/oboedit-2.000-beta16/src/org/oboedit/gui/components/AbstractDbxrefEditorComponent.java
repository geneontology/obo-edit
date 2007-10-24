package org.oboedit.gui.components;

import info.clearthought.layout.TableLayout;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.FocusManager;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellEditor;

import org.bbop.framework.GUIManager;
import org.bbop.framework.event.UserEvent;
import org.bbop.framework.event.UserListener;
import org.bbop.swing.tablelist.AbstractListTableEditor;
import org.bbop.swing.widget.TableList;
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
	
	public class DbxrefListTableEditor extends AbstractListTableEditor<Dbxref> {
		
		protected JTextField dbField = new JTextField();

		protected JTextField idField = new JTextField();

		protected JLabel dbLabel = new JLabel("Database");

		protected JLabel idLabel = new JLabel("ID");

		protected JLabel colonLabel = new JLabel(":");

		protected JLabel descLabel = new JLabel("Description");

		protected JTextField descField = new JTextField();
		
		public DbxrefListTableEditor() {
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
			dbField.requestFocus();
		}

		public Dbxref createNewValue() {
			return createNewDbxref();
		}

		public Dbxref getValue() {
			Dbxref out = createNewDbxref();
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
		dbxrefList.setEditor(new DbxrefListTableEditor());

		dbxrefList.addSelectionListener(
				new ListSelectionListener() {

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
