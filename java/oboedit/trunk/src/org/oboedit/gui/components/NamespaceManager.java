package org.oboedit.gui.components;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.swing.*;
import org.obo.datamodel.*;
import org.obo.history.*;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.gui.event.*;
import org.oboedit.util.GUIUtil;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

import org.apache.log4j.*;

public class NamespaceManager extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(NamespaceManager.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected ListEditor nsList;

	protected JLabel noNamespaceLabel = new JLabel(
			"Click a namespace to edit it.");

	protected JButton commitButton = new JButton("Save Changes");

	protected JButton revertButton = new JButton("Revert");

	private class NamespaceEditor extends JPanel implements
			GenericEditorComponent {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		private JTextField idField;

		protected ListEditor editor;

		public void setMasterComponent(Component c) {
			if (c instanceof ListEditor)
				editor = (ListEditor) c;
		}

		public NamespaceEditor() {
			JLabel idLabel = new JLabel("Namespace id");
			FocusListener listener = new FocusListener() {
				public void focusLost(FocusEvent e) {
					nsList.commit();
				}

				public void focusGained(FocusEvent e) {
				}
			};

			idField = new JTextField(10);

			idField.addFocusListener(listener);
			idField.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					nsList.commit();
				}
			});

			setLayout(new BoxLayout(NamespaceEditor.this, BoxLayout.Y_AXIS));
			add(idLabel);
			add(idField);
			add(Box.createVerticalGlue());
		}

		public void load(Object o) {
			NamespaceWrapper rtw = (NamespaceWrapper) o;
			idField.setText(rtw.getID());
		}

		public void store(Object o) {
			NamespaceWrapper rtw = (NamespaceWrapper) o;
			rtw.setID(idField.getText());
		}

		public Object createNewValue() {
			return new NamespaceWrapper("<new namespace id>");
		}
	}

	protected static class NamespaceWrapper {
		Namespace ns;

		String id;

		public NamespaceWrapper(String id) {
			ns = null;
			this.id = id;
		}

		public NamespaceWrapper(Namespace ns) {
			this.ns = ns;
			this.id = ns.getID();
		}

		public void setID(String id) {
			this.id = id;
		}

		public String getID() {
			return id;
		}

		public Namespace getNamespace() {
			return ns;
		}

		@Override
		public String toString() {
			return id;
		}
	}

	public NamespaceManager(String id) {
		super(id);
	}

	@Override
	public String getName() {
		return "Namespace Manager Plugin";
	}

	@Override
	public void init() {
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		setPreferredSize(new Dimension(400, 300));
		nsList = new ListEditor(new NamespaceEditor(), noNamespaceLabel,
				new Vector(), true, true, true, true, false);

		Box commitBox = new Box(BoxLayout.X_AXIS);
		commitBox.add(Box.createHorizontalGlue());
		commitBox.add(commitButton);
		commitBox.add(Box.createHorizontalStrut(10));
		commitBox.add(revertButton);
		commitBox.add(Box.createHorizontalGlue());

		commitButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				saveNamespaces();
			}
		});
		revertButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				loadNamespaces();
			}
		});

		removeAll();
		add(nsList);
		add(commitBox);
		validate();

		GUIUtil.addReloadListener(reloadListener);

		loadNamespaces();
	}

	@Override
	public void cleanup() {
		GUIUtil.removeReloadListener(reloadListener);
	}

	protected ReloadListener reloadListener = new ReloadListener() {
		public void reload(ReloadEvent e) {
			if (!e.isFilter())
				loadNamespaces();
		}
	};

	protected void loadNamespaces() {
		Vector v = new Vector();

		Iterator it = SessionManager.getManager().getSession().getNamespaces()
				.iterator();
		while (it.hasNext()) {
			Namespace ns = (Namespace) it.next();
			v.add(new NamespaceWrapper(ns));
		}
		nsList.setData(v);
	}

	protected boolean isInUse(Namespace ns) {
		Iterator it2 = SessionManager.getManager().getSession().getObjects()
				.iterator();
		boolean inUse = false;
		while (it2.hasNext()) {
			IdentifiedObject term = (IdentifiedObject) it2.next();
			if (term.getNamespace() != null && term.getNamespace().equals(ns)) {
				inUse = true;
				break;
			}
			if (term instanceof LinkedObject) {
				Iterator it = ((LinkedObject) term).getParents().iterator();
				while (it.hasNext()) {
					Link tr = (Link) it.next();
					if (tr.getNamespace() != null
							&& tr.getNamespace().equals(ns)) {
						inUse = true;
						break;
					}
				}
			}
		}
		if (SessionManager.getManager().getSession().getDefaultNamespace()
				.equals(ns))
			return true;
		return inUse;
	}

	protected void saveNamespaces() {
		Set v = new HashSet();
		Vector data = nsList.getData();
		for (int i = 0; i < data.size(); i++) {
			NamespaceWrapper ns = (NamespaceWrapper) data.get(i);
			if (v.contains(ns)) {
				JOptionPane.showMessageDialog(this, "Could not commit changes "
						+ "because multiple namespaces " + "are named " + ns);
				return;
			}
			v.add(ns);
		}
		Iterator it = SessionManager.getManager().getSession().getNamespaces()
				.iterator();
		TermMacroHistoryItem item = new TermMacroHistoryItem("Namespace edits");
		while (it.hasNext()) {
			Namespace ns = (Namespace) it.next();
			boolean removed = true;
			Iterator it2 = v.iterator();
			while (it2.hasNext()) {
				NamespaceWrapper nw = (NamespaceWrapper) it2.next();
				if (nw.getNamespace() != null && nw.getNamespace().equals(ns)) {
					removed = false;
					break;
				}
			}
			if (removed) {
				if (isInUse(ns)) {
					JOptionPane.showMessageDialog(this,
							"Could not commit changes " + "because " + ns + " "
									+ "is still in use in the " + "ontology");
					loadNamespaces();
					repaint();
					return;
				}
				item.addItem(new TermNamespaceHistoryItem(ns.getID(), null,
						false, true));
			}
		}

		it = v.iterator();
		while (it.hasNext()) {
			NamespaceWrapper nw = (NamespaceWrapper) it.next();
			if (nw.getNamespace() == null) {
				item.addItem(new TermNamespaceHistoryItem(null, nw.getID(),
						true, false));
			} else {
				item.addItem(new TermNamespaceHistoryItem(nw.getNamespace()
						.getID(), nw.getID(), false, false));
			}
		}
		GUIUtil.setSelections(item, SelectionManager.getGlobalSelection(),
				SelectionManager.getGlobalSelection());
		SessionManager.getManager().apply(item);
	}
}
