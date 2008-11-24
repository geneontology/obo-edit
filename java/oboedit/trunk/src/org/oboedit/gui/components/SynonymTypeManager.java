package org.oboedit.gui.components;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.swing.*;
import org.obo.dataadapter.OBOConstants;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.history.*;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.event.*;
import org.oboedit.util.GUIUtil;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

import org.apache.log4j.*;

public class SynonymTypeManager extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SynonymTypeManager.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected ListEditor typeList;

	protected JLabel noTypeLabel = new JLabel(
			"Select type to edit");

	protected JButton commitButton = new JButton("Save Changes");

	protected JButton revertButton = new JButton("Revert");

	protected String[] scopes = { "<unspecified scope>", "Synonym",
			"Exact Synonym", "Narrow Synonym", "Broad Synonym" };

	protected ActionListener scopeListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			typeList.commit();
		}
	};

	private class TypeEditor extends JPanel implements
			GenericEditorComponent {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		protected JTextField idField;

		protected JTextField nameField;

		protected JComboBox scopeDrop;

		protected ListEditor editor;

		public void setMasterComponent(Component c) {
			if (c instanceof ListEditor)
				editor = (ListEditor) c;
		}

		public TypeEditor() {
			JLabel idLabel = new JLabel("Type id");
			JLabel nameLabel = new JLabel("Type name");
			JLabel scopeLabel = new JLabel("Type scope");

			FocusListener listener = new FocusListener() {
				public void focusLost(FocusEvent e) {
					typeList.commit();
				}

				public void focusGained(FocusEvent e) {
				}
			};

			idField = new JTextField(10);
			nameField = new JTextField();
			scopeDrop = new JComboBox(scopes);

			KeyListener keyListener = new KeyAdapter() {
				@Override
				public void keyPressed(KeyEvent e) {
					typeList.commit();
				}

				@Override
				public void keyReleased(KeyEvent e) {
					typeList.commit();
				}
			};

			// scopeDrop.addActionListener(scopeListener);

			// idField.addKeyListener(keyListener);
			// nameField.addKeyListener(keyListener);

			idField.addFocusListener(listener);
			nameField.addFocusListener(listener);
			scopeLabel.addFocusListener(listener);

			setLayout(new BoxLayout(TypeEditor.this, BoxLayout.Y_AXIS));
			add(idLabel);
			add(idField);
			add(Box.createVerticalStrut(10));
			add(nameLabel);
			add(nameField);
			add(Box.createVerticalStrut(10));
			add(scopeLabel);
			add(scopeDrop);
			add(Box.createVerticalGlue());
		}

		public void load(Object o) {
			TypeWrapper rtw = (TypeWrapper) o;
			idField.setText(rtw.getID());
			nameField.setText(rtw.getName());
			scopeDrop.setSelectedIndex(rtw.getScope() + 1);
		}

		public void store(Object o) {
			TypeWrapper rtw = (TypeWrapper) o;
			rtw.setID(idField.getText());
			rtw.setName(nameField.getText());
			rtw.setScope(scopeDrop.getSelectedIndex() - 1);
			/*
			 * (new Exception("setting scope to "+rtw.getScope())).
			 * printStackTrace();
			 */
		}

		public Object createNewValue() {
			return new TypeWrapper(new SynonymTypeImpl("NEWSYNTYPE",
					"<new synonym type>"));
		}
	}

	public SynonymTypeManager(String id) {
		super(id);

		commitButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				saveTypes();
				Preferences.getPreferences().fireReconfigEvent(new ReconfigEvent(this));
			}
		});
		revertButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				loadTypes();
			}
		});
	}

	@Override
	public String getName() {
		return "Synonym Type Manager";
	}

	@Override
	public void init() {
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		setPreferredSize(new Dimension(400, 300));
		typeList = new ListEditor(new TypeEditor(), noTypeLabel,
				new Vector(), true, true, true, true, false);
		Box commitBox = new Box(BoxLayout.X_AXIS);
		commitBox.add(Box.createHorizontalGlue());
		commitBox.add(commitButton);
		commitBox.add(Box.createHorizontalStrut(10));
		commitBox.add(revertButton);
		commitBox.add(Box.createHorizontalGlue());

		removeAll();
		add(typeList);
		add(commitBox);
		validate();
		
		GUIUtil.addReloadListener(reloadListener);
		loadTypes();
		
	}

	@Override
	public void cleanup() {
		GUIUtil.removeReloadListener(reloadListener);
	}

	protected ReloadListener reloadListener = new ReloadListener() {
		public void reload(ReloadEvent e) {
			if (!e.isFilter())
				loadTypes();
		}
	};

	protected static class TypeWrapper {
		protected String id;

		protected String name;

		protected int scope;

		protected SynonymType oldtype;

		public TypeWrapper(SynonymType cat) {
			this.id = cat.getID();
			this.name = cat.getName();
			this.scope = cat.getScope();
			this.oldtype = cat;
		}

		public boolean isChanged() {
			return !oldtype.getID().equals(id) || !oldtype.getName().equals(name)
					|| oldtype.getScope() != scope;
		}

		public void setName(String name) {
			this.name = name;
		}

		public void setID(String id) {
			this.id = id;
		}

		public String getName() {
			return name;
		}

		public String getID() {
			return id;
		}

		public int getScope() {
			return scope;
		}

		public void setScope(int scope) {
			this.scope = scope;
		}

		public SynonymType getSynType() {
			return oldtype;
		}

		@Override
		public String toString() {
			return name + " (" + id + ")";
		}
	}

	protected void loadTypes() {
		Vector v = new Vector();
		
		Iterator it = SessionManager.getManager().getSession()
				.getSynonymTypes().iterator();
		while (it.hasNext()) {
			SynonymType syntype = (SynonymType) it.next();
			v.add(new TypeWrapper(syntype));
		}
		typeList.setData(v);
	}

	protected void saveTypes() {
		Set v = new HashSet();
		typeList.commit();
		Vector data = typeList.getData();
		Set names = new HashSet();
		for (int i = 0; i < data.size(); i++) {
			TypeWrapper rtw = (TypeWrapper) data.get(i);
			if (names.contains(rtw.getID())) {
				JOptionPane.showMessageDialog(this, "Could not commit changes "
						+ "because multiple categories " + "are using the id "
						+ rtw.getName());
				return;
			}

			if (!OBOConstants.isOBOIdentifierToken(rtw.getID())) {
				JOptionPane
						.showMessageDialog(
								this,
								"Illegal characters in synonym "
										+ "category id \""
										+ rtw.getID()
										+ "\".\nSynonym category ids may only "
										+ "contain letters,\ndigits, underscores, and dashes.");
				return;
			}

			v.add(rtw.getSynType());
			names.add(rtw.getID());
		}

		Vector oldtypes = new Vector(SessionManager.getManager().getSession()
				.getSynonymTypes());
		TermMacroHistoryItem item = new TermMacroHistoryItem("Type edits");
		Vector newtypes = (Vector) data.clone();
		for (int i = 0; i < oldtypes.size(); i++) {
			SynonymType syntype = (SynonymType) oldtypes.get(i);
			boolean found = false;
			for (int j = 0; j < data.size(); j++) {
				TypeWrapper tw = (TypeWrapper) data.get(j);
				if (tw.getSynType() == syntype) {
					logger.info(tw.getSynType() + " == " + syntype);
					newtypes.remove(tw);
					if (tw.isChanged()) {
						SynonymTypeHistoryItem typeitem = new SynonymTypeHistoryItem(
								tw.getSynType(), new SynonymTypeImpl(tw
										.getID(), tw.getName(), tw.getScope()),
								false, false);
						item.addItem(typeitem);
					}
					found = true;
					break;
				}
			}
			if (!found) {
				Iterator it = SessionManager.getManager().getSession()
						.getObjects().iterator();
				while (it.hasNext()) {
					IdentifiedObject io = (IdentifiedObject) it.next();
					if (!(io instanceof SynonymedObject))
						continue;
					SynonymedObject term = (SynonymedObject) io;
					Iterator it2 = term.getSynonyms().iterator();
					while (it2.hasNext()) {
						Synonym s = (Synonym) it2.next();
						if (s.getSynonymType() != null) {
							if (s.getSynonymType().equals(syntype)) {
								JOptionPane.showMessageDialog(this,
										"Could not commit changes "
												+ "because " + syntype + " "
												+ "is still in use in the "
												+ "ontology");
								return;
							}
						}
					}
				}
				SynonymTypeHistoryItem typeitem = new SynonymTypeHistoryItem(
						syntype, null, false, true);
				item.addItem(typeitem);
			}
		}
		for (int i = 0; i < newtypes.size(); i++) {
			TypeWrapper tw = (TypeWrapper) newtypes.get(i);
			SynonymTypeHistoryItem typeitem = new SynonymTypeHistoryItem(
					null, SessionManager.getManager().getSession()
							.getObjectFactory().createSynonymType(
									tw.getID(), tw.getName(), tw.getScope()),
					true, false);
			item.addItem(typeitem);
		}
		GUIUtil.setSelections(item, SelectionManager.getGlobalSelection(),
				SelectionManager.getGlobalSelection());
		SessionManager.getManager().apply(item);
	}
}
