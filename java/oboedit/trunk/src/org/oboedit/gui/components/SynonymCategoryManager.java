package org.oboedit.gui.components;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.swing.*;
import org.obo.dataadapter.OBOConstants;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
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

public class SynonymCategoryManager extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SynonymCategoryManager.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected ListEditor catList;

	protected JLabel noCategoryLabel = new JLabel(
			"Click a category to edit it.");

	protected JButton commitButton = new JButton("Save Changes");

	protected JButton revertButton = new JButton("Revert");

	protected String[] scopes = { "<unspecified scope>", "Synonym",
			"Exact Synonym", "Narrow Synonym", "Broad Synonym" };

	protected ActionListener scopeListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			catList.commit();
		}
	};

	private class CategoryEditor extends JPanel implements
			GenericEditorComponent {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		protected JTextField idField;

		protected JTextField nameField;

		protected JComboBox scopeDrop;

		// private JCheckBox defaultBox;

		protected ListEditor editor;

		public void setMasterComponent(Component c) {
			if (c instanceof ListEditor)
				editor = (ListEditor) c;
		}

		public CategoryEditor() {
			JLabel idLabel = new JLabel("Category id");
			JLabel nameLabel = new JLabel("Category name");
			JLabel scopeLabel = new JLabel("Category scope");

			FocusListener listener = new FocusListener() {
				public void focusLost(FocusEvent e) {
					catList.commit();
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
					catList.commit();
				}

				@Override
				public void keyReleased(KeyEvent e) {
					catList.commit();
				}
			};

			// scopeDrop.addActionListener(scopeListener);

			// idField.addKeyListener(keyListener);
			// nameField.addKeyListener(keyListener);

			idField.addFocusListener(listener);
			nameField.addFocusListener(listener);
			scopeLabel.addFocusListener(listener);

			setLayout(new BoxLayout(CategoryEditor.this, BoxLayout.Y_AXIS));
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
			CategoryWrapper rtw = (CategoryWrapper) o;
			idField.setText(rtw.getID());
			nameField.setText(rtw.getName());
			scopeDrop.setSelectedIndex(rtw.getScope() + 1);
		}

		public void store(Object o) {
			CategoryWrapper rtw = (CategoryWrapper) o;
			rtw.setID(idField.getText());
			rtw.setName(nameField.getText());
			rtw.setScope(scopeDrop.getSelectedIndex() - 1);
			/*
			 * (new Exception("setting scope to "+rtw.getScope())).
			 * printStackTrace();
			 */
		}

		public Object createNewValue() {
			return new CategoryWrapper(new SynonymCategoryImpl("NEWSYNCAT",
					"<new synonym category>"));
		}
	}

	public SynonymCategoryManager(String id) {
		super(id);

		commitButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				saveCategories();
			}
		});
		revertButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				loadCategories();
			}
		});
	}

	@Override
	public String getName() {
		return "Synonym Category Manager Plugin";
	}

	@Override
	public void init() {
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		setPreferredSize(new Dimension(400, 300));
		catList = new ListEditor(new CategoryEditor(), noCategoryLabel,
				new Vector(), true, true, true, true, false);
		Box commitBox = new Box(BoxLayout.X_AXIS);
		commitBox.add(Box.createHorizontalGlue());
		commitBox.add(commitButton);
		commitBox.add(Box.createHorizontalStrut(10));
		commitBox.add(revertButton);
		commitBox.add(Box.createHorizontalGlue());

		removeAll();
		add(catList);
		add(commitBox);
		validate();

		GUIUtil.addReloadListener(reloadListener);

		loadCategories();
	}

	@Override
	public void cleanup() {
		GUIUtil.removeReloadListener(reloadListener);
	}

	protected ReloadListener reloadListener = new ReloadListener() {
		public void reload(ReloadEvent e) {
			if (!e.isFilter())
				loadCategories();
		}
	};

	protected static class CategoryWrapper {
		protected String id;

		protected String name;

		protected int scope;

		protected SynonymCategory oldcat;

		public CategoryWrapper(SynonymCategory cat) {
			this.id = cat.getID();
			this.name = cat.getName();
			this.scope = cat.getScope();
			this.oldcat = cat;
		}

		public boolean isChanged() {
			return !oldcat.getID().equals(id) || !oldcat.getName().equals(name)
					|| oldcat.getScope() != scope;
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

		public SynonymCategory getCategory() {
			return oldcat;
		}

		@Override
		public String toString() {
			return name + " (" + id + ")";
		}
	}

	protected void loadCategories() {
		Vector v = new Vector();

		Iterator it = SessionManager.getManager().getSession()
				.getSynonymCategories().iterator();
		while (it.hasNext()) {
			SynonymCategory cat = (SynonymCategory) it.next();
			v.add(new CategoryWrapper(cat));
		}
		catList.setData(v);
	}

	protected void saveCategories() {
		Set v = new HashSet();
		catList.commit();
		Vector data = catList.getData();
		Set names = new HashSet();
		for (int i = 0; i < data.size(); i++) {
			CategoryWrapper rtw = (CategoryWrapper) data.get(i);
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

			v.add(rtw.getCategory());
			names.add(rtw.getID());
		}

		Vector oldcats = new Vector(SessionManager.getManager().getSession()
				.getSynonymCategories());
		TermMacroHistoryItem item = new TermMacroHistoryItem("Category edits");
		Vector newcats = (Vector) data.clone();
		for (int i = 0; i < oldcats.size(); i++) {
			SynonymCategory cat = (SynonymCategory) oldcats.get(i);
			boolean found = false;
			for (int j = 0; j < data.size(); j++) {
				CategoryWrapper tw = (CategoryWrapper) data.get(j);
				if (tw.getCategory() == cat) {
					logger.info(tw.getCategory() + " == " + cat);
					newcats.remove(tw);
					if (tw.isChanged()) {
						SynonymCategoryHistoryItem catitem = new SynonymCategoryHistoryItem(
								tw.getCategory(), new SynonymCategoryImpl(tw
										.getID(), tw.getName(), tw.getScope()),
								false, false);
						item.addItem(catitem);
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
						if (s.getSynonymCategory() != null) {
							if (s.getSynonymCategory().equals(cat)) {
								JOptionPane.showMessageDialog(this,
										"Could not commit changes "
												+ "because " + cat + " "
												+ "is still in use in the "
												+ "ontology");
								return;
							}
						}
					}
				}
				SynonymCategoryHistoryItem catitem = new SynonymCategoryHistoryItem(
						cat, null, false, true);
				item.addItem(catitem);
			}
		}
		for (int i = 0; i < newcats.size(); i++) {
			CategoryWrapper cw = (CategoryWrapper) newcats.get(i);
			SynonymCategoryHistoryItem catitem = new SynonymCategoryHistoryItem(
					null, SessionManager.getManager().getSession()
							.getObjectFactory().createSynonymCategory(
									cw.getID(), cw.getName(), cw.getScope()),
					true, false);
			item.addItem(catitem);
		}
		GUIUtil.setSelections(item, SelectionManager.getGlobalSelection(),
				SelectionManager.getGlobalSelection());
		SessionManager.getManager().apply(item);
	}
}
