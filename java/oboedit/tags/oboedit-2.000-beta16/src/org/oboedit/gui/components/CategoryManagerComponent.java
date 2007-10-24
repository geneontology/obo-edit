package org.oboedit.gui.components;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.swing.*;
import org.obo.datamodel.*;
import org.obo.history.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.event.*;
import org.oboedit.util.GUIUtil;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

public class CategoryManagerComponent extends AbstractGUIComponent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected ListEditor catList;

	protected JLabel noCategoryLabel = new JLabel(
			"Click a category to edit it.");

	protected JButton commitButton = new JButton("Save Changes");

	protected JButton revertButton = new JButton("Revert");

	private class CategoryEditor extends JPanel implements
			GenericEditorComponent {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		private JTextField nameField;

		private JTextField descField;

		// private JCheckBox defaultBox;

		protected ListEditor editor;

		public void setMasterComponent(Component c) {
			if (c instanceof ListEditor)
				editor = (ListEditor) c;
		}

		public CategoryEditor() {
			JLabel nameLabel = new JLabel("Category name");
			JLabel descLabel = new JLabel("Category description");
			FocusListener listener = new FocusListener() {
				public void focusLost(FocusEvent e) {
					catList.commit();
				}

				public void focusGained(FocusEvent e) {
				}
			};

			nameField = new JTextField(10);
			descField = new JTextField();

			nameField.addFocusListener(listener);
			descField.addFocusListener(listener);

			setLayout(new BoxLayout(CategoryEditor.this, BoxLayout.Y_AXIS));
			add(nameLabel);
			add(nameField);
			add(Box.createVerticalStrut(10));
			add(descLabel);
			add(descField);
			add(Box.createVerticalGlue());
		}

		public void load(Object o) {
			CategoryWrapper rtw = (CategoryWrapper) o;
			nameField.setText(rtw.getName());
			descField.setText(rtw.getDesc());
		}

		public void store(Object o) {
			CategoryWrapper rtw = (CategoryWrapper) o;
			rtw.setName(nameField.getText());
			rtw.setDesc(descField.getText());
		}

		public Object createNewValue() {
			return new CategoryWrapper(SessionManager.getManager().getSession()
					.getObjectFactory().createCategory("NEWCAT",
							"<new term category>"));
		}
	}

	protected String id;

	public CategoryManagerComponent(String id) {
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
		return "Category Manager Plugin";
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
		protected String name;

		protected String desc;

		protected TermCategory oldcat;

		public CategoryWrapper(TermCategory cat) {
			this.name = cat.getName();
			this.desc = cat.getDesc();
			this.oldcat = cat;
		}

		public boolean isChanged() {
			return !oldcat.getName().equals(name)
					|| !oldcat.getDesc().equals(desc);
		}

		public void setName(String name) {
			this.name = name;
		}

		public void setDesc(String desc) {
			this.desc = desc;
		}

		public String getName() {
			return name;
		}

		public String getDesc() {
			return desc;
		}

		public TermCategory getCategory() {
			return oldcat;
		}

		@Override
		public String toString() {
			return desc + " (" + name + ")";
		}
	}

	protected void loadCategories() {
		Vector v = new Vector();

		Iterator it = SessionManager.getManager().getSession().getCategories()
				.iterator();
		while (it.hasNext()) {
			TermCategory cat = (TermCategory) it.next();
			Iterator it2 = TermUtil.getTerms(
					SessionManager.getManager().getSession()).iterator();
			while (it2.hasNext()) {
				OBOClass term = (OBOClass) it2.next();
				if (term.getCategories().contains(cat)) {
					break;
				}
			}
			v.add(new CategoryWrapper(cat));
		}
		catList.setData(v);
	}

	protected void saveCategories() {
		(new Exception("Called saveCategories()")).printStackTrace();
		Set v = new HashSet();
		Vector data = catList.getData();
		Set names = new HashSet();
		for (int i = 0; i < data.size(); i++) {
			CategoryWrapper rtw = (CategoryWrapper) data.get(i);
			if (names.contains(rtw.getName())) {
				JOptionPane.showMessageDialog(this, "Could not commit changes "
						+ "because multiple categories "
						+ "are using the name " + rtw.getName());
				return;
			}

			v.add(rtw.getCategory());
			names.add(rtw.getName());
		}
		Iterator it = TermUtil.getTerms(
				SessionManager.getManager().getSession()).iterator();
		while (it.hasNext()) {
			OBOClass term = (OBOClass) it.next();
			Iterator it2 = term.getCategories().iterator();
			while (it2.hasNext()) {
				TermCategory cat = (TermCategory) it2.next();
				if (!v.contains(cat)) {
					JOptionPane.showMessageDialog(this,
							"Could not commit changes " + "because " + cat
									+ " " + "is still in use in the "
									+ "ontology");
					return;
				}
			}
		}

		Vector oldcats = new Vector(SessionManager.getManager().getSession()
				.getCategories());
		TermMacroHistoryItem item = new TermMacroHistoryItem("Category edits");
		Vector newcats = (Vector) data.clone();
		for (int i = 0; i < oldcats.size(); i++) {
			TermCategory cat = (TermCategory) oldcats.get(i);
			boolean found = false;
			for (int j = 0; j < data.size(); j++) {
				CategoryWrapper tw = (CategoryWrapper) data.get(j);
				if (tw.getCategory() == cat) {
					newcats.remove(tw);
					if (tw.isChanged()) {
						TermCategory tc = SessionManager
								.getManager()
								.getSession()
								.getObjectFactory()
								.createCategory("NEWCAT", "<new term category>");
						TermCategoryHistoryItem catitem = new TermCategoryHistoryItem(
								tw.getCategory(), tc, false, false);

						item.addItem(catitem);
					}
					found = true;
					break;
				}
			}
			if (!found) {
				TermCategoryHistoryItem catitem = new TermCategoryHistoryItem(
						cat, null, false, true);
				item.addItem(catitem);
			}
		}
		for (int i = 0; i < newcats.size(); i++) {
			CategoryWrapper cw = (CategoryWrapper) newcats.get(i);
			TermCategoryHistoryItem catitem = new TermCategoryHistoryItem(null,
					SessionManager.getManager().getSession().getObjectFactory()
							.createCategory(cw.getName(), cw.getDesc()), true,
					false);
			item.addItem(catitem);
		}
		GUIUtil.setSelections(item, SelectionManager.getGlobalSelection(),
				SelectionManager.getGlobalSelection());
		SessionManager.getManager().apply(item);
	}
}
