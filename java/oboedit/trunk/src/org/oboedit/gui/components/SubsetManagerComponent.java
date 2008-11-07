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

import org.apache.log4j.*;

public class SubsetManagerComponent extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SubsetManagerComponent.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected ListEditor subsetList;

	protected JLabel noSubsetLabel = new JLabel(
			"Select subset to edit");

	protected JButton commitButton = new JButton("Save Changes");

	protected JButton revertButton = new JButton("Revert");

	private class SubsetEditor extends JPanel implements
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

		public SubsetEditor() {
			JLabel nameLabel = new JLabel("Subset name");
			JLabel descLabel = new JLabel("Subset description");
			FocusListener listener = new FocusListener() {
				public void focusLost(FocusEvent e) {
					subsetList.commit();
				}

				public void focusGained(FocusEvent e) {
				}
			};

			nameField = new JTextField(10);
			descField = new JTextField();

			nameField.addFocusListener(listener);
			descField.addFocusListener(listener);

			setLayout(new BoxLayout(SubsetEditor.this, BoxLayout.Y_AXIS));
			add(nameLabel);
			add(nameField);
			add(Box.createVerticalStrut(10));
			add(descLabel);
			add(descField);
			add(Box.createVerticalGlue());
		}

		public void load(Object o) {
			SubsetWrapper rtw = (SubsetWrapper) o;
			nameField.setText(rtw.getName());
			descField.setText(rtw.getDesc());
		}

		public void store(Object o) {
			SubsetWrapper rtw = (SubsetWrapper) o;
			rtw.setName(nameField.getText());
			rtw.setDesc(descField.getText());
		}

		public Object createNewValue() {
			return new SubsetWrapper(SessionManager.getManager().getSession()
					.getObjectFactory().createSubset("NEWSUBSET",
							"<new term subset>"));
		}
	}

	protected String id;

	public SubsetManagerComponent(String id) {
		super(id);
		commitButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				saveSubsets();
			}
		});
		revertButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				loadSubsets();
			}
		});
	}

	@Override
	public String getName() {
		return "Subset Manager";
	}

	@Override
	public void init() {
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		setPreferredSize(new Dimension(400, 300));
		subsetList = new ListEditor(new SubsetEditor(), noSubsetLabel,
				new Vector(), true, true, true, true, false);
		Box commitBox = new Box(BoxLayout.X_AXIS);
		commitBox.add(Box.createHorizontalGlue());
		commitBox.add(commitButton);
		commitBox.add(Box.createHorizontalStrut(10));
		commitBox.add(revertButton);
		commitBox.add(Box.createHorizontalGlue());

		removeAll();
		add(subsetList);
		add(commitBox);
		validate();

		GUIUtil.addReloadListener(reloadListener);
		loadSubsets();
	}

	@Override
	public void cleanup() {
		GUIUtil.removeReloadListener(reloadListener);
	}

	protected ReloadListener reloadListener = new ReloadListener() {
		public void reload(ReloadEvent e) {
			if (!e.isFilter())
				loadSubsets();
		}
	};

	protected static class SubsetWrapper {
		protected String name;

		protected String desc;

		protected TermSubset oldcat;

		public SubsetWrapper(TermSubset cat) {
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

		public TermSubset getSubset() {
			return oldcat;
		}

		@Override
		public String toString() {
			return desc + " (" + name + ")";
		}
	}

	protected void loadSubsets() {
		Vector v = new Vector();

		Iterator it = SessionManager.getManager().getSession().getSubsets()
				.iterator();
		while (it.hasNext()) {
			TermSubset cat = (TermSubset) it.next();
			Iterator it2 = TermUtil.getTerms(
					SessionManager.getManager().getSession()).iterator();
			while (it2.hasNext()) {
				OBOClass term = (OBOClass) it2.next();
				if (term.getSubsets().contains(cat)) {
					break;
				}
			}
			v.add(new SubsetWrapper(cat));
		}
		subsetList.setData(v);
	}

	protected void saveSubsets() {
		(new Exception("Called saveSubsets()")).printStackTrace();
		Set v = new HashSet();
		Vector data = subsetList.getData();
		Set names = new HashSet();
		for (int i = 0; i < data.size(); i++) {
			SubsetWrapper rtw = (SubsetWrapper) data.get(i);
			if (names.contains(rtw.getName())) {
				JOptionPane.showMessageDialog(this, "Could not commit changes "
						+ "because multiple subsets "
						+ "are using the name " + rtw.getName());
				return;
			}

			v.add(rtw.getSubset());
			names.add(rtw.getName());
		}
		Iterator it = TermUtil.getTerms(
				SessionManager.getManager().getSession()).iterator();
		while (it.hasNext()) {
			OBOClass term = (OBOClass) it.next();
			Iterator it2 = term.getSubsets().iterator();
			while (it2.hasNext()) {
				TermSubset sub = (TermSubset) it2.next();
				if (!v.contains(sub)) {
					JOptionPane.showMessageDialog(this,
							"Could not commit changes " + "because " + sub
									+ " " + "is still in use in the "
									+ "ontology");
					return;
				}
			}
		}

		Vector oldcats = new Vector(SessionManager.getManager().getSession()
				.getSubsets());
		TermMacroHistoryItem item = new TermMacroHistoryItem("Subset edits");
		Vector newcats = (Vector) data.clone();
		for (int i = 0; i < oldcats.size(); i++) {
			TermSubset subset = (TermSubset) oldcats.get(i);
			boolean found = false;
			for (int j = 0; j < data.size(); j++) {
				SubsetWrapper tw = (SubsetWrapper) data.get(j);
				if (tw.getSubset() == subset) {
					newcats.remove(tw);
					if (tw.isChanged()) {
						TermSubset ts = SessionManager
								.getManager()
								.getSession()
								.getObjectFactory()
								.createSubset("NEWSUBSET", "<new term subset>");
						TermSubsetHistoryItem subsetitem = new TermSubsetHistoryItem(
								tw.getSubset(), ts, false, false);

						item.addItem(subsetitem);
					}
					found = true;
					break;
				}
			}
			if (!found) {
				TermSubsetHistoryItem subsetitem = new TermSubsetHistoryItem(
						subset, null, false, true);
				item.addItem(subsetitem);
			}
		}
		for (int i = 0; i < newcats.size(); i++) {
			SubsetWrapper cw = (SubsetWrapper) newcats.get(i);
			TermSubsetHistoryItem subsetitem = new TermSubsetHistoryItem(null,
					SessionManager.getManager().getSession().getObjectFactory()
							.createSubset(cw.getName(), cw.getDesc()), true,
					false);
			item.addItem(subsetitem);
		}
		GUIUtil.setSelections(item, SelectionManager.getGlobalSelection(),
				SelectionManager.getGlobalSelection());
		SessionManager.getManager().apply(item);
	}
}
