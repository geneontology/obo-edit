package org.oboedit.gui.components;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.GUIManager;
import org.bbop.swing.*;
import org.obo.dataadapter.*;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.filters.*;
import org.obo.identifier.DefaultIDGenerator;
import org.obo.identifier.IDProfile;
import org.obo.identifier.IDRule;
import org.obo.util.IDUtil;
import org.oboedit.controller.IDManager;
import org.oboedit.gui.*;
import org.oboedit.gui.event.ReconfigEvent;

import javax.swing.*;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.*;
import java.io.*;
import java.beans.*;
import java.util.*;

import org.apache.log4j.*;

public class IDManagerComponent extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IDManagerComponent.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected static Icon customizeIcon = Preferences
			.loadLibraryIcon("customize.gif");
	protected static Icon minusIcon = Preferences.loadLibraryIcon("minus.gif");
	protected static Icon plusIcon = Preferences.loadLibraryIcon("plus.gif");

	protected JComboBox profileSelector = new JComboBox();
	protected JButton configureButton = new JButton(customizeIcon);
	protected JButton addButton = new JButton(plusIcon);
	protected JButton removeButton = new JButton(minusIcon);

	protected IDProfile currentProfile;
	protected List profiles = null;

	protected ListEditor idList;
	protected JTextField nameField = new JTextField();
	protected JTextField defaultRuleField = new JTextField();
	protected JButton commitButton = new JButton("Commit");
	protected JDialog dialog = new JDialog((JFrame) null);

	protected ActionListener selectorListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			IDProfile profile = (IDProfile) profileSelector.getSelectedItem();
			selectProfile(profile);
			saveProfileConfig();
		}
	};

	public IDManagerComponent(String id) {
		super(id);

		profileSelector.addActionListener(selectorListener);
		configureButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				configure();
			}
		});
		commitButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					saveConfiguration();
					saveProfileConfig();
					dialog.setVisible(false);
				} catch (Exception ex) {
					JOptionPane.showMessageDialog(GUIManager.getManager()
							.getFrame(), ex.getMessage());
				}
			}
		});
		addButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				(new Exception("Add button pressed")).printStackTrace();
				IDProfile profile = new NamedIDProfile("<new id profile>");
				if (profiles.contains(profile)) {
					int counter = 0;
					do {
						counter++;
						profile.setName("<new id profile " + counter + ">");
					} while (profiles.contains(profile));
				}
				profiles.add(profile);
				loadProfiles();
				profileSelector.setSelectedItem(profile);
			}
		});
		removeButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				int index = profiles.indexOf(profileSelector.getSelectedItem());
				IDProfile profileToDelete = (IDProfile) profiles.get(index);
				IDProfile nextProfile = null;
				if (index == 0)
					nextProfile = (IDProfile) profiles.get(index + 1);
				else
					nextProfile = (IDProfile) profiles.get(index - 1);

				selectProfile(nextProfile);
				profiles.remove(profileToDelete);
				saveProfileConfig();

				loadProfiles();
			}
		});
	}

	@Override
	public void init() {

		Box buttonBox = new Box(BoxLayout.X_AXIS);

		setLayout(new BorderLayout());
		add(buttonBox, "North");

		JLabel noRuleLabel = new JLabel("Press 'Add' to create a new rule");
		idList = new ListEditor(new IDRuleEditor(), noRuleLabel, new Vector(),
				true, true, true, true, false);

		JLabel nameLabel = new JLabel("Profile name");

		JLabel defaultRuleLabel = new JLabel("Default rule");

		Box namePanel = new Box(BoxLayout.X_AXIS);
		namePanel.add(nameLabel);
		namePanel.add(Box.createHorizontalStrut(10));
		namePanel.add(nameField);

		Box defaultRulePanel = new Box(BoxLayout.X_AXIS);
		defaultRulePanel.add(defaultRuleLabel);
		defaultRulePanel.add(Box.createHorizontalStrut(10));
		defaultRulePanel.add(defaultRuleField);

		JPanel configPanel = new JPanel();
		configPanel.setLayout(new BoxLayout(configPanel, BoxLayout.Y_AXIS));
		configPanel.add(namePanel);
		configPanel.add(defaultRulePanel);
		configPanel.add(idList);
		configPanel.add(commitButton);
		dialog.setContentPane(configPanel);

		loadProfileConfig();
		loadProfiles();

		configureButton.setPreferredSize(new Dimension(20, 20));
		addButton.setPreferredSize(new Dimension(20, 20));
		removeButton.setPreferredSize(new Dimension(20, 20));

		profileSelector.setMinimumSize(new Dimension(200, 5));
		profileSelector.setPreferredSize(new Dimension(200, 20));

		profileSelector.setSelectedItem(currentProfile);
		selectProfile(currentProfile);
		/*
		 * Box buttonBox = new Box(BoxLayout.X_AXIS);
		 * buttonBox.add(Box.createHorizontalGlue());
		 * buttonBox.add(Box.createHorizontalStrut(20));
		 * buttonBox.add(configureButton);
		 * buttonBox.add(Box.createHorizontalStrut(20));
		 * buttonBox.add(Box.createHorizontalGlue());
		 */
		buttonBox.add(profileSelector);
		buttonBox.add(Box.createHorizontalStrut(10));
		buttonBox.add(configureButton);
		buttonBox.add(Box.createHorizontalStrut(2));
		buttonBox.add(addButton);
		buttonBox.add(Box.createHorizontalStrut(2));
		buttonBox.add(removeButton);
	}

	protected void saveProfileConfig() {
		try {
			File profileFile = new File(GUIManager.getPrefsDir(),
					"idprofiles.xml");
			XMLEncoder encoder = new XMLEncoder(new FileOutputStream(
					profileFile));
			encoder.writeObject(profiles);
			encoder.writeObject(currentProfile);
			encoder.close();
		} catch (IOException ex) {
			ex.printStackTrace();
		}
	}

	protected boolean testRule(String rule) {
		return IDUtil.parseVarString(rule) != null;
	}

	protected void saveConfiguration() throws Exception {
		idList.commit();
		if (!testRule(defaultRuleField.getText())) {
			throw new Exception("Syntax error in default rule \""
					+ defaultRuleField.getText() + "\"");
		}
		Iterator it = idList.getData().iterator();
		while (it.hasNext()) {
			IDRule rule = (IDRule) it.next();
			if (!testRule(rule.getRule())) {
				throw new Exception("Syntax error in filtered rule \""
						+ rule.getRule() + "\"");
			}
		}

		currentProfile.setName(nameField.getText());
		currentProfile.setDefaultRule(defaultRuleField.getText());
		currentProfile.getRules().clear();
		currentProfile.getRules().addAll(idList.getData());

		profileSelector.repaint();
	}

	protected void configure() {
		Vector v = new Vector();
		v.addAll(currentProfile.getRules());
		idList.setData(v);
		nameField.setText(currentProfile.getName());
		defaultRuleField.setText(currentProfile.getDefaultRule());

		dialog.pack();
		dialog.setVisible(true);
	}

	protected void selectProfile(IDProfile profile) {
		logger.info("selecting profile " + profile);
		if (profile == null)
			(new Exception("Selected null profile")).printStackTrace();
		currentProfile = profile;

		if (IDManager.getManager().getIDAdapter() instanceof DefaultIDGenerator) {
			DefaultIDGenerator idGen = (DefaultIDGenerator) IDManager
					.getManager().getIDAdapter();
			idGen.setProfile(profile);
		}
		Preferences.getPreferences().fireReconfigEvent(new ReconfigEvent(this));
	}

	@Override
	public String getName() {
		return "ID Manager Plugin";
	}

	protected void loadProfileConfig() {
		// currentProfile = loadCurrentProfile();
		profiles = IDManager.loadIDProfiles();

		if (IDManager.getManager().getIDAdapter() instanceof DefaultIDGenerator) {
			DefaultIDGenerator idGen = (DefaultIDGenerator) IDManager
					.getManager().getIDAdapter();
			currentProfile = idGen.getProfile();
		}
	}

	protected void loadProfiles() {
		profileSelector.removeActionListener(selectorListener);
		profileSelector.removeAllItems();
		boolean matched = false;

		IDProfile currentProfile = null;
		if (IDManager.getManager().getIDAdapter() instanceof DefaultIDGenerator) {
			DefaultIDGenerator idGen = (DefaultIDGenerator) IDManager
					.getManager().getIDAdapter();
			currentProfile = idGen.getProfile();
		}

		Iterator it = profiles.iterator();
		while (it.hasNext()) {
			IDProfile profile = (IDProfile) it.next();

			if (IDUtil.equals(currentProfile, profile))
				matched = true;

			profileSelector.addItem(profile);
		}

		if (!matched) {
			profileSelector.addItem(currentProfile);
			profiles.add(currentProfile);
		}
		profileSelector.addActionListener(selectorListener);
		removeButton.setEnabled(profiles.size() > 1);
		// write this later
	}

	private class IDRuleEditor extends JPanel implements GenericEditorComponent {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		protected JTextField ruleField = new JTextField();
		protected FilterComponent objectFilterEditor = new FilterComponent(
				new TermFilterEditorFactory());

		protected ListEditor editor;

		public void setMasterComponent(Component c) {
			if (c instanceof ListEditor)
				editor = (ListEditor) c;
		}

		public IDRuleEditor() {

			JLabel filterLabel = new JLabel("Parent match filter:");

			JPanel filterPanel = new JPanel();
			filterPanel.setLayout(new BorderLayout());
			filterPanel.add(filterLabel, "North");
			filterPanel.add(objectFilterEditor, "Center");

			JLabel ruleLabel = new JLabel("ID Rule");

			Box ruleBox = new Box(BoxLayout.X_AXIS);
			ruleBox.add(ruleLabel);
			ruleBox.add(Box.createHorizontalStrut(10));
			ruleBox.add(ruleField);

			setLayout(new BorderLayout());
			add(ruleBox, "South");
			add(filterPanel, "Center");
		}

		public void load(Object o) {
			IDRule rule = (IDRule) o;
			objectFilterEditor.setFilter(rule.getFilter());
			ruleField.setText(rule.getRule());
		}

		public void store(Object o) {
			IDRule rule = (IDRule) o;
			rule.setFilter(objectFilterEditor.getFilter());
			rule.setRule(ruleField.getText());
		}

		public Object createNewValue() {
			IDRule rule = new DefaultIDRule();
			CompoundFilter filter = (CompoundFilter) (new CompoundFilterFactory())
					.createNewFilter();
			filter.addFilter((new ObjectFilterFactory()).createNewFilter());
			rule.setFilter(filter);
			return rule;
		}
	}

}
