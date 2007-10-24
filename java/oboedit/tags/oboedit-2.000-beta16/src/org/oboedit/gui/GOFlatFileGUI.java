package org.oboedit.gui;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.tree.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import org.bbop.dataadapter.*;
import org.bbop.swing.*;
import org.bbop.util.*;
import org.obo.dataadapter.GOFlatFileAdapter;
import org.obo.dataadapter.OBOAdapter;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;

public class GOFlatFileGUI extends JPanel implements GraphicalUI {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	protected Preferences preferences = Preferences.getPreferences();

	private class RelationshipTypeEditor extends JPanel implements
			GenericEditorComponent {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		private JTextField charField;
		private JTextField nameField;
		private JTextField descField;

		protected ListEditor editor;

		public void setMasterComponent(Component c) {
			if (c instanceof ListEditor)
				editor = (ListEditor) c;
		}

		public RelationshipTypeEditor() {
			setOpaque(false);
			JLabel charLabel = new JLabel("Relationship type symbol");
			JLabel nameLabel = new JLabel("Relationship type name");
			JLabel descLabel = new JLabel("Relationship type description");
			FocusListener listener = new FocusListener() {
				public void focusLost(FocusEvent e) {
					symbolList.commit();
				}

				public void focusGained(FocusEvent e) {
				}
			};

			charField = new JTextField(1);
			nameField = new JTextField(10);
			descField = new JTextField();

			charField.addFocusListener(listener);
			nameField.addFocusListener(listener);
			descField.addFocusListener(listener);

			setLayout(new BoxLayout(RelationshipTypeEditor.this,
					BoxLayout.Y_AXIS));
			add(charLabel);
			add(charField);
			add(Box.createVerticalStrut(10));
			add(nameLabel);
			add(nameField);
			add(Box.createVerticalStrut(10));
			add(descLabel);
			add(descField);
			add(Box.createVerticalGlue());
		}

		public void load(Object o) {
			GOFlatFileAdapter.CharTypeMapping rtw = (GOFlatFileAdapter.CharTypeMapping) o;
			charField.setText(rtw.getTypeChar() + "");
			nameField.setText(rtw.getPropertyID());
			descField.setText(rtw.getPropertyName());
		}

		public void store(Object o) {
			GOFlatFileAdapter.CharTypeMapping rtw = (GOFlatFileAdapter.CharTypeMapping) o;

			String charStr = charField.getText();
			if (charStr.length() < 1)
				rtw.setTypeChar("?");
			else
				rtw.setTypeChar(charStr);
			rtw.setPropertyID(nameField.getText());
			rtw.setPropertyName(descField.getText());
		}

		public Object createNewValue() {
			GOFlatFileAdapter.CharTypeMapping ctw = new GOFlatFileAdapter.CharTypeMapping(
					"?", "NEWTYPE", "<new relationship type>");
			return ctw;
		}
	}

	public class SaveRenderer extends DefaultListCellRenderer {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		@Override
		public Component getListCellRendererComponent(JList list, Object value,
				int index, boolean isSelected, boolean cellHasFocus) {
			GOFlatFileAdapter.SaveRecord sr = (GOFlatFileAdapter.SaveRecord) value;
			String id = sr.getID();
			if (id == null)
				id = "ROOT";
			return super.getListCellRendererComponent(list, id, index,
					isSelected, cellHasFocus);
		}
	}

	public class TermBrowser extends JDialog {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		protected JTree tree;
		protected JButton okButton = new JButton("Ok");

		public TermBrowser() {
			super((Frame) null, "Select a term", true);
			tree = new JTree();
			DefaultTermModel model = new DefaultTermModel();
			model.setShowTerms(true);
			model.setShowTypes(false);
			model.setShowObsoletes(false);
			model.reload();
			tree.setModel(model);
			tree.setCellRenderer(new OBOCellRenderer());
			tree.setRootVisible(false);
			okButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					dispose();
				}
			});
			tree.getSelectionModel().setSelectionMode(
					TreeSelectionModel.SINGLE_TREE_SELECTION);

			JPanel panel = new JPanel();
			panel.setLayout(new BorderLayout());
			JScrollPane pane = new JScrollPane(tree);

			panel.add(pane, "Center");
			panel.add(okButton, "South");
			panel.setBackground(Color.white);
			setContentPane(panel);
			tree.expandRow(0);
		}

		public OBOClass getSelectedTerm() {
			TreePath selected = tree.getSelectionPath();
			if (selected == null)
				return null;
			return (OBOClass) ((Link) selected.getLastPathComponent())
					.getChild();
		}
	}

	private class SaveItemEditor extends JPanel implements
			GenericEditorComponent {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		protected JRadioButton rootSave = new JRadioButton("Save at ROOT");
		protected JRadioButton nodeSave = new JRadioButton("Save at");
		protected JTextField idField = new JTextField(15);
		protected JButton nodeBrowseButton = new JButton("Browse...");
		protected JButton fileBrowseButton = new JButton("Browse...");
		protected JTextField filenameField = new JTextField();
		protected JLabel filenameLabel = new JLabel("Filename");
		protected ListEditor editor;
		protected JPanel idPanel;
		protected JPanel filenamePanel;

		private class CommitListener implements ActionListener, FocusListener {
			public void actionPerformed(ActionEvent e) {
				editor.commit();
			}

			public void focusLost(FocusEvent e) {
				editor.commit();
			}

			public void focusGained(FocusEvent e) {
			}
		}

		protected class RadioListener implements ActionListener {
			protected boolean root = false;

			public RadioListener(boolean root) {
				this.root = root;
			}

			public void actionPerformed(ActionEvent e) {
				idPanel.setEnabled(!root);
				idField.setEnabled(!root);
				nodeBrowseButton.setEnabled(!root);
				if (root)
					editor.commit();
			}
		}

		protected void formatField(JTextField field) {
			field.setMaximumSize(new Dimension(Integer.MAX_VALUE, 20));
		}

		public SaveItemEditor() {
			ButtonGroup bg = new ButtonGroup();
			bg.add(rootSave);
			bg.add(nodeSave);
			setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

			rootSave.addActionListener(new RadioListener(true));
			nodeSave.addActionListener(new RadioListener(false));
			rootSave.setOpaque(false);
			nodeSave.setOpaque(false);

			CommitListener cl = new CommitListener();

			idField.addFocusListener(cl);
			idField.addActionListener(cl);
			filenameField.addFocusListener(cl);
			filenameField.addActionListener(cl);

			fileBrowseButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					File startFile = new File(System.getProperty("user.home"));
					if (filenameField.getText().length() > 0) {
						File temp = new File(filenameField.getText());
						if (temp.exists()) {
							if (temp.isDirectory())
								startFile = temp;
							else
								startFile = temp.getParentFile();
						} else {
							temp = temp.getParentFile();
							if (temp != null && temp.isDirectory())
								startFile = temp;
						}
					}
					JFileChooser chooser = new JFileChooser(startFile);
					if (chooser.showSaveDialog(GOFlatFileGUI.this) == JFileChooser.APPROVE_OPTION) {
						File file = chooser.getSelectedFile();
						filenameField.setText(file.toString());
						editor.commit();
					}
				}
			});

			nodeBrowseButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					TermBrowser tb = new TermBrowser();
					tb.setSize(300, 300);
					tb.show();
					OBOClass t = tb.getSelectedTerm();
					if (t == null)
						idField.setText("");
					else
						idField.setText(t.getID());
				}
			});

			formatField(filenameField);
			formatField(idField);

			idPanel = new JPanel();
			idPanel.setOpaque(false);
			idPanel.setLayout(new BoxLayout(idPanel, BoxLayout.X_AXIS));
			idPanel.add(nodeSave);
			idPanel.add(Box.createHorizontalStrut(20));
			idPanel.add(idField);
			idPanel.add(Box.createHorizontalStrut(10));
			idPanel.add(nodeBrowseButton);

			filenamePanel = new JPanel();
			filenamePanel.setOpaque(false);
			filenamePanel.setLayout(new BoxLayout(filenamePanel,
					BoxLayout.X_AXIS));
			filenamePanel.add(filenameField);
			filenamePanel.add(Box.createHorizontalStrut(20));
			filenamePanel.add(fileBrowseButton);

			idPanel.setAlignmentX(JComponent.LEFT_ALIGNMENT);
			filenameLabel.setAlignmentX(JComponent.LEFT_ALIGNMENT);
			idField.setAlignmentX(JComponent.LEFT_ALIGNMENT);
			nodeBrowseButton.setAlignmentX(JComponent.LEFT_ALIGNMENT);
			fileBrowseButton.setAlignmentX(JComponent.LEFT_ALIGNMENT);
			filenameField.setAlignmentX(JComponent.LEFT_ALIGNMENT);
			rootSave.setAlignmentX(JComponent.LEFT_ALIGNMENT);
			nodeSave.setAlignmentX(JComponent.LEFT_ALIGNMENT);
			filenamePanel.setAlignmentX(JComponent.LEFT_ALIGNMENT);

			add(rootSave);
			add(idPanel);
			add(filenameLabel);
			add(filenamePanel);
			add(Box.createVerticalGlue());
		}

		public Object createNewValue() {
			return new GOFlatFileAdapter.SaveRecord(null, "");
		}

		public void load(java.lang.Object o) {
			GOFlatFileAdapter.SaveRecord sr = (GOFlatFileAdapter.SaveRecord) o;
			if (sr.getID() == null) {
				rootSave.setSelected(true);
				idField.setText("");
			} else {
				nodeSave.setSelected(true);
				idField.setText(sr.getID());
			}
			filenameField.setText(sr.getFilename());
		}

		public void setMasterComponent(Component c) {
			if (c instanceof ListEditor)
				editor = (ListEditor) c;
		}

		public void store(java.lang.Object o) {
			GOFlatFileAdapter.SaveRecord sr = (GOFlatFileAdapter.SaveRecord) o;
			if (rootSave.isSelected())
				sr.setID(null);
			else {
				sr.setID(idField.getText());
			}
			sr.setFilename(filenameField.getText());
		}
	}

	private class FilenameEditor extends JPanel implements
			GenericEditorComponent {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		private JTextField filenameField;
		private JButton browseButton;

		protected ListEditor editor;

		public void setMasterComponent(Component c) {
			if (c instanceof ListEditor)
				editor = (ListEditor) c;
		}

		public FilenameEditor() {
			JLabel filenameLabel = new JLabel("File Name or URL");

			FocusListener listener = new FocusListener() {
				public void focusLost(FocusEvent e) {
					editor.commit();
				}

				public void focusGained(FocusEvent e) {
				}
			};

			filenameField = new JTextField();
			browseButton = new JButton("Browse...");

			browseButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					File startFile = new File(System.getProperty("user.home"));
					if (filenameField.getText().length() > 0) {
						File temp = new File(filenameField.getText());
						if (temp.exists()) {
							if (temp.isDirectory())
								startFile = temp;
							else
								startFile = temp.getParentFile();
						} else {
							temp = temp.getParentFile();
							if (temp != null && temp.isDirectory())
								startFile = temp;
						}
					}
					JFileChooser chooser = new JFileChooser(startFile);
					if (chooser.showOpenDialog(GOFlatFileGUI.this) == JFileChooser.APPROVE_OPTION) {
						File file = chooser.getSelectedFile();
						filenameField.setText(file.toString());
						editor.commit();
					}
				}
			});

			filenameField.addFocusListener(listener);

			filenameField.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					editor.commit();
				}
			});

			JPanel fieldBox = new JPanel();
			fieldBox.setLayout(new BoxLayout(fieldBox, BoxLayout.X_AXIS));
			fieldBox.setOpaque(false);

			fieldBox.add(filenameField);
			fieldBox.add(Box.createHorizontalStrut(20));
			fieldBox.add(browseButton);

			fieldBox.setAlignmentX(JComponent.LEFT_ALIGNMENT);
			filenameLabel.setAlignmentX(JComponent.LEFT_ALIGNMENT);
			filenameField.setAlignmentX(JComponent.LEFT_ALIGNMENT);
			browseButton.setAlignmentX(JComponent.LEFT_ALIGNMENT);

			filenameLabel.setAlignmentY(JComponent.TOP_ALIGNMENT);
			fieldBox.setAlignmentY(JComponent.TOP_ALIGNMENT);
			/*
			 * setPreferredSize(new Dimension((int)
			 * getPreferredSize().getWidth(), 150));
			 */
			JPanel panel = new JPanel();
			panel.setOpaque(false);
			panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
			panel.add(filenameLabel);
			panel.add(fieldBox);
			setLayout(new BorderLayout());
			add(panel, "North");
		}

		public void load(Object o) {
			filenameField.setText(o.toString());
		}

		public void store(Object o) {
			((EditableString) o).setValue(filenameField.getText());
		}

		public Object createNewValue() {
			return new EditableString("<new file>");
		}
	}

	DataAdapter driver;
	IOOperation op;

	JButton commitButton;
	JButton cancelButton;
	JButton browseDefButton;

	JLabel definitionLabel;
	JButton advancedButton;
	JScrollPane commentPane;

	ListEditor fileList;
	JTextField filenameField;
	JTextField definitionField;
	JTextArea commentField;

	ListEditor saveList;

	JCheckBox hideDownstreamBox;
	JCheckBox allowCyclesBox;
	JCheckBox allowDanglingBox;
	JCheckBox reduceSizeBox;
	JCheckBox useLegacyTypesBox;
	JCheckBox translateTypesBox;

	Vector fileNames;

	// Vector relationshipTypes;

	ListEditor symbolList;
	Properties props;
	JDialog optionDialog;

	boolean notListPick = false;
	int currentSelectedIndex = -1;

	JPanel commitBox;

	protected GOFlatFileAdapter.GOFlatFileConfiguration config;
	protected UIConfiguration uiconfig;
	protected GraphicalUI simpleUI;

	protected boolean embed = false;

	public void init(AdapterWidgetI widget, IOOperation op,
			DataAdapter adapter, Object input) {
		this.driver = adapter;
		this.op = op;

		removeAll();

		JPanel advancedButtonPanel = new JPanel();
		advancedButtonPanel.setOpaque(false);
		advancedButtonPanel.setLayout(new BoxLayout(advancedButtonPanel,
				BoxLayout.X_AXIS));
		advancedButtonPanel.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		advancedButtonPanel.add(Box.createHorizontalGlue());
		advancedButtonPanel.add(advancedButton);
		advancedButtonPanel.add(Box.createHorizontalGlue());

		JPanel advancedBox = new JPanel();
		advancedBox.setOpaque(false);
		advancedBox.setLayout(new BoxLayout(advancedBox, BoxLayout.X_AXIS));
		advancedBox.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		advancedBox.add(advancedButtonPanel);
		advancedBox.add(Box.createVerticalGlue());

		JPanel definitionLabelBox = new JPanel();
		definitionLabelBox.setOpaque(false);
		definitionLabelBox.setLayout(new BoxLayout(definitionLabelBox,
				BoxLayout.X_AXIS));
		definitionLabelBox.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		definitionLabelBox.add(definitionLabel);
		definitionLabelBox.add(Box.createHorizontalGlue());

		JPanel definitionNameBox = new JPanel();
		definitionNameBox.setOpaque(false);
		definitionNameBox.setLayout(new BoxLayout(definitionNameBox,
				BoxLayout.X_AXIS));
		definitionNameBox.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		definitionNameBox.add(definitionField);
		definitionNameBox.add(Box.createHorizontalStrut(10));
		definitionNameBox.add(browseDefButton);
		definitionNameBox.add(Box.createHorizontalGlue());

		advancedButton.setAlignmentX(JComponent.CENTER_ALIGNMENT);

		if (op.equals(OBOAdapter.READ_ONTOLOGY)) {
			add(fileList);
		}
		JPanel filenamePanel = new JPanel();
		filenamePanel.setOpaque(false);
		filenamePanel.setLayout(new BoxLayout(filenamePanel, BoxLayout.Y_AXIS));
		JButton filenameBrowseButton = new JButton("Browse...");

		filenameBrowseButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				JFileChooser chooser = new JFileChooser();
				if (chooser.showOpenDialog(GOFlatFileGUI.this) == JFileChooser.APPROVE_OPTION) {
					File file = chooser.getSelectedFile();
					filenameField.setText(file.toString());
				}
			}
		});

		JLabel filenameLabel = new JLabel("File name");
		/*
		 * filenamePanel.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		 */
		JPanel filenameButtonPanel = new JPanel();
		filenameButtonPanel.setOpaque(false);
		filenameButtonPanel.setLayout(new BoxLayout(filenameButtonPanel,
				BoxLayout.X_AXIS));
		filenameButtonPanel.add(filenameField);
		filenameButtonPanel.add(Box.createHorizontalStrut(10));
		filenameButtonPanel.add(filenameBrowseButton);

		filenamePanel.add(filenameLabel);
		filenamePanel.add(filenameButtonPanel);

		fileList.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		filenamePanel.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		filenameLabel.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		filenameButtonPanel.setAlignmentX(JComponent.LEFT_ALIGNMENT);

		if (op.equals(OBOAdapter.WRITE_ONTOLOGY)) {
			add(saveList);
		}
		add(Box.createVerticalStrut(20));

		add(definitionLabelBox);
		add(definitionNameBox);

		add(Box.createVerticalStrut(10));
		add(advancedBox);

		JPanel optionBox = new JPanel();
		optionBox.setOpaque(false);
		optionBox.setLayout(new BoxLayout(optionBox, BoxLayout.X_AXIS));
		optionBox.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		optionBox.add(Box.createHorizontalGlue());

		boolean showingOptions = false;

		if (op.equals(OBOAdapter.READ_ONTOLOGY)) {
			showingOptions = true;
			optionBox.add(hideDownstreamBox);
			optionBox.add(Box.createHorizontalGlue());
		}

		showingOptions = true;
		optionBox.add(allowCyclesBox);
		optionBox.add(Box.createHorizontalGlue());

		optionBox.add(allowDanglingBox);
		optionBox.add(Box.createHorizontalGlue());
		optionBox.add(translateTypesBox);
		optionBox.add(Box.createHorizontalGlue());

		if (op.equals(OBOAdapter.WRITE_ONTOLOGY)) {
			showingOptions = true;
			optionBox.add(reduceSizeBox);
			optionBox.add(Box.createHorizontalGlue());
			optionBox.add(useLegacyTypesBox);
			optionBox.add(Box.createHorizontalGlue());
		}

		if (showingOptions) {
			add(Box.createVerticalStrut(20));

			add(optionBox);
		}

		if (op.equals(OBOAdapter.WRITE_ONTOLOGY)) {

			commentField.setText(SessionManager.getManager().getSession().getCurrentHistory()
					.getComment());

			JLabel commentLabel = new JLabel("Comment");
			commentLabel.setAlignmentX(JComponent.LEFT_ALIGNMENT);

			JPanel commentBox = new JPanel();
			commentBox.setOpaque(false);
			commentBox.setLayout(new BoxLayout(commentBox, BoxLayout.X_AXIS));
			commentBox.setAlignmentX(JComponent.LEFT_ALIGNMENT);
			commentBox.add(commentLabel);
			commentBox.add(Box.createHorizontalGlue());

			add(Box.createVerticalStrut(20));
			add(commentBox);
			add(commentPane);
		}

		add(Box.createVerticalStrut(20));

	}

	public void setConfiguration(AdapterConfiguration c) {
		if (c instanceof GOFlatFileAdapter.GOFlatFileConfiguration) {
			this.config = (GOFlatFileAdapter.GOFlatFileConfiguration) c;
			Vector v = new Vector();
			Iterator it = config.getReadPaths().iterator();
			while (it.hasNext()) {
				String s = (String) it.next();
				v.add(new EditableString(s));
			}
			fileList.setData(v);

			v = new Vector();
			v.addAll(config.getSaveRecords());
			saveList.setData(v);

			if (op.equals(OBOAdapter.READ_ONTOLOGY)) {
				if (config.getDefFilename() != null)
					definitionField.setText(config.getDefFilename());
			} else if (op.equals(OBOAdapter.WRITE_ONTOLOGY)) {
				if (config.getSaveDefFilename() != null)
					definitionField.setText(config.getSaveDefFilename());
			}

			hideDownstreamBox.setSelected(config.getHideDownstream());
			allowCyclesBox.setSelected(config.getAllowCycles());
			allowDanglingBox.setSelected(config.getAllowDangling());
			reduceSizeBox.setSelected(config.getReduceSize());
			useLegacyTypesBox.setSelected(config.getUseLegacyTypes());
			translateTypesBox.setSelected(config.getTranslateTypes());

			v = new Vector();
			v.addAll(config.getTypeMappings());
			symbolList.setData(v);
		}
	}

	public AdapterConfiguration getConfig(IOOperation op, DataAdapter adapter,
			Object input) throws DataAdapterUIException {
		return config;
	}

	public AdapterConfiguration createEmptyConfig() {
		return new GOFlatFileAdapter.GOFlatFileConfiguration();
	}

	public void cleanup() {
	}

	public UIConfiguration getUIConfiguration() {
		return uiconfig;
	}

	public void setUIConfiguration(UIConfiguration uiconfig) {
		this.uiconfig = uiconfig;
	}

	public void setEmbed(boolean embed) {
		this.embed = embed;
		if (embed) {
			remove(commitBox);
			validate();
		}
	}

	protected void showAdvancedOptions() {
		JPanel advancedPanel = getAdvancedPanel(new JPanel());
		optionDialog = new JDialog();
		optionDialog.setModal(true);
		optionDialog.setContentPane(advancedPanel);
		optionDialog.setTitle("Advanced flat file options");
		optionDialog.pack();
		optionDialog.show();
		SwingUtil.center(optionDialog);
	}

	public GOFlatFileGUI() {
		// this.op = op;

		// setBackground(Preferences.defaultBackgroundColor());

		JLabel noRelationshipLabel = new JLabel("Click a symbol to edit it.");
		symbolList = new ListEditor(new RelationshipTypeEditor(),
				noRelationshipLabel, new Vector(), true, true, true, true,
				false);

		JLabel noFileLabel = new JLabel("Click a filename to edit it.");
		fileList = new ListEditor(new FilenameEditor(), noFileLabel,
				new Vector(), true, true, true, true, false);
		fileList.setDefaultDividerLoc(70);
		fileList.setListSize(100);

		Vector firstvector = new Vector();
		firstvector.add(new GOFlatFileAdapter.SaveRecord(null, ""));
		saveList = new ListEditor(new SaveItemEditor(), noFileLabel,
				new Vector(), true, true, true, true, false);
		saveList.getList().setCellRenderer(new SaveRenderer());
		saveList.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		saveList.setListSize(140);

		TitledBorder fileListBorder = new TitledBorder("File paths");
		fileList.setBorder(fileListBorder);
		fileList.setOrientation(ListEditor.VERTICAL_SPLIT);

		definitionField = new JTextField();
		filenameField = new JTextField();

		commentField = new JTextArea(4, 30);
		commentField.setWrapStyleWord(true);
		commentField.setLineWrap(true);
		commentField.setAlignmentX(JComponent.LEFT_ALIGNMENT);
		commentPane = new JScrollPane(commentField,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		commentPane.setAlignmentX(JComponent.LEFT_ALIGNMENT);

		filenameField.setPreferredSize(new Dimension(300, 20));
		definitionField.setPreferredSize(new Dimension(300, 20));

		hideDownstreamBox = new JCheckBox("Hide downstream errors", true);
		allowCyclesBox = new JCheckBox("Allow cycles", false);
		allowDanglingBox = new JCheckBox("Allow dangling parent references",
				false);
		reduceSizeBox = new JCheckBox("Reduce file size", false);
		useLegacyTypesBox = new JCheckBox("Use legacy compatible types", false);
		translateTypesBox = new JCheckBox("Translate type ids", true);

		hideDownstreamBox.setOpaque(false);
		allowCyclesBox.setOpaque(false);
		allowDanglingBox.setOpaque(false);
		reduceSizeBox.setOpaque(false);
		useLegacyTypesBox.setOpaque(false);
		translateTypesBox.setOpaque(false);

		browseDefButton = new JButton("Browse...");
		browseDefButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				File startFile = new File(System.getProperty("user.home"));
				if (definitionField.getText().length() > 0) {
					File temp = new File(definitionField.getText());
					if (temp.exists()) {
						if (temp.isDirectory())
							startFile = temp;
						else
							startFile = temp.getParentFile();
					} else {
						temp = temp.getParentFile();
						if (temp != null && temp.isDirectory())
							startFile = temp;
					}
				}
				JFileChooser chooser = new JFileChooser(startFile);
				if (chooser.showOpenDialog(GOFlatFileGUI.this) == JFileChooser.APPROVE_OPTION) {
					File file = chooser.getSelectedFile();
					definitionField.setText(file.toString());
				}
			}
		});

		commitButton = new JButton("Ok");
		cancelButton = new JButton("Cancel");
		advancedButton = new JButton("Advanced Options...");

		advancedButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				showAdvancedOptions();
			}
		});

		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		definitionLabel = new JLabel("Definition file name:");

		setOpaque(false);

	}

	protected void commitAdvancedChanges() {

		Vector symbols = symbolList.getData();
		Hashtable newBindings = new Hashtable();
		if (symbols.size() < 1) {
			JOptionPane.showMessageDialog(null,
					"You must specify at least one "
							+ "term relationship type.");
			return;
		}

		HashMap nameList = new HashMap();
		for (int i = 0; i < symbols.size(); i++) {
			GOFlatFileAdapter.CharTypeMapping rtw = (GOFlatFileAdapter.CharTypeMapping) symbols
					.elementAt(i);
			if (newBindings.get(rtw.getTypeChar()) != null) {
				JOptionPane.showMessageDialog(null,
						"Each relationship must have a "
								+ "unique symbol. You may not reuse "
								+ "the character '" + rtw.getTypeChar() + "'.");
				return;
			}
			if ((rtw.getTypeChar().length() == 1 && GOFlatFileAdapter
					.isReservedCharacter(rtw.getTypeChar().charAt(0)))
					|| (rtw.getTypeChar().length() != 1 && !(rtw.getTypeChar()
							.charAt(0) == GOFlatFileAdapter.BOUNDARY_CHAR && rtw
							.getTypeChar().charAt(
									rtw.getTypeChar().length() - 1) == GOFlatFileAdapter.BOUNDARY_CHAR))) {
				JOptionPane.showMessageDialog(null, rtw.getTypeChar()
						+ " is a " + "reserved character, and "
						+ "cannot be used as a " + "relationship type "
						+ "identifier.");
				return;
			}

			if (rtw.getPropertyID().length() < 1) {
				JOptionPane.showMessageDialog(null,
						"Each relationship must have a " + "name!");
				return;
			}
			if (rtw.getPropertyName().length() < 1) {
				JOptionPane.showMessageDialog(null,
						"Each relationship must have a " + "description!");
				return;
			}
			if (nameList.get(rtw.getPropertyID()) != null) {
				JOptionPane.showMessageDialog(null,
						"Each relationship must have a "
								+ "unique name. You may not reuse "
								+ "the name '" + rtw.getPropertyID() + "'.");
				return;
			}

			OBOProperty trt = new OBOPropertyImpl(rtw.getPropertyID(), rtw
					.getPropertyName());
			newBindings.put(rtw.getTypeChar(), trt);

			nameList.put(rtw.getPropertyID(), rtw.getPropertyID());
		}

		config.setTypeMappings(symbols);

		if (optionDialog != null)
			optionDialog.dispose();
	}

	public JPanel getAdvancedPanel(JPanel panel) {
		panel.removeAll();
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
		JButton okButton = new JButton("Ok");
		JButton cancelButton = new JButton("Cancel");

		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				commitAdvancedChanges();
			}
		});

		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (optionDialog != null)
					optionDialog.dispose();
				/*
				 * else controllingObject.cancel();
				 */
			}
		});

		Box buttonBox = new Box(BoxLayout.X_AXIS);
		buttonBox.add(Box.createHorizontalGlue());
		buttonBox.add(okButton);
		buttonBox.add(Box.createHorizontalStrut(20));
		buttonBox.add(cancelButton);
		buttonBox.add(Box.createHorizontalGlue());

		panel.add(symbolList);
		panel.add(Box.createVerticalStrut(10));
		panel.add(buttonBox);
		panel.add(Box.createVerticalStrut(10));

		panel.setPreferredSize(new Dimension(450, 250));
		panel.setMinimumSize(new Dimension(450, 250));
		panel.setBorder(new TitledBorder("Define term relationship symbols"));
		return panel;
	}

	/*
	 * public void commit() { if (op.equals(DEDataAdapterI.CONFIGURE)) return;
	 * acceptComponentConfig(true); Vector files = (Vector) fileNames.clone();
	 * 
	 * if (files.size() < 1) { JOptionPane.showMessageDialog(null, "You must
	 * provide at least one "+ "file name."); return; }
	 * 
	 * if ((op.equals(OBOEditAdapter.READ_ONTOLOGY) ||
	 * op.equals(DEDataAdapterI.IMPORT_TERMS))) { if (config.getDefFilename() !=
	 * null && config.getDefFilename().length() > 0)
	 * files.add(config.getDefFilaneme()); } else { if
	 * (config.getSaveDefFilename() != null &&
	 * config.getSaveDefFilename().length() > 0)
	 * files.add(config.getSaveDefFilaneme()); }
	 * 
	 * int filesFound = 0; Vector foundFiles = new Vector(); Vector missedFiles =
	 * new Vector(); boolean containsURLs = false; for(int i=0; i <
	 * files.size(); i++) { String fileName; if (files.elementAt(i) instanceof
	 * GOFlatFileAdapter.SaveRecord) fileName = ((GOFlatFileAdapter.SaveRecord)
	 * files.get(i)). getFilename(); else fileName = (String) files.get(i);
	 * 
	 * try { URL url = new URL(fileName); InputStream is = url.openStream();
	 * is.close(); containsURLs = true; foundFiles.addElement(fileName); } catch
	 * (MalformedURLException e) { File file = new File(fileName); if
	 * (file.exists()) foundFiles.addElement(fileName); else
	 * missedFiles.addElement(fileName); } catch (IOException e) {
	 * missedFiles.add(fileName); } }
	 * 
	 * if ((op.equals(OBOEditAdapter.READ_ONTOLOGY) ||
	 * op.equals(DEDataAdapterI.IMPORT_TERMS)) && missedFiles.size() > 0) {
	 * String message = "Couldn't find the following paths:\n"; for(int i=0; i <
	 * missedFiles.size(); i++) { message += " "+((String)
	 * missedFiles.elementAt(i)) + "\n"; } JOptionPane.showMessageDialog(null,
	 * message); return; }
	 * 
	 * 
	 * if ((op.equals(OBOEditAdapter.WRITE_ONTOLOGY) ||
	 * op.equals(DEDataAdapterI.EXPORT_TERMS))) { if (containsURLs) {
	 * JOptionPane.showMessageDialog(null, "Can't write to a URL."); return; }
	 * 
	 * if (controller.warnBeforeDefinitionLoss() && (saveDefFileName == null ||
	 * saveDefFileName.length() == 0)) { Iterator it =
	 * controller.getHistory().getTerms().iterator(); boolean doWarning = false;
	 * while(it.hasNext()) { OBOClass term = (OBOClass) it.next(); if
	 * ((term.getDefinition() != null && term.getDefinition().length() > 0) ||
	 * (term.getComment() != null && term.getComment().length() > 0)) {
	 * doWarning = true; break; } } if (doWarning && !embed) { String message =
	 * "You have not specified a definitions \n"+ "file, but some terms in this
	 * session are defined. \n"+ "Those definitions will be lost if you save
	 * \n"+ "without specifying a definitions file.\n"+ "Do you want to continue
	 * this save and \n"+ "discard the definitions?"; int response =
	 * JOptionPane. showConfirmDialog(null, message, "Discard defintions?",
	 * JOptionPane.YES_NO_OPTION); if (response != JOptionPane.YES_OPTION)
	 * return; } }
	 * 
	 * if (foundFiles.size() > 0) { String message = "The following files
	 * already exist:\n"; for(int i=0; i < foundFiles.size(); i++) { message += "
	 * "+((String) foundFiles.elementAt(i)) + "\n"; } message += "Overwrite
	 * files?"; int response = JOptionPane. showConfirmDialog(null, message,
	 * "Overwrite files?", JOptionPane.YES_NO_OPTION); if (response !=
	 * JOptionPane.YES_OPTION) return; } } controllingObject.commit(); }
	 */

	@Override
	public void setFont(Font font) {
		super.setFont(font);
		if (browseDefButton != null) {
			filenameField.setFont(font);
			browseDefButton.setFont(font);
			definitionField.setFont(font);
			definitionLabel.setFont(font);
			hideDownstreamBox.setFont(font);
			allowCyclesBox.setFont(font);
			allowDanglingBox.setFont(font);
			reduceSizeBox.setFont(font);
			useLegacyTypesBox.setFont(font);
			translateTypesBox.setFont(font);
			commitButton.setFont(font);
			cancelButton.setFont(font);
		}
	}

	public void setSimpleUI(GraphicalUI simpleUI) {
		this.simpleUI = simpleUI;
	}

	public GraphicalUI getSimpleUI() {
		return simpleUI;
	}

	public GraphicalUI getAdvancedUI() {
		return null;
	}

	public void acceptComponentConfig(boolean storeonly)
			throws DataAdapterUIException {
		config.setComment(commentField.getText());
		config.setHideDownstream(hideDownstreamBox.isSelected());
		config.setAllowCycles(allowCyclesBox.isSelected());

		config.setAllowDangling(allowDanglingBox.isSelected());
		config.setReduceSize(reduceSizeBox.isSelected());
		config.setUseLegacyTypes(useLegacyTypesBox.isSelected());
		config.setTranslateTypes(translateTypesBox.isSelected());

		java.util.List saveRecords = new ArrayList();
		saveRecords.addAll(saveList.getData());

		config.setSaveRecords(saveRecords);

		java.util.List loadFiles = new ArrayList();
		Vector v = fileList.getData();
		Iterator it = v.iterator();
		while (it.hasNext()) {
			Object o = it.next();
			loadFiles.add(o.toString());
		}
		config.setReadPaths(loadFiles);

		if (op.equals(OBOAdapter.WRITE_ONTOLOGY)) {
			config.setSaveDefFilename(definitionField.getText());
		} else {
			if (definitionField.getText().length() > 0)
				config.setDefFilename(definitionField.getText());
			else
				config.setDefFilename(null);
		}

		if (!storeonly && op.equals(OBOAdapter.WRITE_ONTOLOGY)) {
			java.util.List overwriteFiles = new LinkedList();
			if (preferences.getWarnBeforeDefinitionLoss()) {
				if (config.getSaveDefFilename() == null
						|| config.getSaveDefFilename().trim().length() == 0) {
					int val = JOptionPane.showConfirmDialog(this,
							"No definition file was specified. "
									+ "Definitions will be\nlost if you "
									+ "save without a definitions file. "
									+ "Continue?", "Discard definitions?",
							JOptionPane.YES_NO_OPTION);
					if (val != JOptionPane.YES_OPTION)
						throw new DataAdapterUIException();
				}
			}
			it = saveRecords.iterator();
			while (it.hasNext()) {
				GOFlatFileAdapter.SaveRecord sr = (GOFlatFileAdapter.SaveRecord) it
						.next();
				if ((new File(sr.getFilename())).exists())
					overwriteFiles.add(sr.getFilename());
			}

			if (config.getSaveDefFilename() != null
					&& ((new File(config.getSaveDefFilename())).exists())) {
				overwriteFiles.add(config.getSaveDefFilename());
			}

			if (overwriteFiles.size() > 0) {
				StringBuffer out = new StringBuffer();
				out.append("The following files exist:\n");
				it = overwriteFiles.iterator();
				while (it.hasNext()) {
					String s = (String) it.next();
					out.append("   * " + s + "\n");
				}
				out.append("Overwrite these files?");
				int val = JOptionPane.showConfirmDialog(this, out.toString(),
						"Overwrite files?", JOptionPane.YES_NO_OPTION);
				if (val != JOptionPane.YES_OPTION)
					throw new DataAdapterUIException();
			}
			config.setBasicSave(false);
		}
	}
	/*
	 * protected void loadProfile(FileProfile fileProfile) { if
	 * (op.equals(OBOEditAdapter.WRITE_ONTOLOGY) || op.equals(DEDataAdapterI.EXPORT_TERMS) ||
	 * op.equals(DEDataAdapterI.IMPORT_TERMS)) { if
	 * (fileProfile.getFiles().size() > 0) {
	 * filenameField.setText(fileProfile.getFiles().get(0). toString()); } }
	 * else { Vector v = new Vector(); for(int i=0; i <
	 * fileProfile.getFiles().size(); i++) { v.add(new
	 * EditableString(fileProfile.getFiles(). get(i).toString())); }
	 * fileList.setData(v); v = new Vector();
	 * v.addAll(fileProfile.getSaveFiles()); saveList.setData(v); // } if
	 * (op.equals(OBOEditAdapter.WRITE_ONTOLOGY) || op.equals(DEDataAdapterI.EXPORT_TERMS)) {
	 * if (fileProfile.getSaveDefFile() != null)
	 * definitionField.setText(fileProfile.getSaveDefFile()); else
	 * definitionField.setText(fileProfile.getDefFile()); } else {
	 * definitionField.setText(fileProfile.getDefFile()); } }
	 */

	public void setAdvancedUI(GraphicalUI ui) {
	}
}
