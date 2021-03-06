package org.oboedit.gui.components;

import org.bbop.expression.ExpressionException;
import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.GUIManager;
import org.bbop.swing.*;
import org.bbop.util.*;
import org.bounce.text.xml.*;
import org.obo.datamodel.*;
import org.oboedit.gui.*;
import org.oboedit.gui.event.ReconfigEvent;
import org.oboedit.gui.widget.DbxrefListEditor;
import org.oboedit.util.GUIUtil;
import org.xml.sax.SAXParseException;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.text.Element;
import javax.swing.text.PlainDocument;
import java.util.*;
import java.util.List;
import java.net.*;
import java.io.*;
import java.awt.*;
import java.awt.event.*;

public class ConfigurationManager extends AbstractGUIComponent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 683746976443349712L;

	ListEditor iconList;

	JTextField selectionBatchField;

	JTextField browserCommandField;

	JCheckBox useDefaultBrowserBox;

	JTextField userField;

	JTextField fullnameField;

	JTextField emailField;

	JCheckBox allowCyclesBox;

	JCheckBox warnBeforeDeleteBox;

	JCheckBox warnBeforeDefinitionLossBox;

	JCheckBox advancedRootDetectionBox;

	JCheckBox autoCommitCheckBox;

	JCheckBox warnBeforeDiscardingEditsBox;

	JButton commitButton;
	
	JTabbedPane mainPanel;

	JTextField memoryField;

	JComboBox fontNameList;

	JComboBox fontSizeList;

	JComboBox fontTypeList;

	JTextArea fontPreviewArea;

	JCheckBox showUndefinedTermsBox;

	JCheckBox caseSensitiveSortBox;

	JCheckBox showToolTipsBox;

	JCheckBox confirmOnExitBox;

	JCheckBox autosaveEnabledCheckBox;

	JTextField autosavePathField;

	JTextField autosaveExpirationField;

	JTextField autosaveWaitField;

	DbxrefListEditor dbxrefEditor;

	DbxrefListEditor defDbxrefListEditor;

	ListEditor defDbxrefList;

	JCheckBox allowExtendedCheckbox;

	JTextArea defTextArea = new JTextArea();

	JCheckBox personalDefCheckbox = new JCheckBox("Use personal definition");

	/*
	 * protected GrayUndefinedRenderer grayUndefinedRenderer = new
	 * GrayUndefinedRenderer();
	 */
	private Vector icons;

	/*
	 * private class TimeUnitSelector extends JPanel { protected JLabel label;
	 * protected JTextField textField; protected JComboBox unitSelector;
	 * protected String [] units = {"minutes", "hours", "days"}; protected
	 * String oldUnits = "minutes";
	 * 
	 * public TimeUnitSelector(String labelStr, String fieldVal) { label = new
	 * JLabel(labelStr); textField = new JTextField(fieldVal); unitSelector =
	 * new JComboBox(units); setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
	 * add(Box.createHorizontalStrut(20)); add(label);
	 * add(Box.createHorizontalStrut(20)); add(textField);
	 * add(Box.createHorizontalStrut(5)); add(unitSelector);
	 * 
	 * label.setFont(Preferences.getPreferences().getFont());
	 * textField.setFont(Preferences.getPreferences().getFont());
	 * unitSelector.setFont(Preferences.getPreferences().getFont());
	 * 
	 * unitSelector.addActionListener(new ActionListener() { public void
	 * actionPerformed(ActionEvent e) { unitChange(); oldUnits = (String)
	 * unitSelector.getSelectedItem(); } }); }
	 * 
	 * protected void unitChange() { } }
	 */

	private class IconWrapper {
		private String type;

		private String url;

		public IconWrapper(String type, String url) {
			this.type = type;
			this.url = url;
		}

		public String getType() {
			return type;
		}

		public String getURL() {
			return url;
		}

		public void setType(String type) {
			this.type = type;
		}

		public void setURL(String url) {
			this.url = url;
		}

		@Override
		public String toString() {
			return type;
		}
	}

	private class IconEditor extends JPanel implements GenericEditorComponent {

		/**
		 * 
		 */
		private static final long serialVersionUID = 4386550408050571020L;

		JLabel typeLabel = new JLabel("Relationship type");

		JLabel urlLabel = new JLabel("Icon URL");

		JTextField typeField = new JTextField(10);

		JTextField urlField = new JTextField(50);

		JLabel previewLabel = new JLabel();

		JLabel previewTextLabel = new JLabel("Icon preview");

		JButton browseButton = new JButton("Browse files");

		JButton libraryButton = new JButton("Browse built-in icons");

		protected ListEditor editor;

		public void setMasterComponent(Component c) {
			if (c instanceof ListEditor)
				editor = (ListEditor) c;
		}

		public IconEditor() {
			setMinimumSize(new Dimension(0, 0));
			setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

			urlField.setMaximumSize(new Dimension(Integer.MAX_VALUE, urlField
					.getPreferredSize().height));
			typeField.setMaximumSize(new Dimension(Integer.MAX_VALUE, typeField
					.getPreferredSize().height));

			FocusListener listener = new FocusListener() {
				public void focusLost(FocusEvent e) {
					update();
					iconList.commit();
				}

				public void focusGained(FocusEvent e) {
				}
			};

			urlField.addFocusListener(listener);
			typeField.addFocusListener(listener);
			libraryButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					showIconLibrary();
				}
			});
			browseButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					showFileChooser();
				}
			});

			Box typeBox = new Box(BoxLayout.X_AXIS);
			typeBox.add(typeLabel);
			typeBox.add(Box.createHorizontalStrut(10));
			typeBox.add(Box.createHorizontalGlue());
			typeBox.add(typeField);
			typeBox.setMaximumSize(new Dimension(Integer.MAX_VALUE, typeBox
					.getPreferredSize().height));

			Box urlLabelBox = new Box(BoxLayout.X_AXIS);
			urlLabelBox.add(urlLabel);
			urlLabelBox.add(Box.createHorizontalGlue());

			Box buttonBox = new Box(BoxLayout.X_AXIS);
			buttonBox.add(browseButton);
			buttonBox.add(libraryButton);
			buttonBox.add(Box.createHorizontalGlue());

			Box previewBox = new Box(BoxLayout.X_AXIS);
			previewBox.add(previewTextLabel);
			previewBox.add(Box.createHorizontalGlue());
			previewBox.add(previewLabel);

			add(typeBox);
			add(Box.createVerticalStrut(10));
			add(urlLabelBox);
			add(urlField);
			add(Box.createVerticalStrut(5));
			add(buttonBox);
			add(Box.createVerticalStrut(10));
			add(previewBox);
			add(Box.createVerticalGlue());
		}

		/*
		 * public Insets getInsets() { return new Insets(5, 5, 5, 5); }
		 * 
		 * public Insets getInsets(Insets in) { return getInsets(); }
		 */

		private void update() {
			try {
				URL url = new URL(urlField.getText());
				previewLabel.setIcon(new ImageIcon(url));
				validate();
			} catch (Exception e) {
			}
		}

		public void load(Object o) {
			IconWrapper iw = (IconWrapper) o;
			urlField.setText(iw.getURL());
			typeField.setText(iw.getType());
			urlField.setCaretPosition(0);
			typeField.setCaretPosition(0);
			update();
		}

		public void store(Object o) {
			IconWrapper iw = (IconWrapper) o;
			iw.setURL(urlField.getText());
			iw.setType(typeField.getText());
		}

		public Object createNewValue() {
			return new IconWrapper("UNKNOWN", "<new icon path>");
		}

		private void showIconLibrary() {
			JPanel panel = new JPanel();
			panel.setBackground(Color.white);
			List<URL> icons = Preferences.getIconLibrary();
			final JDialog dialog = new JDialog();
			dialog.setTitle("Click an icon to select it");
			dialog.setModal(true);

			for (final URL url : icons) {
				ActionListener listener = new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						urlField.setText(url.toString());
						update();
						dialog.dispose();
					}
				};
				JButton button = new JButton(new ImageIcon(url));
				button.setOpaque(false);
				button.addActionListener(listener);
				panel.add(button);
			}
			dialog.setContentPane(panel);
			dialog.pack();
			dialog.setVisible(true);
		}

		private void showFileChooser() {
			JFileChooser chooser = new JFileChooser(System
					.getProperty("user.dir"));
			int returnVal = chooser.showOpenDialog(GUIManager.getManager()
					.getFrame());
			if (returnVal == JFileChooser.APPROVE_OPTION) {
				File path = chooser.getSelectedFile();
				try {
					urlField.setText(path.toURL().toString());
					update();
				} catch (MalformedURLException e) {
				}
			}
		}
	}

	protected void selectListItem(JComboBox list, Object item) {
		list.removeItem(item);
		list.insertItemAt(item, 0);
		list.setSelectedIndex(0);
	}

	protected Font getChosenFont() {
		return GUIUtil.decodeFont((String) fontNameList.getSelectedItem(),
				(String) fontSizeList.getSelectedItem(), (String) fontTypeList
						.getSelectedItem());
	}

	public void buildFontPreview() {
		fontPreviewArea.setFont(getChosenFont());
		revalidate();
		repaint();
	}

	private class FontListener implements ActionListener, ItemListener {
		public void actionPerformed(ActionEvent e) {
			buildFontPreview();
		}

		public void itemStateChanged(ItemEvent e) {
			buildFontPreview();
		}
	}

	protected void formatField(JLabel label, JTextField field) {
		field.setMaximumSize(new Dimension(Integer.MAX_VALUE, (int) field
				.getPreferredSize().getHeight()));
		label.setMaximumSize(label.getPreferredSize());
	}

	protected void updatePersonalDefFields(boolean enabled) {
		defTextArea.setEnabled(enabled);
		defDbxrefList.setEnabled(enabled);
	}

	@Override
	public void init() {
		removeAll();
		mainPanel = new JTabbedPane();

		// setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		setLayout(new BorderLayout());

		JLabel noIconLabel = new JLabel("Click an icon definition to edit");

		icons = getIcons();

		commitButton = new JButton("Save Configuration");
		commitButton.setAlignmentX((float) .5);
		commitButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				save();
			}
		});

		iconList = new ListEditor(new IconEditor(), noIconLabel, icons, true,
				true, true, true, false);
		iconList.setBorder(new TitledBorder("Relationship Type Icons"));

		iconList.setMinimumSize(new Dimension(0, 0));

		Box allowBox = new Box(BoxLayout.X_AXIS);
		Box warnDeleteBox = new Box(BoxLayout.X_AXIS);
		Box warnDefinitionBox = new Box(BoxLayout.X_AXIS);
		Box advancedRootBox = new Box(BoxLayout.X_AXIS);
		Box autoCommitPanel = new Box(BoxLayout.X_AXIS);

		JLabel userLabel = new JLabel("User Name", JLabel.TRAILING);
		JLabel fullnameLabel = new JLabel("Full Name", JLabel.TRAILING);
		JLabel emailLabel = new JLabel("Email Address", JLabel.TRAILING);

		JLabel startLabel = new JLabel("Start of id range", JLabel.TRAILING);
		JLabel endLabel = new JLabel("End of id range", JLabel.TRAILING);
		JLabel prefixLabel = new JLabel("Default id prefix", JLabel.TRAILING);
		JLabel idLengthLabel = new JLabel("Default id length", JLabel.TRAILING);
		JLabel idAdapterLabel = new JLabel("Default id adapter name",
				JLabel.TRAILING);
		JLabel selectionBatchLabel = new JLabel("Selection batch size",
				JLabel.TRAILING);
		JTextArea noDbxLabel = new JTextArea("Select a dbxref from the list "
				+ "to edit it, or press add to " + "create a new dbxref");
		noDbxLabel.setMinimumSize(new Dimension(0, 0));
		noDbxLabel.setOpaque(false);
		noDbxLabel.setLineWrap(true);
		noDbxLabel.setWrapStyleWord(true);
		noDbxLabel.setEditable(false);
		noDbxLabel.setEnabled(false);

		final JLabel browserLabel = new JLabel("Browser command",
				JLabel.TRAILING);

		userField = new JTextField(20);
		fullnameField = new JTextField(20);
		emailField = new JTextField(20);

		selectionBatchField = new JTextField(3);

		browserCommandField = new JTextField(20);

		dbxrefEditor = new DbxrefListEditor(Dbxref.UNKNOWN);
		defDbxrefListEditor = new DbxrefListEditor(Dbxref.DEFINITION);
		defDbxrefList = new ListEditor(defDbxrefListEditor, noDbxLabel,
				new Vector(0), true, true, true, true, true);
		allowExtendedCheckbox = new JCheckBox("Allow extended characters");

		formatField(userLabel, userField);
		formatField(fullnameLabel, fullnameField);
		formatField(emailLabel, emailField);

		formatField(selectionBatchLabel, selectionBatchField);
		formatField(browserLabel, browserCommandField);

		useDefaultBrowserBox = new JCheckBox("Use default browser");
		useDefaultBrowserBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				browserCommandField.setEnabled(!useDefaultBrowserBox
						.isSelected());
				browserLabel.setEnabled(!useDefaultBrowserBox.isSelected());
			}
		});

		allowCyclesBox = new JCheckBox("Allow new cycles to be created");
		warnBeforeDeleteBox = new JCheckBox("Warn before final delete");
		warnBeforeDefinitionLossBox = new JCheckBox("Warn before discarding "
				+ "definitions on save");
		autoCommitCheckBox = new JCheckBox("Auto commit text edits");
		autoCommitCheckBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				warnBeforeDiscardingEditsBox.setEnabled(!autoCommitCheckBox
						.isSelected());
			}
		});
		warnBeforeDiscardingEditsBox = new JCheckBox(
				"Warn before discarding text edits.");
		advancedRootDetectionBox = new JCheckBox("Use advanced root "
				+ "detection");
		showUndefinedTermsBox = new JCheckBox("Gray out undefined terms");
		caseSensitiveSortBox = new JCheckBox("Case sensitive term sorting");
		showToolTipsBox = new JCheckBox("Show terms ids as tool tips in "
				+ "term panels");
		confirmOnExitBox = new JCheckBox("Confirm on exit");

		autosaveEnabledCheckBox = new JCheckBox("Enable Autosave");
		autosavePathField = new JTextField();
		autosaveExpirationField = new JTextField();
		autosaveWaitField = new JTextField();

		autosaveEnabledCheckBox.setOpaque(false);
		useDefaultBrowserBox.setOpaque(false);
		allowCyclesBox.setOpaque(false);
		warnBeforeDeleteBox.setOpaque(false);
		warnBeforeDefinitionLossBox.setOpaque(false);
		autoCommitCheckBox.setOpaque(false);
		warnBeforeDiscardingEditsBox.setOpaque(false);
		advancedRootDetectionBox.setOpaque(false);
		showUndefinedTermsBox.setOpaque(false);
		caseSensitiveSortBox.setOpaque(false);
		showToolTipsBox.setOpaque(false);
		confirmOnExitBox.setOpaque(false);

		useDefaultBrowserBox.setSelected(Preferences.getPreferences()
				.getBrowserCommand().length() == 0);
		browserCommandField.setEnabled(!useDefaultBrowserBox.isSelected());
		browserLabel.setEnabled(!useDefaultBrowserBox.isSelected());
		autosaveEnabledCheckBox.setSelected(Preferences.getPreferences()
				.getAutosaveEnabled());
		dbxrefEditor.load(Preferences.getPreferences().getPersonalDbxref());
		autosavePathField.setText(Preferences.getPreferences()
				.getAutosavePath().toString());
		autosaveExpirationField.setText(Preferences.getPreferences()
				.getAutosaveExpirationDays()
				+ "");
		autosaveWaitField.setText(Preferences.getPreferences()
				.getAutosaveWaitTime()
				+ "");

		userField.setText(Preferences.getPreferences().getUserName());
		fullnameField.setText(Preferences.getPreferences().getFullName());
		emailField.setText(Preferences.getPreferences().getEmail());
		selectionBatchField.setText(Preferences.getPreferences()
				.getSelectionBatchSize()
				+ "");
		browserCommandField.setText(Preferences.getPreferences()
				.getBrowserCommand()
				+ "");
		allowCyclesBox.setSelected(Preferences.getPreferences()
				.getAllowCycles());
		allowExtendedCheckbox.setSelected(Preferences.getPreferences()
				.getAllowExtendedCharacters());
		personalDefCheckbox.setSelected(Preferences.getPreferences()
				.getUsePersonalDefinition());

		if (Preferences.getPreferences().getUsePersonalDefinition()) {
			defTextArea.setText(Preferences.getPreferences()
					.getPersonalDefinition());
			Vector<Dbxref> v = new Vector<Dbxref>();
			v.addAll(Preferences.getPreferences().getPersonalDbxrefs());
			defDbxrefList.setData(v);
		} else {
			defTextArea.setText("");
			defDbxrefList.setData(new Vector());
		}

		updatePersonalDefFields(Preferences.getPreferences()
				.getUsePersonalDefinition());

		autoCommitCheckBox.setSelected(Preferences.getPreferences()
				.getAutoCommitTextEdits());
		warnBeforeDiscardingEditsBox.setSelected(Preferences.getPreferences()
				.getWarnBeforeDiscardingEdits());
		warnBeforeDiscardingEditsBox.setEnabled(!autoCommitCheckBox
				.isSelected());
		warnBeforeDeleteBox.setSelected(Preferences.getPreferences()
				.getWarnBeforeDelete());
		warnBeforeDefinitionLossBox.setSelected(Preferences.getPreferences()
				.getWarnBeforeDefinitionLoss());
		advancedRootDetectionBox.setSelected(!Preferences.getPreferences()
				.getUseBasicRootDetection());
		// showUndefinedTermsBox.setSelected(controller.getGlobalFilteredRenderers().contains(grayUndefinedRenderer));
		caseSensitiveSortBox.setSelected(Preferences.getPreferences()
				.getCaseSensitiveSort());
		showToolTipsBox.setSelected(Preferences.getPreferences()
				.getShowToolTips());
		confirmOnExitBox.setSelected(Preferences.getPreferences()
				.getConfirmOnExit());

		allowBox.add(allowCyclesBox);
		allowBox.add(Box.createHorizontalGlue());

		warnDeleteBox.add(warnBeforeDeleteBox);
		warnDeleteBox.add(Box.createHorizontalGlue());
		warnDefinitionBox.add(warnBeforeDefinitionLossBox);
		warnDefinitionBox.add(Box.createHorizontalGlue());
		advancedRootBox.add(advancedRootDetectionBox);
		advancedRootBox.add(Box.createHorizontalGlue());
		autoCommitPanel.add(autoCommitCheckBox);
		autoCommitPanel.add(Box.createHorizontalStrut(10));
		autoCommitPanel.add(warnBeforeDiscardingEditsBox);
		autoCommitPanel.add(Box.createHorizontalStrut(10));
		autoCommitPanel.add(allowExtendedCheckbox);
		autoCommitPanel.add(Box.createHorizontalGlue());

		Box configFileLabelBox = new Box(BoxLayout.X_AXIS);
		JLabel configFileLabel = new JLabel("Config file");
		JLabel configFilePath = new JLabel(System.getProperty("user.home")
				+ "/.oboedit");

		configFileLabelBox.add(configFileLabel);
		configFileLabelBox.add(Box.createHorizontalStrut(20));
		configFileLabelBox.add(Box.createHorizontalGlue());
		configFileLabelBox.add(configFilePath);

		Box autosaveEnabledBox = new Box(BoxLayout.X_AXIS);
		autosaveEnabledBox.add(autosaveEnabledCheckBox);
		autosaveEnabledBox.add(Box.createHorizontalGlue());

		JLabel autosavePathLabel = new JLabel("Autosave file path");
		Box autosavePathBox = new Box(BoxLayout.X_AXIS);
		autosavePathBox.add(Box.createHorizontalStrut(20));
		autosavePathBox.add(autosavePathLabel);
		autosavePathBox.add(Box.createHorizontalStrut(20));
		autosavePathBox.add(Box.createHorizontalGlue());
		autosavePathBox.add(autosavePathField);

		JLabel autosaveWaitLabel = new JLabel("Do autosave every");
		JLabel minutesLabel = new JLabel("minutes");
		Box autosaveWaitBox = new Box(BoxLayout.X_AXIS);
		autosaveWaitBox.add(Box.createHorizontalStrut(20));
		autosaveWaitBox.add(autosaveWaitLabel);
		autosaveWaitBox.add(Box.createHorizontalStrut(20));
		autosaveWaitBox.add(autosaveWaitField);
		autosaveWaitBox.add(Box.createHorizontalStrut(5));
		autosaveWaitBox.add(minutesLabel);

		JLabel autosaveExpirationLabel = new JLabel("Autosave files expire in");
		Box autosaveExpirationBox = new Box(BoxLayout.X_AXIS);
		JLabel daysLabel = new JLabel("days");
		autosaveExpirationBox.add(Box.createHorizontalStrut(20));
		autosaveExpirationBox.add(autosaveExpirationLabel);
		autosaveExpirationBox.add(Box.createHorizontalStrut(20));
		autosaveExpirationBox.add(autosaveExpirationField);
		autosaveExpirationBox.add(Box.createHorizontalStrut(5));
		autosaveExpirationBox.add(daysLabel);

		JPanel userPanel = new JPanel();
		userPanel.setLayout(new BoxLayout(userPanel, BoxLayout.Y_AXIS));
		dbxrefEditor.setBorder(new TitledBorder("Personal Dbxref"));
		JPanel fieldsPanel = new JPanel(new SpringLayout());
		fieldsPanel.setOpaque(false);
		fieldsPanel.add(userLabel);
		fieldsPanel.add(userField);
		fieldsPanel.add(fullnameLabel);
		fieldsPanel.add(fullnameField);
		fieldsPanel.add(emailLabel);
		fieldsPanel.add(emailField);

		userPanel.add(fieldsPanel);
		// userPanel.add(dbxrefEditor);
		userPanel.add(Box.createVerticalGlue());

		SwingUtil.makeCompactGrid(fieldsPanel, 3, 2, 5, 5, 5, 5);

		JPanel autosavePanel = new JPanel();
		autosavePanel.setOpaque(false);
		autosavePanel.setLayout(new BoxLayout(autosavePanel, BoxLayout.Y_AXIS));
		autosavePanel.setBorder(new TitledBorder("Autosave Options"));
		autosavePanel.add(autosaveEnabledBox);
		autosavePanel.add(autosavePathBox);
		autosavePanel.add(autosaveWaitBox);
		autosavePanel.add(autosaveExpirationBox);

		JPanel pathPanel = new JPanel();
		pathPanel.setLayout(new BorderLayout());

		Box pathBox = new Box(BoxLayout.Y_AXIS);

		pathBox.add(configFileLabelBox);
		pathBox.add(Box.createVerticalStrut(5));
		pathBox.add(autosavePanel);

		pathPanel.add(pathBox, "North");
		pathPanel.add(Box.createVerticalGlue());
		pathPanel.setBorder(new TitledBorder("Config file paths"));

		memoryField = new JTextField();
		memoryField.setText(Preferences.getPreferences().getMemString());

		String[] sizes = { "6", "8", "10", "12", "14", "16", "18", "20" };
		String[] fonts = GraphicsEnvironment.getLocalGraphicsEnvironment()
				.getAvailableFontFamilyNames();
		String[] fontStyles = { "Normal", "Italic", "Bold", "Bold-Italic" };
		fontNameList = new JComboBox(fonts);
		fontSizeList = new JComboBox(sizes);
		fontTypeList = new JComboBox(fontStyles);

		selectListItem(fontNameList, getDefaultFontName());

		fontNameList.setSelectedItem(getDefaultFontName());
		fontSizeList.setSelectedItem(getDefaultFontSize());
		fontTypeList.setSelectedItem(getDefaultFontStyle());

		fontPreviewArea = new JTextArea();
		fontPreviewArea.setText("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
				+ "abcdefghijklmnopqrstuvwxyz");
		fontPreviewArea.setWrapStyleWord(true);
		fontPreviewArea.setLineWrap(true);
		fontPreviewArea.setEditable(false);
		fontPreviewArea.setOpaque(false);


		JPanel guiPanel = new JPanel();
		// guiPanel.setLayout(new BoxLayout(guiPanel, BoxLayout.Y_AXIS));
		guiPanel.setLayout(new SpringLayout());
		guiPanel.setBorder(new TitledBorder("GUI Options"));

		JLabel memoryLabel = new JLabel("OBO-Edit memory allocation");

		Box memoryBox = new Box(BoxLayout.X_AXIS);
		memoryBox.add(memoryLabel);
		memoryBox.add(Box.createHorizontalStrut(15));
		memoryBox.add(memoryField);
		memoryBox.add(fontPreviewArea);

		Box fontLine = new Box(BoxLayout.X_AXIS);
		fontLine.add(fontNameList);
		fontLine.add(fontSizeList);
		fontLine.add(fontTypeList);

		JLabel fontPreviewLabel = new JLabel("Font preview:");

		Box fontPreviewLabelLine = new Box(BoxLayout.X_AXIS);
		fontPreviewLabelLine.add(fontPreviewLabel);
		fontPreviewLabelLine.add(Box.createHorizontalGlue());

		JLabel fontLabel = new JLabel("Default font");

		Box fontAndPreviewBox = new Box(BoxLayout.Y_AXIS);
		fontAndPreviewBox.add(fontLine);
		fontAndPreviewBox.add(Box.createVerticalStrut(5));
		fontAndPreviewBox.add(fontPreviewLabelLine);
		fontAndPreviewBox.add(fontPreviewArea);

		Box fontLabelBox = new Box(BoxLayout.Y_AXIS);
		fontLabelBox.add(fontLabel);
		fontLabelBox.add(Box.createVerticalGlue());

		Box fontBox = new Box(BoxLayout.X_AXIS);
		fontBox.add(fontLabelBox);
		fontBox.add(Box.createHorizontalStrut(20));
		fontBox.add(fontAndPreviewBox);

		FontListener fontListener = new FontListener();
		fontNameList.addActionListener(fontListener);
		fontNameList.addItemListener(fontListener);
		fontSizeList.addActionListener(fontListener);
		fontSizeList.addItemListener(fontListener);
		fontTypeList.addActionListener(fontListener);
		fontTypeList.addItemListener(fontListener);

		JPanel moreOptionsPanel = new JPanel();
		moreOptionsPanel.setOpaque(false);
		moreOptionsPanel.setLayout(new BoxLayout(moreOptionsPanel,
				BoxLayout.Y_AXIS));
		moreOptionsPanel.setBorder(new TitledBorder(
				"Startup Options (changes do not take effect until restart)"));
		moreOptionsPanel.add(Box.createVerticalStrut(10));
		moreOptionsPanel.add(memoryBox);
		moreOptionsPanel.add(Box.createVerticalStrut(10));
		moreOptionsPanel.add(fontBox);
		moreOptionsPanel.setMaximumSize(moreOptionsPanel.getPreferredSize());

		JPanel runtimeDisplayPanel = new JPanel();
		runtimeDisplayPanel.setBorder(new TitledBorder(
				"Runtime display options"));
		runtimeDisplayPanel.setOpaque(false);
		runtimeDisplayPanel.setLayout(new BoxLayout(runtimeDisplayPanel,
				BoxLayout.Y_AXIS));

		JPanel caseSensitiveSortPanel = new JPanel();
		caseSensitiveSortPanel.setOpaque(false);
		caseSensitiveSortPanel.setLayout(new BoxLayout(caseSensitiveSortPanel,
				BoxLayout.X_AXIS));
		caseSensitiveSortPanel.add(caseSensitiveSortBox);
		caseSensitiveSortPanel.add(Box.createHorizontalGlue());

		JPanel showToolTipsPanel = new JPanel();
		showToolTipsPanel.setOpaque(false);
		showToolTipsPanel.setLayout(new BoxLayout(showToolTipsPanel,
				BoxLayout.X_AXIS));
		showToolTipsPanel.add(showToolTipsBox);
		showToolTipsPanel.add(Box.createHorizontalGlue());

		JPanel showConfirmOnExitPanel = new JPanel();
		showConfirmOnExitPanel.setOpaque(false);
		showConfirmOnExitPanel.setLayout(new BoxLayout(showConfirmOnExitPanel,
				BoxLayout.X_AXIS));
		showConfirmOnExitPanel.add(confirmOnExitBox);
		showConfirmOnExitPanel.add(Box.createHorizontalGlue());

		runtimeDisplayPanel.add(caseSensitiveSortPanel);
		runtimeDisplayPanel.add(showToolTipsPanel);
		runtimeDisplayPanel.add(showConfirmOnExitPanel);
		runtimeDisplayPanel.setMaximumSize(runtimeDisplayPanel
				.getPreferredSize());

		JPanel iconPanel = new JPanel();
		iconPanel.setOpaque(false);
		iconPanel.setLayout(new GridLayout(1, 1));
		iconPanel.add(iconList);
		iconPanel.setMaximumSize(new Dimension(Integer.MAX_VALUE, 300));
		guiPanel.add(iconPanel);
		guiPanel.add(runtimeDisplayPanel);
		guiPanel.add(moreOptionsPanel);
		guiPanel.add(Box.createVerticalGlue());

		SwingUtil.makeCompactGrid(guiPanel, 4, 1, 0, 0, 0, 5);

		JPanel behaviorPanel = new JPanel();

		behaviorPanel.setLayout(new BoxLayout(behaviorPanel, BoxLayout.Y_AXIS));

		behaviorPanel.add(allowBox);
		behaviorPanel.add(warnDeleteBox);
		behaviorPanel.add(warnDefinitionBox);
		behaviorPanel.add(advancedRootBox);
		behaviorPanel.add(Box.createVerticalGlue());

		/*
		 * JPanel valuesPanel = new JPanel(new SpringLayout());
		 * valuesPanel.setOpaque(false);
		 *  // 6 valuesPanel.add(Box.createRigidArea(new Dimension(1, 10)));
		 * valuesPanel.add(Box.createRigidArea(new Dimension(1, 10))); // 7
		 * valuesPanel.add(selectionBatchLabel);
		 * valuesPanel.add(selectionBatchField); // 8
		 * valuesPanel.add(Box.createRigidArea(new Dimension(1, 10)));
		 * valuesPanel.add(Box.createRigidArea(new Dimension(1, 10)));
		 *  // 9 valuesPanel.add(useDefaultBrowserBox);
		 * valuesPanel.add(Box.createRigidArea(new Dimension(1, 10))); // 10
		 * valuesPanel.add(browserLabel); valuesPanel.add(browserCommandField); //
		 * 11 valuesPanel.add(Box.createVerticalGlue());
		 * valuesPanel.add(Box.createVerticalGlue());
		 * 
		 * SwingUtil.makeCompactGrid(valuesPanel, 6, 2, 5, 5, 5, 5);
		 */

		JScrollPane scroller = new JScrollPane(defTextArea,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		defTextArea.setLineWrap(true);
		defTextArea.setWrapStyleWord(true);

		personalDefCheckbox.setOpaque(false);

		JPanel textEditTopPanel = new JPanel();
		textEditTopPanel.setLayout(new BoxLayout(textEditTopPanel,
				BoxLayout.Y_AXIS));
		textEditTopPanel.setOpaque(false);

		// textEditTopPanel.add(allowExtendedCheckbox);
		textEditTopPanel.add(autoCommitPanel);

		JPanel textEditPanel = new JPanel();
		textEditPanel.setLayout(new BorderLayout());
		textEditPanel.setOpaque(true);

		JPanel personalDefinitionSubPanel = new JPanel();
		personalDefinitionSubPanel.setLayout(new GridLayout(1, 2));
		personalDefinitionSubPanel.add(scroller);
		personalDefinitionSubPanel.add(defDbxrefList);

		JPanel personalDefinitionPanel = new JPanel();
		personalDefinitionPanel.setLayout(new BorderLayout());
		personalDefinitionPanel.add(personalDefinitionSubPanel, "Center");
		personalDefinitionPanel.add(personalDefCheckbox, "North");

		personalDefCheckbox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				updatePersonalDefFields(personalDefCheckbox.isSelected());
			}
		});

		personalDefinitionPanel.setOpaque(true);
		personalDefinitionSubPanel.setOpaque(true);
		textEditPanel.add(personalDefinitionPanel, "Center");
		textEditPanel.add(textEditTopPanel, "North");

		mainPanel.addTab("General GUI", null, guiPanel, "General gui options");
		mainPanel.addTab("Paths & Autosave", null, pathPanel, "Set paths");
		mainPanel.addTab("Behavior", null, behaviorPanel, "Set ID options");
		mainPanel.addTab("User Settings", null, userPanel, "Set user options");
		mainPanel.addTab("Text Editing", null, textEditPanel,
				"Set text editing options");

		add(mainPanel, "Center");
		Box buttonBox = Box.createVerticalBox();
		buttonBox.add(Box.createVerticalStrut(15));
		buttonBox.add(commitButton);
		buttonBox.add(Box.createVerticalStrut(15));
		add(buttonBox, "South");
		buildFontPreview();
	}

	private String getDefaultFontName() {
		return getFont().getFamily();
	}

	private String getDefaultFontSize() {
		return getFont().getSize() + "";
	}

	private String getDefaultFontStyle() {
		Font font = getFont();
		String style;
		if (font.isItalic() && font.isBold())
			style = "Bold-Italic";
		else if (font.isItalic())
			style = "Italic";
		else if (font.isBold())
			style = "Bold";
		else
			style = "Normal";
		return style;
	}

	private Vector<IconWrapper> getIcons() {
		Vector<IconWrapper> out = new Vector<IconWrapper>();

		for (String id : Preferences.getPreferences().getIconURLIndex()
				.keySet()) {
			IconWrapper iw = new IconWrapper(id, Preferences.getPreferences()
					.getIconURLIndex().get(id));
			out.addElement(iw);
		}

		return out;
	}

	protected boolean validateConfiguration() {
		return true;
	}

	public void save() {
		/*
		 * if (!validateConfiguration()) return;
		 */
		iconList.commit();
		defDbxrefList.commit();

		icons = iconList.getData();

		Preferences preferences = Preferences.getPreferences();
		try {
			int batchSize = Integer.parseInt(selectionBatchField.getText());
			preferences.setSelectionBatchSize(batchSize);
		} catch (NumberFormatException ex) {
		}

		if (useDefaultBrowserBox.isSelected())
			preferences.setBrowserCommand("");
		else
			preferences.setBrowserCommand(browserCommandField.getText());

		preferences.setUserName(userField.getText());
		preferences.setFullName(fullnameField.getText());
		preferences.setEmail(emailField.getText());

		preferences.setAllowCycles(allowCyclesBox.isSelected());
		preferences.setAutoCommitTextEdits(autoCommitCheckBox.isSelected());
		preferences.setWarnBeforeDiscardingEdits(warnBeforeDiscardingEditsBox
				.isSelected());
		preferences.setWarnBeforeDelete(warnBeforeDeleteBox.isSelected());
		preferences.setWarnBeforeDefinitionLoss(warnBeforeDefinitionLossBox
				.isSelected());
		preferences.setUseBasicRootDetection(!advancedRootDetectionBox
				.isSelected());
		preferences.setAllowExtendedCharacters(allowExtendedCheckbox
				.isSelected());

		Dbxref ref = (Dbxref) dbxrefEditor.createNewValue();
		dbxrefEditor.store(ref);

		preferences.setPersonalDbxref(ref);
		preferences.setCaseSensitiveSort(caseSensitiveSortBox.isSelected());
		preferences.setShowToolTips(showToolTipsBox.isSelected());
		preferences.setConfirmOnExit(confirmOnExitBox.isSelected());
		preferences.setAutosaveEnabled(autosaveEnabledCheckBox.isSelected());

		try {
			int waitTime = Integer.parseInt(autosaveWaitField.getText());
			preferences.setAutosaveWaitTime(waitTime);
		} catch (NumberFormatException ex) {
		}

		try {
			int days = Integer.parseInt(autosaveExpirationField.getText());
			preferences.setAutosaveExpirationDays(days);
		} catch (NumberFormatException ex) {
		}

		preferences.setAutosavePath(new File(autosavePathField.getText()));
		preferences.setFont(GUIUtil.decodeFont((String) fontNameList
				.getSelectedItem(), (String) fontSizeList.getSelectedItem(),
				(String) fontTypeList.getSelectedItem()));
		preferences.setMemoryValue(memoryField.getText());

		Map<String, String> iconURLIndex = new HashMap<String, String>();
		for (int i = 0; i < icons.size(); i++) {
			IconWrapper iw = (IconWrapper) icons.elementAt(i);
			iconURLIndex.put(iw.getType(), iw.getURL());
		}
		preferences.setIconURLIndex(iconURLIndex);

		preferences.setUsePersonalDefinition(personalDefCheckbox.isSelected());
		if (personalDefCheckbox.isSelected()) {
			preferences.setPersonalDefinition(defTextArea.getText());
			if (preferences.getPersonalDbxrefs() == null)
				preferences.setPersonalDbxrefs(new LinkedList());
			else
				preferences.getPersonalDbxrefs().clear();
			preferences.getPersonalDbxrefs().addAll(defDbxrefList.getData());
		} else {
			preferences.setPersonalDefinition(null);
			preferences.setPersonalDbxrefs(null);
		}

		Preferences.getPreferences().fireReconfigEvent(new ReconfigEvent(this));
	}

	public ConfigurationManager(String id) {
		super(id);
	}

	@Override
	public String getName() {
		return "OBO-Edit Configuration Manager";
	}

}
