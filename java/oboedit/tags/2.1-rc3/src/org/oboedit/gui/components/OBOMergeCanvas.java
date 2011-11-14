package org.oboedit.gui.components;

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.ArrayList;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

import org.bbop.dataadapter.DataAdapterException;
import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.GUIComponent;

import org.bbop.framework.dock.LayoutAdapter;
import org.bbop.framework.dock.LayoutListener;
import org.bbop.swing.SelectDialog;

import javax.swing.JFrame;


/*
 * By Jennifer Deegan and Nicolas Rodriguez
 * EMBL-EBI
 * January to May 2008
 * 
 * This is a user interface for the OBOMerge script. It takes the arguments provided by the user and sends them to the pre-existing script, then 
 * displays the script feedback in a JTextArea and allows the user to save this to a file. 
 * 
 * There is a save profiles feature half implemented but then commented out. This feature was not finished as 
 * it was not straightforward to make files in the default directory and create them on installation of the application. 
 * However, the GUI components were left in but commented in case they are ever needed. The feature was to work
 * like in the open and save files panels so that users would not have to enter all the paths each time. 
 * 
 * The OBOMerge option '-ignore-clash-on-id' is partially implemented but then commented out as this feature has never been used and no one 
 * is clear on how exactly to format the arguments for presentation to the script. 
 * 
 * The way this GUI works is to allow users to add file paths for all the ontology files and then to choose arguments from JComboBoxes. 
 * The various paths and other arguments are added to an array and then the array is changed to a string and sent to the OBOMerge script. The OBOMerge script code
 * has not been altered in any way. 
 * 
 */

import org.apache.log4j.*;

public class OBOMergeCanvas extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBOMergeCanvas.class);

	protected JPanel inputFilePanel = new JPanel();
	protected JPanel filePathPanel = new JPanel();
	protected JPanel mergeOptionPanel = new JPanel();
	//	JPanel saveProfilePanel = new JPanel();
	protected JPanel parentFilePanel = new JPanel();
	protected JPanel liveFilePanel = new JPanel();
	protected JPanel branchFilePanel = new JPanel();
	protected JPanel mergedFilePanel = new JPanel();
	
	protected JLabel parentFileLabel = new JLabel("Original File:      ");
	protected JTextField parentFileTextField = new JTextField(5);
	protected JButton parentFileBrowseButton = new JButton("Browse");
	protected JLabel branchFileLabel = new JLabel("Changed File 2: ");
	protected JTextField branchFileTextField = new JTextField(5);
	protected JButton branchFileBrowseButton = new JButton("Browse");
	protected JLabel liveFileLabel =   new JLabel("Changed File 1: ");
	protected JTextField liveFileTextField = new JTextField(5);
	protected JButton liveFileBrowseButton = new JButton("Browse");
	protected JLabel mergedFileLabel = new JLabel("Merged File:      ");
	protected JTextField mergedFileTextField = new JTextField(5);
	protected JButton mergedFileBrowseButton = new JButton("Browse");
	//	JLabel saveProfileLabel = new JLabel("Save Profile");
	//	String[] savedProfiles = {"new profile", ""};
	//	JComboBox saveProfileComboBox = new JComboBox(savedProfiles);
	//	JButton addProfilePlusButton = new JButton(new PlusIcon(1.5f, 8, 8));
	//	JButton removeProfileMinusButton = new JButton(new MinusIcon(1.5f, 8, 8));
	protected JPanel topLinePanel = new JPanel();
	protected JPanel bottomLinePanel = new JPanel();
	protected JLabel updateIDsLabel = new JLabel("Update IDs");
	protected String[] idOptionsFailOnClash = { "IF_LIKELY", "   ", "NEVER", "ALWAYS" };
	protected String[] idOptionsUpdateIDs = { "   ", "NEVER", "ALWAYS", "IF_LIKELY" };
	protected JComboBox updateIDsCombobox = new JComboBox(idOptionsUpdateIDs);
	protected JLabel mergedFileFormatLabel = new JLabel("Output File Format");
	protected String[] fileFormatOptions = { "OBO_1_2", "OBO_1_0" };
	protected JComboBox mergedFileFormatCombobox = new JComboBox(fileFormatOptions);
	protected JComponent failOnClashLabel = new JLabel("Fail On Clash");
	protected JComboBox failOnClashCombobox = new JComboBox(idOptionsFailOnClash);
	protected JLabel ignoreClashOnIDsLabel = new JLabel("Ignore Clash on ID");
	protected JTextArea ignoreClashOnIDsTextArea = new JTextArea();
	protected String id;
	protected JTabbedPane oboMergeTabbedPane = new JTabbedPane();
	protected JPanel processFeedbackPanel = new JPanel();
	protected JLabel saveFeedbackToFileLabel = new JLabel("Save Feedback to File");
	protected JTextField saveFeedbackToFileTextField = new JTextField();
	protected JTextField ProgressTextField = new JTextField();
	protected String saveFeedbackToFileTextFieldString = new String("C:\\output.txt");
	protected SelectDialog loadDialog = SelectDialog.getFileSelector(SelectDialog.LOAD, null);
	protected String updateIDsChoiceString = new String("   ");
	protected String failOnClashChoiceString = new String("IF_LIKELY");
	protected String outputFormatChoiceString = new String("OBO_1_2");
	protected String[] obomergeArgsArray = new String[0];

	protected JButton mergeButton = new JButton("Merge");
	protected JFrame missingPathFrame = new JFrame();
	protected ArrayList<String> obomergeArgsArrayList = new ArrayList<String>();
	protected String parentFileTextFieldString = new String();
	protected String liveFileTextFieldString = new String();
	protected String branchFileTextFieldString = new String();
	protected String mergedFileTextFieldString = new String();
	protected JPanel mergeAndAdvancedButtonPanel = new JPanel();
	protected JButton advancedButton = new JButton("Advanced");
	protected JPanel showProgressPanel = new JPanel();
	protected JPanel saveProgressToFilePanel = new JPanel();
	protected JPanel centerPanel = new JPanel();
	protected JPanel saveFeedbackToFileDetailPanel = new JPanel();
	protected String feedbackTextAreaString = new String();
	protected JTextArea feedbackTextArea = new JTextArea(feedbackTextAreaString);
	protected JLabel feedbackFilePathLabel = new JLabel("Save Feedback to File");
	protected JTextField feedbackFileTextField = new JTextField();
	protected String ignoreClashOnIDsChoiceString = new String();
	protected PrintStream feedbackFileOutputStream;
	protected JButton saveFeedbackToFileSaveButton = new JButton("Save");

	// Note, %n is newline
	protected SimpleLayout loggerLayout = new SimpleLayout();
	protected String pattern =  "%r %n";

	protected PatternLayout layout = new PatternLayout(pattern);
	protected ConsoleAppender console = new ConsoleAppender(layout);


	public static final int HIDE_ON_CLOSE = 1;
	private int defaultCloseOperation = HIDE_ON_CLOSE;
	protected LayoutListener layoutListener = new LayoutAdapter() {
		@Override
		public boolean closing(GUIComponent c) {
			if (c.equals(OBOMergeCanvas.this)) {
				save();
			}
			return true;
		}
	};

	public OBOMergeCanvas(String id) {
		super(id);
	}

	@Override
	public void init() {
		setLayout(new BorderLayout());
		JPanel mainGUIPanel = new JPanel();
		JScrollPane mainGUIPanelScrollPane = new JScrollPane(mainGUIPanel);

		//		Add tabs.
		add(oboMergeTabbedPane, "Center");
		oboMergeTabbedPane.addTab("Ontology Files", null, mainGUIPanelScrollPane, "Ontology Files");
		oboMergeTabbedPane.addTab("Process Feedback", null, processFeedbackPanel, "Process Feedback");
		//		End of tabs.

		//		Layout of main input panel.              
		mainGUIPanel.setLayout(new GridBagLayout());
		GridBagConstraints mainGUIPanelGBC = new GridBagConstraints();

		mainGUIPanelGBC.fill = GridBagConstraints.HORIZONTAL;
		//	mainGUIPanelGBC.gridx = 0;
		//	mainGUIPanelGBC.gridy = 0;
		mainGUIPanelGBC.weightx = 1;
		mainGUIPanelGBC.insets = new Insets(5,5,5,5);
		//	mainGUIPanel.add(saveProfilePanel, mainGUIPanelGBC);
		//	saveProfilePanel.setBorder(new TitledBorder ("Saved Profiles"));

		mainGUIPanelGBC.gridy = 0;
		mainGUIPanel.add(inputFilePanel, mainGUIPanelGBC);
		inputFilePanel.setBorder(new TitledBorder ("File Paths"));

		mainGUIPanelGBC.gridy = 1;
		mainGUIPanel.add(mergeOptionPanel, mainGUIPanelGBC);
		mergeOptionPanel.setBorder(new TitledBorder ("Merge Options"));

		mainGUIPanelGBC.gridy = 2;
		mainGUIPanel.add(mergeAndAdvancedButtonPanel, mainGUIPanelGBC);
		mergeOptionPanel.setBorder(new TitledBorder ("Merge Options"));
		//		End of layout of main input panel

		//		Start of basic panel where the four file paths go.
		//		Add a panel for each file.       
		inputFilePanel.setLayout(new GridBagLayout());
		GridBagConstraints inputFilePanelGBC = new GridBagConstraints();

		//		Parent file panel
		inputFilePanelGBC.fill = GridBagConstraints.HORIZONTAL;
		inputFilePanelGBC.gridx = 0;
		inputFilePanelGBC.gridy = 0;
		inputFilePanelGBC.anchor = GridBagConstraints.FIRST_LINE_START;
		inputFilePanelGBC.weightx = 1;
		inputFilePanelGBC.insets = new Insets(5,5,5,5);
		inputFilePanel.add(parentFilePanel, inputFilePanelGBC);

		//		Live file panel              
		inputFilePanelGBC.gridy = 1;
		inputFilePanel.add(liveFilePanel, inputFilePanelGBC);

		//		Branch file panel
		inputFilePanelGBC.gridy = 2;
		inputFilePanel.add(branchFilePanel, inputFilePanelGBC);

		//		Merged file panel              
		inputFilePanelGBC.gridy = 3;
		inputFilePanel.add(mergedFilePanel, inputFilePanelGBC);

		//		End of setting up a panel for each input file.       

		//		Start setting up the contents of the file input panels.

		//		Start Parent file panel contents.
		parentFilePanel.setLayout(new GridBagLayout());
		GridBagConstraints parentFilePanelGBC = new GridBagConstraints();

		//		Parent file label
		parentFilePanelGBC.fill = GridBagConstraints.NONE;
		parentFilePanelGBC.gridx = 0;
		parentFilePanelGBC.gridy = 0;
		parentFilePanelGBC.anchor = GridBagConstraints.LINE_START;
		parentFilePanelGBC.insets = new Insets(0,2,2,0);
		parentFilePanelGBC.weightx = 0;
		parentFilePanel.add(parentFileLabel, parentFilePanelGBC);

		//		Parent file text field.              
		parentFilePanelGBC.fill = GridBagConstraints.HORIZONTAL;
		parentFilePanelGBC.gridx = 1;
		parentFilePanelGBC.anchor = GridBagConstraints.CENTER;
		parentFilePanelGBC.weightx = 1;
		parentFilePanel.add(parentFileTextField, parentFilePanelGBC);

		//		Parent file Browse button.               
		parentFilePanelGBC.fill = GridBagConstraints.NONE;
		parentFilePanelGBC.gridx = 2;
		parentFilePanelGBC.anchor = GridBagConstraints.LINE_END;
		parentFilePanelGBC.weightx = 0;
		parentFilePanel.add(parentFileBrowseButton, parentFilePanelGBC);
		//		End of Parent file Panel

		//		Start of Live file Panel contents.              
		liveFilePanel.setLayout(new GridBagLayout());
		GridBagConstraints liveFilePanelGBC = new GridBagConstraints();

		//		Live file Label.              
		liveFilePanelGBC.fill = GridBagConstraints.NONE;
		liveFilePanelGBC.gridx = 0;
		liveFilePanelGBC.gridy = 0;
		liveFilePanelGBC.anchor = GridBagConstraints.LINE_START;
		liveFilePanelGBC.insets = new Insets(0,2,2,0);
		liveFilePanel.add(liveFileLabel, liveFilePanelGBC);

		//		Live file Text Field.              
		liveFilePanelGBC.fill = GridBagConstraints.HORIZONTAL;
		liveFilePanelGBC.gridx = 1;
		liveFilePanelGBC.anchor = GridBagConstraints.CENTER;
		liveFilePanelGBC.weightx = 1;
		liveFilePanel.add(liveFileTextField, liveFilePanelGBC);

		//		Live file Browse Button.               
		liveFilePanelGBC.fill = GridBagConstraints.NONE;
		liveFilePanelGBC.gridx = 2;
		liveFilePanelGBC.anchor = GridBagConstraints.LINE_END;
		liveFilePanelGBC.weightx = 0;
		liveFilePanel.add(liveFileBrowseButton, liveFilePanelGBC);
		//		End of live file panel contents.

		//		Start of Branch file panel contents.
		branchFilePanel.setLayout(new GridBagLayout());
		GridBagConstraints branchFilePanelGBC = new GridBagConstraints();

		//		Branch file label
		branchFilePanelGBC.fill = GridBagConstraints.NONE;
		branchFilePanelGBC.gridx = 0;
		branchFilePanelGBC.gridy = 0;
		branchFilePanelGBC.weightx = 0;
		branchFilePanelGBC.anchor = GridBagConstraints.LINE_START;
		branchFilePanelGBC.insets = new Insets(0,2,2,0);
		branchFilePanel.add(branchFileLabel, branchFilePanelGBC);

		//		Branch file text field.               
		branchFilePanelGBC.fill = GridBagConstraints.HORIZONTAL;
		branchFilePanelGBC.weightx = 1;
		branchFilePanelGBC.gridx = 1;
		branchFilePanelGBC.anchor = GridBagConstraints.CENTER;
		branchFilePanel.add(branchFileTextField, branchFilePanelGBC);

		//		Branch file browse button              
		branchFilePanelGBC.fill = GridBagConstraints.NONE;
		branchFilePanelGBC.gridx = 2;
		branchFilePanelGBC.anchor = GridBagConstraints.LINE_END;
		branchFilePanelGBC.weightx = 0;
		branchFilePanel.add(branchFileBrowseButton, branchFilePanelGBC);
		//		End of branch file panel contents.

		//		Start of merged file panel contents.
		mergedFilePanel.setLayout(new GridBagLayout());
		GridBagConstraints mergedFilePanelGBC = new GridBagConstraints();

		//		Merged file label.              
		mergedFilePanelGBC.fill = GridBagConstraints.NONE;
		mergedFilePanelGBC.gridx = 0;
		mergedFilePanelGBC.gridy = 0;
		mergedFilePanelGBC.anchor = GridBagConstraints.LINE_START;
		mergedFilePanelGBC.insets = new Insets(0,2,2,0);
		mergedFilePanel.add(mergedFileLabel, mergedFilePanelGBC);

		//		Merged file Text field.
		mergedFilePanelGBC.fill = GridBagConstraints.HORIZONTAL;
		mergedFilePanelGBC.gridx = 1;
		mergedFilePanelGBC.anchor = GridBagConstraints.CENTER;
		mergedFilePanelGBC.weightx = 1;
		mergedFilePanel.add(mergedFileTextField, mergedFilePanelGBC);

		//		Merged file browse button.               
		mergedFilePanelGBC.fill = GridBagConstraints.NONE;
		mergedFilePanelGBC.gridx = 2;
		mergedFilePanelGBC.anchor = GridBagConstraints.LINE_END;
		mergedFilePanelGBC.weightx = 0;
		mergedFilePanel.add(mergedFileBrowseButton, mergedFilePanelGBC);
		//		End of merged file panel contents.


		//		Start of save profile panel contents.
		//		saveProfilePanel.setLayout(new GridBagLayout());
		//		GridBagConstraints saveProfilePanelGBC = new GridBagConstraints();

		//		Save profile label.              
		//		saveProfilePanelGBC.fill = GridBagConstraints.NONE;
		//		saveProfilePanelGBC.gridx = 0;
		//		saveProfilePanelGBC.gridy = 0;
		//		saveProfilePanelGBC.anchor = GridBagConstraints.LINE_START;
		//		saveProfilePanelGBC.insets = new Insets(5,5,5,5);
		//		saveProfilePanel.add(saveProfileLabel, saveProfilePanelGBC);

		//		Save profile editable combobox.              
		//		saveProfilePanelGBC.fill = GridBagConstraints.HORIZONTAL;
		//		saveProfilePanelGBC.gridx = 1;
		//		saveProfilePanelGBC.anchor = GridBagConstraints.CENTER;
		//		saveProfilePanelGBC.weightx = 1;
		//		saveProfilePanel.add(saveProfileComboBox, saveProfilePanelGBC);
		//		saveProfileComboBox.setEditable(true);

		//		Save profile plus button.               
		//		saveProfilePanelGBC.fill = GridBagConstraints.NONE;
		//		saveProfilePanelGBC.gridx = 2;
		//		saveProfilePanelGBC.weightx = 0;
		//		saveProfilePanel.add(addProfilePlusButton, saveProfilePanelGBC);

		//		Save profile minus button.
		//		saveProfilePanelGBC.gridx = 3;
		//		saveProfilePanelGBC.anchor = GridBagConstraints.LINE_END;
		//		saveProfilePanelGBC.weightx = 0;
		//		saveProfilePanel.add(removeProfileMinusButton, saveProfilePanelGBC);
		//		End of save profile panel contents.

		//		Start of merge option panel contents.
		mergeOptionPanel.setLayout(new GridBagLayout());
		GridBagConstraints mergeOptionPanelGBC = new GridBagConstraints();

		//		Update ID control label.                
		mergeOptionPanelGBC.fill = GridBagConstraints.NONE;
		mergeOptionPanelGBC.gridx = 0;
		mergeOptionPanelGBC.gridy = 0;
		mergeOptionPanelGBC.ipadx = 5;
		mergeOptionPanelGBC.ipady = 5;
		mergeOptionPanelGBC.insets = new Insets(5,5,5,5);
		mergeOptionPanel.add(updateIDsLabel, mergeOptionPanelGBC);

		//		Update ID combobox
		mergeOptionPanelGBC.gridx = 1;
		mergeOptionPanel.add(updateIDsCombobox, mergeOptionPanelGBC);

		//		Merged file format label.
		mergeOptionPanelGBC.gridx = 2;
		mergeOptionPanel.add(mergedFileFormatLabel, mergeOptionPanelGBC);

		//		Merged file format combobox.              
		mergeOptionPanelGBC.gridx = 3;
		mergeOptionPanel.add(mergedFileFormatCombobox, mergeOptionPanelGBC);

		//		Fail on clash option label.               
		mergeOptionPanelGBC.gridx = 0;
		mergeOptionPanelGBC.gridy = 1;
		mergeOptionPanel.add(failOnClashLabel, mergeOptionPanelGBC);

		//		Fail on clash combobox              
		mergeOptionPanelGBC.gridx = 1;
		mergeOptionPanelGBC.gridy = 1;
		mergeOptionPanel.add(failOnClashCombobox, mergeOptionPanelGBC);

		//		If we ever want to implement the IgnoreClashOnIDs option in the script then this commented
		//		out code will produce the GUI items required. The processing code has not been written.        
		//        mergeOptionPanelGBC.gridx = 2;
		//		mergeOptionPanelGBC.gridy = 1;
		//		mergeOptionPanel.add(ignoreClashOnIDsLabel, mergeOptionPanelGBC);

		//		mergeOptionPanelGBC.gridx = 3;
		//		mergeOptionPanelGBC.gridy = 1;
		//		mergeOptionPanel.add(ignoreClashOnIDsTextArea, mergeOptionPanelGBC);

		//		End of merge option panel contents.       
		//		Start of contents of panel with merge button and advanced button. (Bottom panel)              
		mergeAndAdvancedButtonPanel.setLayout(new GridBagLayout());
		GridBagConstraints mergeAndAdvancedButtonPanelGBC = new GridBagConstraints();

		//		Advanced button.              
		mergeAndAdvancedButtonPanelGBC.fill = GridBagConstraints.NONE;
		mergeAndAdvancedButtonPanelGBC.gridx = 0;
		mergeAndAdvancedButtonPanelGBC.gridy = 0;
		mergeAndAdvancedButtonPanelGBC.weightx = 1;
		mergeAndAdvancedButtonPanel.add(advancedButton, mergeAndAdvancedButtonPanelGBC);

		//		Merge button              
		mergeAndAdvancedButtonPanelGBC.gridx = 1;
		mergeAndAdvancedButtonPanel.add(mergeButton, mergeAndAdvancedButtonPanelGBC);
		//		End of contents of panel with advanced button and merge button on it.

		//		Start of contents of panel in other tab that shows progress.              
		processFeedbackPanel.setLayout(new GridBagLayout());
		GridBagConstraints processFeedbackPanelGBC = new GridBagConstraints();

		//		Scroll pane enclosing progress window.               
		JScrollPane feedbackTextAreaScrollPane = new JScrollPane(feedbackTextArea);

		//		Scroll pane enclosing progress window now on tab.               
		processFeedbackPanelGBC.fill = GridBagConstraints.BOTH;
		processFeedbackPanelGBC.gridx = 0;
		processFeedbackPanelGBC.gridy = 0;
		processFeedbackPanelGBC.anchor = GridBagConstraints.PAGE_START;
		processFeedbackPanelGBC.weightx = 1;
		processFeedbackPanelGBC.weighty = 1;
		processFeedbackPanelGBC.insets = new Insets(5,5,5,5);
		processFeedbackPanel.add(feedbackTextAreaScrollPane, processFeedbackPanelGBC);

		//		Panel to contain part to let you select where progress feedback is saved to a file.               
		processFeedbackPanelGBC.fill = GridBagConstraints.HORIZONTAL;
		processFeedbackPanelGBC.gridy = 1;
		processFeedbackPanelGBC.weightx = 1;
		processFeedbackPanelGBC.weighty = 0;
		processFeedbackPanelGBC.anchor = GridBagConstraints.PAGE_END;
		processFeedbackPanel.add(saveFeedbackToFileDetailPanel, processFeedbackPanelGBC);

		//		Contents of panel for saving feedback to file.
		saveFeedbackToFileDetailPanel.setLayout(new GridBagLayout());
		GridBagConstraints saveFeedbackToFileDetailPanelGBC = new GridBagConstraints();

		//		Save feedback to file label.
		saveFeedbackToFileDetailPanelGBC.fill = GridBagConstraints.NONE;
		saveFeedbackToFileDetailPanelGBC.gridx = 0;
		saveFeedbackToFileDetailPanelGBC.gridy = 0;
		saveFeedbackToFileDetailPanelGBC.weightx = 0;
		saveFeedbackToFileDetailPanelGBC.insets = new Insets(5,5,5,5);
		saveFeedbackToFileDetailPanel.add(feedbackFilePathLabel, saveFeedbackToFileDetailPanelGBC);

		//Save feedback to file text field.
		saveFeedbackToFileDetailPanelGBC.fill = GridBagConstraints.HORIZONTAL;
		saveFeedbackToFileDetailPanelGBC.gridx = 1;
		saveFeedbackToFileDetailPanelGBC.weightx = 1;
		saveFeedbackToFileDetailPanel.add(saveFeedbackToFileTextField, saveFeedbackToFileDetailPanelGBC);


		//Save feedback to file save button.
		saveFeedbackToFileDetailPanelGBC.fill = GridBagConstraints.NONE;
		saveFeedbackToFileDetailPanelGBC.gridx = 2;
		saveFeedbackToFileDetailPanelGBC.gridy = 0;
		saveFeedbackToFileDetailPanelGBC.weightx = 0;
		saveFeedbackToFileDetailPanel.add(saveFeedbackToFileSaveButton, saveFeedbackToFileDetailPanelGBC);
		//End of save feedback to file panel contents.

		//		These to lines are needed or the GUI components will not show up.               
		validate();
		repaint();

		//        Start of add action listeners.              
		//		addProfilePlusButton.addActionListener(new java.awt.event.ActionListener() {
		//			public void actionPerformed(java.awt.event.ActionEvent e) {
		//				addProfilePlusButtonActionPerformed(e);
		//			}
		//		});

		mergedFileFormatCombobox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				mergedFileFormatComboboxActionPerformed(e);
			}
		});

		updateIDsCombobox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				updateIDsComboboxActionPerformed(e);
			}
		});

		failOnClashCombobox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				failOnClashChoiceComboBoxActionPerformed(e);
			}
		});

		parentFileBrowseButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				parentFileButtonActionPerformed(e);
			}
		});

		mergedFileTextField.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				mergedFileTextFieldActionPerformed(e);
			}
		});

		saveFeedbackToFileSaveButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					saveFeedbackToFileSaveButtonActionPerformed(e);
				} catch (IOException ex) {
					ex.printStackTrace();
				}
			}
		});


		branchFileBrowseButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				branchFileBrowseButtonActionPerformed(e);
			}
		});

		liveFileBrowseButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				liveFileBrowseButtonActionPerformed(e);
			}
		});

		mergeButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				mergeButtonActionPerformed(e);
			}
		});

		mergedFileBrowseButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				mergedFileBrowseButtonActionPerformed(e);
			}
		});

		advancedButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				advancedButtonActionPerformed(e);
			}
			//End of add action listeners

			//Start of action performeds.
			//Advance button action                      
			private void advancedButtonActionPerformed(ActionEvent e) {
				//	boolean saveProfilePanelVisibility = saveProfilePanel.isVisible();
				//	saveProfilePanel.setVisible(!saveProfilePanelVisibility);
				boolean mergeOptionPanelVisibility = mergeOptionPanel.isVisible();
				mergeOptionPanel.setVisible(!mergeOptionPanelVisibility);
				if (mergeOptionPanelVisibility) {
					advancedButton.setText("Advanced");
				} else {
					advancedButton.setText("Basic");
				}
			}
		});
		//		saveProfilePanel.setVisible(false);
		mergeOptionPanel.setVisible(false);
	}

	protected void addProfilePlusButtonActionPerformed(ActionEvent e) {

	}

	//	This is part of the mechanism to close the dialog now that shows if someone has not filled
	//	in all the file paths.       
	public void save(){
		//		logger.info("all going well so far.");
	}


	private void branchFileBrowseButtonActionPerformed(ActionEvent e) {
		loadDialog.show();
		File selected = loadDialog.getSelected();
		if (selected != null) {
			branchFileTextField.setText(selected.getAbsolutePath());
			branchFileTextFieldString = branchFileTextField.getText();
			// logger.info("arg = " + branchFileTextFieldString);
		}
	}

	protected void saveFeedbackToFileSaveButtonActionPerformed(ActionEvent e) throws IOException {
		SelectDialog dialog = SelectDialog.getFileSelector(SelectDialog.SAVE, null);
		dialog.show();
		File selected = dialog.getSelected();
		if (selected != null) {
			selected.createNewFile();
			saveFeedbackToFileTextField.setText(selected.getAbsolutePath());
			String feedbackTextAreaContents = feedbackTextArea.getText();
			BufferedWriter out = new BufferedWriter(new FileWriter(selected));
			out.write(feedbackTextAreaContents);
			out.flush();
			out.close();
		}
	}


	private void failOnClashChoiceComboBoxActionPerformed(ActionEvent e) {
		failOnClashChoiceString = (String) failOnClashCombobox
		.getSelectedItem();
		//		logger.info("arg = " + failOnClashChoiceString);
	}

	//    This is part of the mechanism to close the dialog now that shows if someone has not filled
	//	in all the file paths.       
	private int getDefaultCloseOperation() {
		// TODO Auto-generated method stub
		return defaultCloseOperation;

	};

	//    This method takes all the different variables from the various controls on the file input page
	//	and makes them into an array that can be fed to the OBOMerge script which is in a totally
	//	different place.
	private Boolean makeArgArrayList() {
		/*
		 * This class takes the return strings from all the
		 *  GUI controls and puts them
		 * into the array to be fed to obomerge.
		 */

		obomergeArgsArrayList.clear();

		logger.info("Arguments applied were:\n");

		if (!failOnClashChoiceString.trim().equals("")) {
			obomergeArgsArrayList.add("-fail-on-clash");
			obomergeArgsArrayList.add(failOnClashChoiceString);
			logger.info("    -fail-on-clash " + failOnClashChoiceString);

		}

		if (!updateIDsChoiceString.trim().equals("")) {
			obomergeArgsArrayList.add("-update-ids");
			obomergeArgsArrayList.add(updateIDsChoiceString);
			logger.info("    -update-ids " + updateIDsChoiceString);
		}
		//This feature not implemented.
		//		if (ignoreClashOnIDsChoiceString != "") {
		//		obomergeArgsArrayList.add("-ignore-clash-on-id");
		//		obomergeArgsArrayList.add(ignoreClashOnIDsChoiceString);
		//		logger.info("    -ignore-clash-on-id "
		//		+ ignoreClashOnIDsChoiceString);
		//		}


		parentFileTextFieldString = parentFileTextField.getText();
		liveFileTextFieldString = liveFileTextField.getText();
		branchFileTextFieldString = branchFileTextField
		.getText();
		mergedFileTextFieldString = mergedFileTextField.getText();

		if (parentFileTextFieldString.length() == 0
				|| liveFileTextFieldString.length() == 0
				|| branchFileTextFieldString.length() == 0
				|| mergedFileTextFieldString.length() == 0) {

			JOptionPane.showMessageDialog(missingPathFrame,
					"Please fill in all of the necessary file paths.",
					"Missing Information", getDefaultCloseOperation());

			return false;
		}
		obomergeArgsArrayList.add("-version");
		obomergeArgsArrayList.add(outputFormatChoiceString);
		logger.info("    -version " + outputFormatChoiceString);

		obomergeArgsArrayList.add("-original");
		obomergeArgsArrayList.add(parentFileTextFieldString);
		logger.info("    -original " + parentFileTextFieldString);

		obomergeArgsArrayList.add("-revision");
		obomergeArgsArrayList.add(liveFileTextFieldString);
		logger.info("    -revision " + liveFileTextFieldString);

		obomergeArgsArrayList.add("-revision");
		obomergeArgsArrayList.add(branchFileTextFieldString);
		logger.info("    -revision "
				+ branchFileTextFieldString);

		obomergeArgsArrayList.add("-o");
		obomergeArgsArrayList.add(mergedFileTextFieldString);
		logger.info("    -o " + mergedFileTextFieldString);

		//logger.info(obomergeArgsArrayList);
		obomergeArgsArrayList.trimToSize();
		obomergeArgsArray = obomergeArgsArrayList.toArray(obomergeArgsArray);

		logger.info("\nThe merge process gave the following feedback:\n");
		return true;
	}

	//Mechanism fired when the merge button is pressed. Take the array and sends it to the obomerge script.
	private void mergeButtonActionPerformed(ActionEvent e) {
		oboMergeTabbedPane.setSelectedIndex(1);
		repaint();      
		writeFeedbkToTxtArea();
		if (makeArgArrayList() == true) {
			try {
				org.oboedit.launcher.OBOMerge.main(obomergeArgsArray);
			} catch (IOException ex) {
				logger.error("There has been an error in reading one or " +
						"more of the input files. Only " +
				"OBO format files can be used with this utility.");
				ex.printStackTrace();
			} catch (DataAdapterException ex) {
				logger.error("There has been an error in reading one or " +
						"more of the input files. Only " +
				"OBO format files can be used with this utility.");
				ex.printStackTrace();
			}
			catch (RuntimeException ex) {
				ex.printStackTrace();
			}          }
		//Write list of profiles out now.
	}
	private void mergedFileBrowseButtonActionPerformed(ActionEvent e) {
		loadDialog.show();
		String selected = loadDialog.getSelectedCanonicalPath();
		if (selected != null) {
			mergedFileTextFieldString = selected;
			mergedFileTextField.setText(mergedFileTextFieldString);
			//logger.info("arg = " + mergedFileTextFieldString);
		}
	}
	private void mergedFileFormatComboboxActionPerformed(ActionEvent e) {
		outputFormatChoiceString = (String) mergedFileFormatCombobox.getSelectedItem();
	}
	private void mergedFileTextFieldActionPerformed(ActionEvent e) {
		loadDialog.show();
		String selected = loadDialog.getSelectedCanonicalPath();
		if (selected != null) {
			mergedFileTextFieldString = selected;
			mergedFileTextField.setText(mergedFileTextFieldString);
			//logger.info("arg = " + mergedFileTextFieldString);
		}

	}


	private void parentFileButtonActionPerformed(ActionEvent e) {
		loadDialog.show();
		String selected = loadDialog.getSelectedCanonicalPath();
		if (selected != null) {
			parentFileTextFieldString = selected;
			parentFileTextField.setText(parentFileTextFieldString);
			//			logger.info("arg = " + parentFileTextFieldString);
		}
	}

	private void updateIDsComboboxActionPerformed(ActionEvent e) {
		updateIDsChoiceString = (String) updateIDsCombobox.getSelectedItem();
		//    logger.info("arg = " + updateIDsChoiceString);
	}

	private void writeFeedbkToTxtArea() {
		PrintStream toTextArea = new PrintStream( new TextAreaOutputStream( feedbackTextArea ) );
		logger.addAppender(new WriterAppender(loggerLayout, toTextArea));
		//logger.setLevel((Level) Level.INFO);
		System.setOut(toTextArea);
		System.setErr(toTextArea);
		logger.error(toTextArea);
	}

	protected void liveFileBrowseButtonActionPerformed(ActionEvent e) {
		loadDialog.show();
		String selected = loadDialog.getSelectedCanonicalPath();
		if (selected != null) {
			liveFileTextFieldString = selected;
			liveFileTextField.setText(liveFileTextFieldString);
			//logger.info("arg = " + liveFileTextFieldString);
		}
	}

	/**
	 * @author  Ranganath Kini
	 * @see      javax.swing.JTextArea
	 * http://www.jcreator.com/forums/index.php?showtopic=773
	 */
	private class TextAreaOutputStream extends OutputStream {
		private JTextArea textControl;
		public TextAreaOutputStream(JTextArea control) {
			textControl = control;
		}
		@Override
		public void write(int b) throws IOException {
			// append the data as characters to the JTextArea control
			textControl.append(String.valueOf((char) b));
		}
	}

}

