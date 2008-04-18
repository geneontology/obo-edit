package org.oboedit.gui.components;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.ArrayList;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

import org.bbop.dataadapter.DataAdapterException;
import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.GUIComponent;
import org.bbop.framework.dock.LayoutAdapter;
import org.bbop.framework.dock.LayoutListener;
import org.bbop.swing.MinusIcon;
import org.bbop.swing.PlusIcon;
import javax.swing.JFrame;


public class OBOMergeCanvasMark2 extends AbstractGUIComponent {


	/**
	 * @author  Ranganath Kini
	 * @see      javax.swing.JTextArea
	 * http://www.jcreator.com/forums/index.php?showtopic=773
	 */
	public class TextAreaOutputStream extends OutputStream {
		private JTextArea textControl;

		public TextAreaOutputStream(JTextArea control) {
			textControl = control;
		}

		public void write(int b) throws IOException {
			// append the data as characters to the JTextArea control
			textControl.append(String.valueOf((char) b));
		}
	}
	/**
	 * @author  Ranganath Kini
	 * @see      javax.swing.JTextArea
	 * http://www.jcreator.com/forums/index.php?showtopic=773
	 */
	private class TextAreaOutputStream1 extends OutputStream {
		private JTextArea textControl;
		public void TextAreaOutputStream(JTextArea control) {
			textControl = control;
		}
		@Override
		public void write(int b) throws IOException {
			// append the data as characters to the JTextArea control
			textControl.append(String.valueOf((char) b));
		}
	}
	public static final int HIDE_ON_CLOSE = 1;
	public static void main(String args[]) {
		java.awt.EventQueue.invokeLater(new Runnable() {
			public void run() {
				new OBOMergeCanvas().setVisible(true);
			}
		});
	}
	private static void addAComponentXAlignment(JComponent componentName, Container container) {
//		componentName.setAlignmentX(Component.CENTER_ALIGNMENT);
		container.add(componentName);

	}
	private static void addAComponentYAlignment(JComponent componentName, Container container) {
//		componentName.setAlignmentY(Component.CENTER_ALIGNMENT);
		container.add(componentName);

	}
	private int defaultCloseOperation = HIDE_ON_CLOSE;
	protected LayoutListener layoutListener = new LayoutAdapter() {
		public boolean closing(GUIComponent c) {
			if (c.equals(OBOMergeCanvasMark2.this)) {
				save();
			}
			return true;
		}

	};
	JPanel inputFilePanel = new JPanel();
	JPanel filePathPanel = new JPanel();
	JPanel mergeOptionPanel = new JPanel();
	JPanel saveProfilePanel = new JPanel();
	JPanel parentFilePanel = new JPanel();
	JPanel liveFilePanel = new JPanel();
	JPanel branchFilePanel = new JPanel();
	JPanel mergedFilePanel = new JPanel();
	JLabel parentFileLabel = new JLabel("Parent File");
	JTextField parentFileTextField = new JTextField("                      ");
	JButton parentFileBrowseButton = new JButton("Browse");
	JLabel branchFileLabel = new JLabel("Branch File");
	JTextField branchFileTextField = new JTextField("                      ");
	JButton branchFileBrowseButton = new JButton("Browse");
	JLabel liveFileLabel = new JLabel("Live File");
	JTextField liveFileTextField = new JTextField("                      ");
	JButton liveFileBrowseButton = new JButton("Browse");
	JLabel mergedFileLabel = new JLabel("Merged File");
	JTextField mergedFileTextField = new JTextField("                      ");
	JButton mergedFileBrowseButton = new JButton("Browse");
	JLabel saveProfileLabel = new JLabel("Save Profile");
	String[] savedProfiles = {"new profile", ""};
	JComboBox saveProfileComboBox = new JComboBox(savedProfiles);
	JButton addProfilePlusButton = new JButton(new PlusIcon(1.5f, 8, 8));
	JButton removeProfileMinusButton = new JButton(new MinusIcon(1.5f, 8, 8));
	JPanel topLinePanel = new JPanel();
	JPanel bottomLinePanel = new JPanel();
	JLabel updateIDsLabel = new JLabel("Update IDs");
	String[] idOptions = { "NEVER", "ALWAYS", "IF_LIKELY" };
	JComboBox updateIDsCombobox = new JComboBox(idOptions);
	JLabel mergedFileFormatLabel = new JLabel("Output File Format");
	String[] fileFormatOptions = { "OBO_1_2", "OBO_1_0" };
	JComboBox mergedFileFormatCombobox = new JComboBox(fileFormatOptions);
	JComponent failOnClashLabel = new JLabel("Fail On Clash");
	JComboBox failOnClashCombobox = new JComboBox(idOptions);
	JLabel ignoreClashOnIDsLabel = new JLabel("Ignore Clash on ID");
	JTextArea ignoreClashOnIDsTextArea = new JTextArea();
	String id;
	JTabbedPane oboMergeTabbedPane = new JTabbedPane();
	JPanel processFeedbackPanel = new JPanel();
	JButton saveFeedbackToFileBrowseButton = new JButton("Browse");
	JLabel saveFeedbackToFileLabel = new JLabel("Save Feedback to File");
	JTextField saveFeedbackToFileTextField = new JTextField();
	JTextField ProgressTextField = new JTextField();
	String saveFeedbackToFileTextFieldString = new String();
	JFileChooser fileChooser = new JFileChooser();
	String updateIDsChoiceString = new String();
	String failOnClashChoiceString = new String();
	String outputFormatChoiceString = new String();
	String[] obomergeArgsArray = new String[0];
	JButton mergeButton = new JButton("Merge");
	JFrame missingPathFrame = new JFrame();
	ArrayList<String> obomergeArgsArrayList = new ArrayList<String>();
	String parentFileTextFieldString = new String();
	String liveFileTextFieldString = new String();
	String branchFileTextFieldString = new String();
	String mergedFileTextFieldString = new String();
	JPanel finalOptionPanel = new JPanel();
	JButton advancedButton = new JButton("Advanced");
	JPanel showProgressPanel = new JPanel();
	JPanel saveProgressToFilePanel = new JPanel();

	
	public OBOMergeCanvasMark2(String id) {
		super(id);
	}

	@Override
	public void init() {

		setLayout(new BorderLayout());
		JPanel mainGUIPanel = new JPanel();

		add(oboMergeTabbedPane, "Center");
		oboMergeTabbedPane.addTab("Ontology Files", null, mainGUIPanel, "Ontology Files");
		oboMergeTabbedPane.addTab("Process Feedback", null, processFeedbackPanel, "Process Feedback");

		mainGUIPanel.setLayout(new BorderLayout());

		mainGUIPanel.add(saveProfilePanel, BorderLayout.PAGE_START);
		saveProfilePanel.setBorder(new TitledBorder ("Saved Profiles"));
		mainGUIPanel.add(inputFilePanel, BorderLayout.CENTER);
		inputFilePanel.setBorder(new TitledBorder ("Ontology File Paths"));
		mainGUIPanel.add(mergeOptionPanel, BorderLayout.PAGE_END);
		mergeOptionPanel.setBorder(new TitledBorder ("Merge Options"));
		mainGUIPanel.add(finalOptionPanel, BorderLayout.LINE_END);
		finalOptionPanel.setBorder(new TitledBorder ("Final Options"));

		
		//Make GridBag layout for the contents of the inputFilePanel. 
		inputFilePanel.setLayout(new GridBagLayout());
		GridBagConstraints inputFilePanelGBC = new GridBagConstraints();

		inputFilePanelGBC.fill = GridBagConstraints.HORIZONTAL;
		inputFilePanelGBC.gridx = 0;
		inputFilePanelGBC.gridy = 0;
		inputFilePanelGBC.anchor = GridBagConstraints.FIRST_LINE_START;
		inputFilePanelGBC.weightx = 1;
		inputFilePanelGBC.insets = new Insets(5,5,5,5);
		//Add the four horizontal panels to take a path each. 
		inputFilePanel.add(parentFilePanel, inputFilePanelGBC);

		inputFilePanelGBC.gridx = 0;
		inputFilePanelGBC.gridy = 1;

		inputFilePanel.add(liveFilePanel, inputFilePanelGBC);
		inputFilePanelGBC.gridx = 0;
		inputFilePanelGBC.gridy = 2;

		inputFilePanel.add(branchFilePanel, inputFilePanelGBC);
		inputFilePanelGBC.gridx = 0;
		inputFilePanelGBC.gridy = 3;

		inputFilePanel.add(mergedFilePanel, inputFilePanelGBC);

	
	
		
		//set up the contents for the parent file path. 
		parentFilePanel.setLayout(new GridBagLayout());
		GridBagConstraints parentFilePanelGBC = new GridBagConstraints();

		parentFilePanelGBC.fill = GridBagConstraints.NONE;
		parentFilePanelGBC.gridx = 0;
		parentFilePanelGBC.gridy = 0;
		parentFilePanelGBC.anchor = GridBagConstraints.LINE_START;
		parentFilePanelGBC.insets = new Insets(0,2,2,0);
		parentFilePanel.add(parentFileLabel, parentFilePanelGBC);

		parentFilePanelGBC.fill = GridBagConstraints.HORIZONTAL;
		parentFilePanelGBC.gridx = 1;
		parentFilePanelGBC.gridy = 0;
		parentFilePanelGBC.anchor = GridBagConstraints.CENTER;
		parentFilePanelGBC.weightx = 1;

		parentFilePanel.add(parentFileTextField, parentFilePanelGBC);

		parentFilePanelGBC.fill = GridBagConstraints.NONE;
		parentFilePanelGBC.gridx = 2;
		parentFilePanelGBC.gridy = 0;
		parentFilePanelGBC.anchor = GridBagConstraints.LINE_END;
		parentFilePanelGBC.weightx = 0;

		parentFilePanel.add(parentFileBrowseButton, parentFilePanelGBC);

		//set up the contents for the parent file path. 
		liveFilePanel.setLayout(new GridBagLayout());
		GridBagConstraints liveFilePanelGBC = new GridBagConstraints();

		liveFilePanelGBC.fill = GridBagConstraints.NONE;
		liveFilePanelGBC.gridx = 0;
		liveFilePanelGBC.gridy = 0;
		liveFilePanelGBC.anchor = GridBagConstraints.LINE_START;
		liveFilePanelGBC.insets = new Insets(0,2,2,0);
		liveFilePanel.add(liveFileLabel, liveFilePanelGBC);

		liveFilePanelGBC.fill = GridBagConstraints.HORIZONTAL;
		liveFilePanelGBC.gridx = 1;
		liveFilePanelGBC.gridy = 0;
		liveFilePanelGBC.anchor = GridBagConstraints.CENTER;
		liveFilePanelGBC.weightx = 1;

		liveFilePanel.add(liveFileTextField, liveFilePanelGBC);

		liveFilePanelGBC.fill = GridBagConstraints.NONE;
		liveFilePanelGBC.gridx = 2;
		liveFilePanelGBC.gridy = 0;
		liveFilePanelGBC.anchor = GridBagConstraints.LINE_END;
		liveFilePanelGBC.weightx = 0;

		liveFilePanel.add(liveFileBrowseButton, liveFilePanelGBC);

		//set up the contents for the branch file path. 
		branchFilePanel.setLayout(new GridBagLayout());
		GridBagConstraints branchFilePanelGBC = new GridBagConstraints();

		branchFilePanelGBC.fill = GridBagConstraints.NONE;
		branchFilePanelGBC.gridx = 0;
		branchFilePanelGBC.gridy = 0;
		branchFilePanelGBC.anchor = GridBagConstraints.LINE_START;
		branchFilePanelGBC.insets = new Insets(0,2,2,0);
		branchFilePanel.add(branchFileLabel, branchFilePanelGBC);

		branchFilePanelGBC.fill = GridBagConstraints.HORIZONTAL;
		branchFilePanelGBC.gridx = 1;
		branchFilePanelGBC.gridy = 0;
		branchFilePanelGBC.anchor = GridBagConstraints.CENTER;
		branchFilePanelGBC.weightx = 1;

		branchFilePanel.add(branchFileTextField, branchFilePanelGBC);

		branchFilePanelGBC.fill = GridBagConstraints.NONE;
		branchFilePanelGBC.gridx = 2;
		branchFilePanelGBC.gridy = 0;
		branchFilePanelGBC.anchor = GridBagConstraints.LINE_END;
		branchFilePanelGBC.weightx = 0;

		branchFilePanel.add(branchFileBrowseButton, branchFilePanelGBC);

		//set up the contents for the merged file path. 
		mergedFilePanel.setLayout(new GridBagLayout());
		GridBagConstraints mergedFilePanelGBC = new GridBagConstraints();

		mergedFilePanelGBC.fill = GridBagConstraints.NONE;
		mergedFilePanelGBC.gridx = 0;
		mergedFilePanelGBC.gridy = 0;
		mergedFilePanelGBC.anchor = GridBagConstraints.LINE_START;
		mergedFilePanelGBC.insets = new Insets(0,2,2,0);
		mergedFilePanel.add(mergedFileLabel, mergedFilePanelGBC);

		mergedFilePanelGBC.fill = GridBagConstraints.HORIZONTAL;
		mergedFilePanelGBC.gridx = 1;
		mergedFilePanelGBC.gridy = 0;
		mergedFilePanelGBC.anchor = GridBagConstraints.CENTER;
		mergedFilePanelGBC.weightx = 1;

		mergedFilePanel.add(mergedFileTextField, mergedFilePanelGBC);

		mergedFilePanelGBC.fill = GridBagConstraints.NONE;
		mergedFilePanelGBC.gridx = 2;
		mergedFilePanelGBC.gridy = 0;
		mergedFilePanelGBC.anchor = GridBagConstraints.LINE_END;
		mergedFilePanelGBC.weightx = 0;

		mergedFilePanel.add(mergedFileBrowseButton, mergedFilePanelGBC);
	
		saveProfilePanel.setLayout(new GridBagLayout());
		GridBagConstraints saveProfilePanelGBC = new GridBagConstraints();

		saveProfilePanelGBC.fill = GridBagConstraints.NONE;
		saveProfilePanelGBC.gridx = 0;
		saveProfilePanelGBC.gridy = 0;
		saveProfilePanelGBC.anchor = GridBagConstraints.LINE_START;
		saveProfilePanelGBC.insets = new Insets(5,5,5,5);
		saveProfilePanel.add(saveProfileLabel, saveProfilePanelGBC);

		saveProfilePanelGBC.fill = GridBagConstraints.HORIZONTAL;
		saveProfilePanelGBC.gridx = 1;
		saveProfilePanelGBC.gridy = 0;
		saveProfilePanelGBC.anchor = GridBagConstraints.CENTER;
		saveProfilePanelGBC.weightx = 1;

		saveProfilePanel.add(saveProfileComboBox, saveProfilePanelGBC);
		saveProfileComboBox.setEditable(true);

		saveProfilePanelGBC.fill = GridBagConstraints.NONE;
		saveProfilePanelGBC.gridx = 1;
		saveProfilePanelGBC.gridy = 0;
//		saveProfilePanelGBC.anchor = GridBagConstraints.CENTER;
		saveProfilePanelGBC.weightx = 1;

		saveProfilePanel.add(addProfilePlusButton, saveProfilePanelGBC);

		saveProfilePanelGBC.fill = GridBagConstraints.NONE;
		saveProfilePanelGBC.gridx = 1;
		saveProfilePanelGBC.gridy = 0;
		saveProfilePanelGBC.anchor = GridBagConstraints.LINE_END;
		saveProfilePanelGBC.weightx = 1;

		saveProfilePanel.add(removeProfileMinusButton, saveProfilePanelGBC);


		mergeOptionPanel.setLayout(new GridBagLayout());
		GridBagConstraints mergeOptionPanelGBC = new GridBagConstraints();

		mergeOptionPanelGBC.fill = GridBagConstraints.NONE;
		mergeOptionPanelGBC.gridx = 0;
		mergeOptionPanelGBC.gridy = 0;
		mergeOptionPanelGBC.ipadx = 5;
		mergeOptionPanelGBC.ipady = 5;
		mergeOptionPanelGBC.insets = new Insets(5,5,5,5);
//		mergeOptionPanelGBC.anchor = GridBagConstraints.CENTER;
//		mergeOptionPanelGBC.weightx = 1;
		mergeOptionPanel.add(updateIDsLabel, mergeOptionPanelGBC);

		mergeOptionPanelGBC.gridx = 1;
		mergeOptionPanelGBC.gridy = 0;
		mergeOptionPanel.add(updateIDsCombobox, mergeOptionPanelGBC);

		mergeOptionPanelGBC.gridx = 2;
		mergeOptionPanelGBC.gridy = 0;
		mergeOptionPanel.add(mergedFileFormatLabel, mergeOptionPanelGBC);

		mergeOptionPanelGBC.gridx = 3;
		mergeOptionPanelGBC.gridy = 0;
		mergeOptionPanel.add(mergedFileFormatCombobox, mergeOptionPanelGBC);

		mergeOptionPanelGBC.gridx = 0;
		mergeOptionPanelGBC.gridy = 1;
		mergeOptionPanel.add(failOnClashLabel, mergeOptionPanelGBC);

		mergeOptionPanelGBC.gridx = 1;
		mergeOptionPanelGBC.gridy = 1;
		mergeOptionPanel.add(failOnClashCombobox, mergeOptionPanelGBC);

		mergeOptionPanelGBC.gridx = 2;
		mergeOptionPanelGBC.gridy = 1;
		mergeOptionPanel.add(ignoreClashOnIDsLabel, mergeOptionPanelGBC);

		mergeOptionPanelGBC.gridx = 3;
		mergeOptionPanelGBC.gridy = 1;
		mergeOptionPanel.add(ignoreClashOnIDsTextArea, mergeOptionPanelGBC);
	
		finalOptionPanel.setLayout(new GridBagLayout());
		GridBagConstraints finalOptionPanelGBC = new GridBagConstraints();

		finalOptionPanelGBC.fill = GridBagConstraints.HORIZONTAL;
		finalOptionPanelGBC.gridx = 0;
		finalOptionPanelGBC.gridy = 0;
	//	finalOptionPanelGBC.anchor = GridBagConstraints.FIRST_LINE_START;
		finalOptionPanelGBC.weightx = 1;
//		finalOptionPanelGBC.insets = new Insets(5,5,5,5);
		finalOptionPanel.add(advancedButton, finalOptionPanelGBC);

		finalOptionPanelGBC.fill = GridBagConstraints.HORIZONTAL;
		finalOptionPanelGBC.gridx = 0;
		finalOptionPanelGBC.gridy = 1;
		finalOptionPanel.add(showProgressPanel, finalOptionPanelGBC);
		showProgressPanel.setBorder(new TitledBorder("Show Progress"));
		
		finalOptionPanelGBC.fill = GridBagConstraints.HORIZONTAL;
		finalOptionPanelGBC.gridx = 0;
		finalOptionPanelGBC.gridy = 2;
		finalOptionPanel.add(saveProgressToFilePanel, finalOptionPanelGBC);
		saveProgressToFilePanel.setBorder(new TitledBorder("Save Progress"));
		
		finalOptionPanelGBC.fill = GridBagConstraints.HORIZONTAL;
		finalOptionPanelGBC.gridx = 0;
		finalOptionPanelGBC.gridy = 3;
		finalOptionPanel.add(mergeButton, finalOptionPanelGBC);

		
		
		validate();
		repaint();


//		saveFeedbackToFileBrowseButton
//		.addActionListener(new java.awt.event.ActionListener() {
//		public void actionPerformed(java.awt.event.ActionEvent evt) {
//		saveFeedbackToFileBrowseButtonActionPerformed(evt);
//		}
//		});

		mergedFileFormatCombobox
		.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				mergedFileFormatComboboxActionPerformed(evt);
			}

			private void mergedFileFormatComboboxActionPerformed(
					ActionEvent evt) {
				// TODO Auto-generated method stub

			}
		});

		updateIDsCombobox
		.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				updateIDsComboboxActionPerformed(evt);
			}
		});

		failOnClashCombobox
		.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				failOnClashChoiceComboBoxActionPerformed(evt);
			}
		});


		parentFileBrowseButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				parentFileButtonActionPerformed(evt);
			}
		});

		mergedFileTextField
		.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				mergedFileTextFieldActionPerformed(evt);
			}
		});

		branchFileBrowseButton
		.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				branchFileBrowseButtonActionPerformed(evt);
			}
		});

		liveFileBrowseButton
		.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				liveFileBrowseButtonActionPerformed(evt);
			}
		});

		mergedFileBrowseButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				mergedFileBrowseButtonActionPerformed(evt);
			}
		});

		liveFileTextField
		.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				liveFileTextFieldActionPerformed(evt);
			}
		});


//		activateAdvancedOptionButton
//		.addActionListener(new java.awt.event.ActionListener() {
//		public void actionPerformed(java.awt.event.ActionEvent evt) {
//		activateAdvancedOptionButtonActionPerformed(evt);
//		}
//		});


//		protected void activateAdvancedOptionButtonActionPerformed(ActionEvent evt) {
		//
//		boolean saveProfilePanelVisibility = saveProfilePanel.isVisible();
//		saveProfilePanel.setVisible(!saveProfilePanelVisibility);
		//
//		boolean mergeOptionPanelVisibility = mergeOptionPanel.isVisible();
//		mergeOptionPanel.setVisible(!mergeOptionPanelVisibility);
		//
//		if (saveProfilePanelVisibility) {
//		activateAdvancedOptionButton.setText("Advanced");
//		} else {
//		activateAdvancedOptionButton.setText("Basic");
		//
//		}

	}

	public void save(){
		System.out.println("all going well so far.");
	}

	private void branchFileBrowseButtonActionPerformed(
			java.awt.event.ActionEvent evt) {
		int showOpenDialogReturnValue = fileChooser.showOpenDialog(null);
		if (showOpenDialogReturnValue == JFileChooser.APPROVE_OPTION) {
			File SecondaryEditedChosenFile = fileChooser.getSelectedFile();
			branchFileTextField.setText(SecondaryEditedChosenFile
					.getAbsolutePath());
			branchFileTextFieldString = branchFileTextField
			.getText();

			System.out.println("arg = " + branchFileTextFieldString);

		}

	}

	private void failOnClashChoiceComboBoxActionPerformed(
			java.awt.event.ActionEvent evt) {
		failOnClashChoiceString = (String) failOnClashCombobox
		.getSelectedItem();
		System.out.println("arg = " + failOnClashChoiceString);

	};

	private int getDefaultCloseOperation() {
		// TODO Auto-generated method stub
		return defaultCloseOperation;

	}
	private void liveFileTextFieldActionPerformed(
			java.awt.event.ActionEvent evt) {
		// TODO add your handling code here:
	}

	private Boolean makeArgArrayList() {
		// TODO Auto-generated method stub
		/*
		 * This class takes the return strings from all the
		 *  GUI controls and puts them
		 * into the array to be fed to obomerge.
		 */
		System.out.println("Arguments applied are:");
		//obomergeArgsArrayList
		//		if (failOnClashActivatedCheckBox.isSelected()) {
		//			obomergeArgsArrayList.add("-fail-on-clash");
		//			obomergeArgsArrayList.add(failOnClashChoiceString);
		//			System.out.println("    -fail-on-clash " + failOnClashChoiceString);
		//
		//		}
		//		if (updateIDsActivatedCheckBox.isSelected()) {
		//			obomergeArgsArrayList.add("-update-ids");
		//			obomergeArgsArrayList.add(updateIDsChoiceString);
		//			System.out.println("    -update-ids " + updateIDsChoiceString);
		//		}
		//		//		if (ignoreClashOnIDsActivatedCheckBox.isSelected()) {
		//			obomergeArgsArrayList.add("-ignore-clash-on-id");
		//			obomergeArgsArrayList.add(ignoreClashOnIDsChoiceString);
		//			System.out.println("    -ignore-clash-on-id "
		//					+ ignoreClashOnIDsChoiceString);
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
		System.out.println("    -version " + outputFormatChoiceString);

		obomergeArgsArrayList.add("-original");
		obomergeArgsArrayList.add(parentFileTextFieldString);
		System.out.println("    -original " + parentFileTextFieldString);

		obomergeArgsArrayList.add("-revision");
		obomergeArgsArrayList.add(liveFileTextFieldString);
		System.out.println("    -revision " + liveFileTextFieldString);

		obomergeArgsArrayList.add("-revision");
		obomergeArgsArrayList.add(branchFileTextFieldString);
		System.out.println("    -revision "
				+ branchFileTextFieldString);

		obomergeArgsArrayList.add("-o");
		obomergeArgsArrayList.add(mergedFileTextFieldString);
		System.out.println("    -o " + mergedFileTextFieldString);

		obomergeArgsArrayList.trimToSize();
		obomergeArgsArray = obomergeArgsArrayList.toArray(obomergeArgsArray);
		return true;
	}

	private void mergeButtonActionPerformed(java.awt.event.ActionEvent evt) {
		//		if (showProgressCheckBox.isSelected()) {
		//			PrintStream progressTextAreaPrintStream = new PrintStream(
		//					new TextAreaOutputStream(progressTextArea));
		//			System.setOut(progressTextAreaPrintStream);
		//			System.setErr(progressTextAreaPrintStream);
		//		}
//		if (saveFeedbackToFileCheckBox.isSelected()) {
//		WriteFeedbackToFile();
//		}
		if (makeArgArrayList() == true) {
			try {
				org.oboedit.launcher.OBOMerge.main(obomergeArgsArray);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (DataAdapterException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	private void mergedFileBrowseButtonActionPerformed(java.awt.event.ActionEvent evt) {
		int returnVal = fileChooser.showOpenDialog(null);
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			mergedFileTextFieldString = fileChooser.getSelectedFile()
			.getAbsolutePath();
			mergedFileTextField.setText(mergedFileTextFieldString);

			System.out.println("arg = " + mergedFileTextFieldString);
		}
	}

	private void mergedFileFormatComboboxActionPerformed(
			java.awt.event.ActionEvent evt) {
		outputFormatChoiceString = (String) mergedFileFormatCombobox.getSelectedItem();
	}
	private void mergedFileTextFieldActionPerformed(
			java.awt.event.ActionEvent evt) {
		int returnVal = fileChooser.showOpenDialog(null);
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			mergedFileTextFieldString = fileChooser.getSelectedFile()
			.getAbsolutePath();
			mergedFileTextField.setText(mergedFileTextFieldString);

			System.out.println("arg = " + mergedFileTextFieldString);
		}

	}
	private void parentFileButtonActionPerformed(java.awt.event.ActionEvent evt) {
		int returnVal = fileChooser.showOpenDialog(null);
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			parentFileTextFieldString = fileChooser.getSelectedFile()
			.getAbsolutePath();
			parentFileTextField.setText(parentFileTextFieldString);

			System.out.println("arg = " + parentFileTextFieldString);
		}

	}
	private void saveFeedbackToFileBrowseButtonActionPerformed(
			java.awt.event.ActionEvent evt) {
		int returnVal = fileChooser.showOpenDialog(null);
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			saveFeedbackToFileTextFieldString = fileChooser.getSelectedFile()
			.getAbsolutePath();
			saveFeedbackToFileTextField
			.setText(saveFeedbackToFileTextFieldString);
		}
	}



	private void updateIDsComboboxActionPerformed(
			java.awt.event.ActionEvent evt) {
		updateIDsChoiceString = (String) updateIDsCombobox
		.getSelectedItem();
		System.out.println("arg = " + updateIDsChoiceString);
	}
	private void WriteFeedbackToFile() {
		File feedbackFile = new File(saveFeedbackToFileTextFieldString);
		try {

			PrintStream feedbackFileOutputStream = new PrintStream(
					saveFeedbackToFileTextFieldString);
			ObjectOutputStream feedbackFileObjectOutputStream = new ObjectOutputStream(
					feedbackFileOutputStream);

			System.setOut(feedbackFileOutputStream);
			System.setErr(feedbackFileOutputStream);

		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}

	}


















	protected void liveFileBrowseButtonActionPerformed(ActionEvent evt) {

		int returnVal = fileChooser.showOpenDialog(null);
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			liveFileTextFieldString = fileChooser.getSelectedFile()
			.getAbsolutePath();
			liveFileTextField.setText(liveFileTextFieldString);

			System.out.println("arg = " + liveFileTextFieldString);

		}

	}
}


