package org.oboedit.gui.components;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.ArrayList;

import javax.swing.BoxLayout;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JTextArea;

import org.bbop.dataadapter.DataAdapterException;
import org.bbop.framework.AbstractGUIComponent;
import org.bbop.swing.MinusIcon;
import org.bbop.swing.PlusIcon;

public class OBOMergeCanvasMark2 extends AbstractGUIComponent{







//	private static final long serialVersionUID = 1L;
//	public static void main(String args[]) {
//	java.awt.EventQueue.invokeLater(new Runnable() {
//	public void run() {

	public OBOMergeCanvasMark2(String id) {
		super(id);
		// TODO Auto-generated constructor stub
		buildGUI();
	}
	
	
	
//	}
//	});
//	}
	private javax.swing.JButton activateAdvancedOptionButton;
	private javax.swing.JComboBox failOnClashChoiceComboBox;
	private javax.swing.JLabel failOnClashLabel;
	private javax.swing.JComboBox fileFormatComboBox;
	private javax.swing.JPanel inputFilePanel;
	private javax.swing.JComboBox jComboBox1;
	private javax.swing.JLabel jLabel1;
	private javax.swing.JLabel jLabel2;
	private javax.swing.JScrollPane jScrollPane2;
	private javax.swing.JTextField jTextField1;
	private javax.swing.JButton mainEditedFileButton;
	private javax.swing.JLabel mainEditedFileLabel;
	private javax.swing.JTextField mainEditedFileTextField;
	private javax.swing.JButton mergeButton;
	private javax.swing.JPanel mergeOptionPanel;
	private javax.swing.JLabel mergedFileLabel;
	private javax.swing.JButton minusButton;
	private javax.swing.JTabbedPane oboMergeTabbedPane1;
	private javax.swing.JPanel ontologyPathPanel;
	private javax.swing.JButton outputFileButton;
	private javax.swing.JLabel outputFileFormatLabel;
	private javax.swing.JTextField outputFileTextField;
	private javax.swing.JButton parentFileButton;
	private javax.swing.JLabel parentFileLabel;
	private javax.swing.JTextField parentFileTextField;
	private javax.swing.JButton plusButton;
	private javax.swing.JPanel processFeedbackPanel;
	private javax.swing.JTextArea progressTextArea;
	private javax.swing.JButton saveFeedbackToFileBrowseButton;
	private javax.swing.JCheckBox saveFeedbackToFileCheckBox;
	private javax.swing.JTextField saveFeedbackToFileTextField;
	private javax.swing.JPanel saveProfilePanel;
	private javax.swing.JButton secondaryEditedFileButton;
	private javax.swing.JLabel secondaryEditedFileLabel;
	private javax.swing.JTextField secondaryEditedFileTextField;
	private javax.swing.JPanel showProgressPanel;
	private javax.swing.JComboBox updateIDsChoiceComboBox;
	private javax.swing.JLabel updateIDsLabel;
	private ArrayList<String> obomergeArgsArrayList = new ArrayList<String>();
	private String parentFileTextFieldString;
	private String mainEditedFileTextFieldString;
	private String secondaryEditedFileTextFieldString;
	private String outputFileTextFieldString;
	private String[] obomergeArgsArray = new String[0];
	private String ignoreClashOnIDsChoiceString = "NEVER";
	private String failOnClashChoiceString = "NEVER"; //Default value.
	private String updateIDsChoiceString = "NEVER"; //Default value;
	private String outputFormatChoiceString = "OBO_1_2";
	private JFileChooser fileChooser = new JFileChooser();
	private JFrame missingPathFrame;
	private String saveFeedbackToFileTextFieldString;
	String id;



	private void buildGUI() {
		// TODO Auto-generated method stub
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

	}
	
	/**
	 * @author  Ranganath Kini
	 * @see      javax.swing.JTextArea
	 * http://www.jcreator.com/forums/index.php?showtopic=773
	 */
	abstract class TextAreaOutputStream extends OutputStream {
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

	private void initComponents() {

		oboMergeTabbedPane1 = new javax.swing.JTabbedPane();
		processFeedbackPanel = new javax.swing.JPanel();
		saveFeedbackToFileBrowseButton = new javax.swing.JButton();
		saveFeedbackToFileCheckBox = new javax.swing.JCheckBox();
		saveFeedbackToFileTextField = new javax.swing.JTextField();
		showProgressPanel = new javax.swing.JPanel();
		jScrollPane2 = new javax.swing.JScrollPane();
		progressTextArea = new javax.swing.JTextArea();
		inputFilePanel = new javax.swing.JPanel();
		saveProfilePanel = new javax.swing.JPanel();
		jLabel2 = new javax.swing.JLabel();
		jComboBox1 = new javax.swing.JComboBox();
		plusButton = new javax.swing.JButton(new PlusIcon(1.5f, 8, 8));
		minusButton = new javax.swing.JButton(new MinusIcon(1.5f, 8, 8));
		mergeOptionPanel = new javax.swing.JPanel();
		outputFileFormatLabel = new javax.swing.JLabel();
		fileFormatComboBox = new javax.swing.JComboBox();
		updateIDsChoiceComboBox = new javax.swing.JComboBox();
		failOnClashChoiceComboBox = new javax.swing.JComboBox();
		jTextField1 = new javax.swing.JTextField();
		updateIDsLabel = new javax.swing.JLabel();
		failOnClashLabel = new javax.swing.JLabel();
		jLabel1 = new javax.swing.JLabel();
		ontologyPathPanel = new javax.swing.JPanel();
		parentFileButton = new javax.swing.JButton();
		outputFileTextField = new javax.swing.JTextField();
		mergedFileLabel = new javax.swing.JLabel();
		mainEditedFileLabel = new javax.swing.JLabel();
		parentFileTextField = new javax.swing.JTextField();
		secondaryEditedFileButton = new javax.swing.JButton();
		parentFileLabel = new javax.swing.JLabel();
		mainEditedFileButton = new javax.swing.JButton();
		secondaryEditedFileTextField = new javax.swing.JTextField();
		outputFileButton = new javax.swing.JButton();
		mainEditedFileTextField = new javax.swing.JTextField();
		secondaryEditedFileLabel = new javax.swing.JLabel();
		activateAdvancedOptionButton = new javax.swing.JButton();
		mergeButton = new javax.swing.JButton();

//		setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
		setTitle("OBO Merge");

		saveFeedbackToFileBrowseButton.setText("Browse");
		saveFeedbackToFileBrowseButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				saveFeedbackToFileBrowseButtonActionPerformed(evt);
			}

			private void saveFeedbackToFileBrowseButtonActionPerformed(java.awt.event.ActionEvent evt) {
				int returnVal = fileChooser.showOpenDialog(null);
				if (returnVal == JFileChooser.APPROVE_OPTION) {
					saveFeedbackToFileTextFieldString = fileChooser.getSelectedFile()
					.getAbsolutePath();
					saveFeedbackToFileTextField
					.setText(saveFeedbackToFileTextFieldString);
				}
			}

		});

		saveFeedbackToFileCheckBox.setText("Save feed back to file");

		oboMergeTabbedPane1.addTab("Process Feedback", processFeedbackPanel);

		saveProfilePanel.setBorder(javax.swing.BorderFactory
				.createTitledBorder("Save Profile"));

		jLabel2.setText("Stored adapter settings");

		jComboBox1.setEditable(true);
		jComboBox1.setModel(new javax.swing.DefaultComboBoxModel(
				new String[] { "<create new profile>" }));

		plusButton.setBackground(new java.awt.Color(51, 51, 255));
		plusButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				plusButtonActionPerformed(evt);
			}

			private void addNewNamedOntologyMergePathProfile() {
				// TODO Auto-generated method stub

			}

			private void plusButtonActionPerformed(java.awt.event.ActionEvent evt) {
				addNewNamedOntologyMergePathProfile();
			}

		});

		minusButton.setBackground(new java.awt.Color(0, 0, 255));

		mergeOptionPanel.setBorder(javax.swing.BorderFactory
				.createTitledBorder("Merge Options"));

		outputFileFormatLabel.setText("Output File Format");

		fileFormatComboBox.setModel(new javax.swing.DefaultComboBoxModel(
				new String[] { "OBO_1_2", "OBO_1_0" }));
		fileFormatComboBox
		.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				fileFormatComboBoxActionPerformed(evt);
			}

			private void fileFormatComboBoxActionPerformed(
					java.awt.event.ActionEvent evt) {
				outputFormatChoiceString = (String) fileFormatComboBox
				.getSelectedItem();
			}

		});

		updateIDsChoiceComboBox.setModel(new javax.swing.DefaultComboBoxModel(
				new String[] { "", "NEVER", "IF_LIKELY", "ALWAYS" }));
		updateIDsChoiceComboBox
		.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				updateIDsChoiceComboBoxActionPerformed(evt);
			}

			private void updateIDsChoiceComboBoxActionPerformed(
					java.awt.event.ActionEvent evt) {
				updateIDsChoiceString = (String) updateIDsChoiceComboBox
				.getSelectedItem();
				System.out.println("arg = " + updateIDsChoiceString);
			}

		});

		failOnClashChoiceComboBox
		.setModel(new javax.swing.DefaultComboBoxModel(new String[] {
				"IF_LIKELY", "NEVER", "ALWAYS" }));
		failOnClashChoiceComboBox
		.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				failOnClashChoiceComboBoxActionPerformed(evt);
			}

			private void failOnClashChoiceComboBoxActionPerformed(
					java.awt.event.ActionEvent evt) {
				failOnClashChoiceString = (String) failOnClashChoiceComboBox
				.getSelectedItem();
				System.out.println("arg = " + failOnClashChoiceString);

			}

		});

		jTextField1.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jTextField1ActionPerformed(evt);
			}

			private void jTextField1ActionPerformed(java.awt.event.ActionEvent evt) {
				// TODO add your handling code here:
			}

		});

		updateIDsLabel.setText("Update IDs");

		failOnClashLabel.setText("Fail on Clash");

		jLabel1.setText("Ignore Clash on IDs");
		ontologyPathPanel.setBorder(javax.swing.BorderFactory
				.createTitledBorder("Ontology Paths"));

		parentFileButton.setText("Browse");
		parentFileButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				parentFileButtonActionPerformed(evt);
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

		}


		);

		outputFileTextField
		.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				outputFileTextFieldActionPerformed(evt);
			}
			private void outputFileTextFieldActionPerformed(java.awt.event.ActionEvent evt) {
				int returnVal = fileChooser.showOpenDialog(null);
				if (returnVal == JFileChooser.APPROVE_OPTION) {
					outputFileTextFieldString = fileChooser.getSelectedFile().getAbsolutePath();
					outputFileTextField.setText(outputFileTextFieldString);
					System.out.println("arg = " + outputFileTextFieldString);
				}

			}

		});

		mergedFileLabel.setText("Merged File");

		mainEditedFileLabel.setText("Primary Edited File");

		secondaryEditedFileButton.setText("Browse");
		secondaryEditedFileButton
		.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				secondaryEditedFileButtonActionPerformed(evt);
			}

			private void secondaryEditedFileButtonActionPerformed(
					java.awt.event.ActionEvent evt) {
				int showOpenDialogReturnValue = fileChooser.showOpenDialog(null);
				if (showOpenDialogReturnValue == JFileChooser.APPROVE_OPTION) {
					File SecondaryEditedChosenFile = fileChooser.getSelectedFile();
					secondaryEditedFileTextField.setText(SecondaryEditedChosenFile
							.getAbsolutePath());
					secondaryEditedFileTextFieldString = secondaryEditedFileTextField
					.getText();

					System.out.println("arg = " + secondaryEditedFileTextFieldString);

				}

			}

		});

		parentFileLabel.setText("Parent File");

		mainEditedFileButton.setText("Browse");
		mainEditedFileButton
		.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				mainEditedFileButtonActionPerformed(evt);
			}

			protected void mainEditedFileButtonActionPerformed(ActionEvent evt) {

				int returnVal = fileChooser.showOpenDialog(null);
				if (returnVal == JFileChooser.APPROVE_OPTION) {
					mainEditedFileTextFieldString = fileChooser.getSelectedFile()
					.getAbsolutePath();
					mainEditedFileTextField.setText(mainEditedFileTextFieldString);

					System.out.println("arg = " + mainEditedFileTextFieldString);

				}

			}

		});

		outputFileButton.setText("Browse");
		outputFileButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				outputFileButtonActionPerformed(evt);
			}
			private void outputFileButtonActionPerformed(java.awt.event.ActionEvent evt) {
				int returnVal = fileChooser.showOpenDialog(null);
				if (returnVal == JFileChooser.APPROVE_OPTION) {
					outputFileTextFieldString = fileChooser.getSelectedFile()
					.getAbsolutePath();
					outputFileTextField.setText(outputFileTextFieldString);

					System.out.println("arg = " + outputFileTextFieldString);
				}
			}

		});

		mainEditedFileTextField
		.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				mainEditedFileTextFieldActionPerformed(evt);
			}

			private void mainEditedFileTextFieldActionPerformed(
					java.awt.event.ActionEvent evt) {
				// TODO add your handling code here:
			}

		});

		secondaryEditedFileLabel.setText("Secondary Edited File");


		activateAdvancedOptionButton.setText("Advanced");
		activateAdvancedOptionButton
		.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				activateAdvancedOptionButtonActionPerformed(evt);
			}

			private void activateAdvancedOptionButtonActionPerformed(
					ActionEvent evt) {
				boolean saveProfilePanelVisibility = saveProfilePanel.isVisible();
				saveProfilePanel.setVisible(!saveProfilePanelVisibility);

				boolean mergeOptionPanelVisibility = mergeOptionPanel.isVisible();
				mergeOptionPanel.setVisible(!mergeOptionPanelVisibility);

				if (saveProfilePanelVisibility) {
					activateAdvancedOptionButton.setText("Advanced");
				} else {
					activateAdvancedOptionButton.setText("Basic");

				}

			}
		});


		oboMergeTabbedPane1.addTab("Ontology Files", inputFilePanel);

		mergeButton.setText("Merge");
		mergeButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				mergeButtonActionPerformed(evt);
			}
			private boolean makeArgArrayList() {
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
				mainEditedFileTextFieldString = mainEditedFileTextField.getText();
				secondaryEditedFileTextFieldString = secondaryEditedFileTextField
				.getText();
				outputFileTextFieldString = outputFileTextField.getText();

				if (parentFileTextFieldString.length() == 0
						|| mainEditedFileTextFieldString.length() == 0
						|| secondaryEditedFileTextFieldString.length() == 0
						|| outputFileTextFieldString.length() == 0) {

//					JOptionPane.showMessageDialog(missingPathFrame,
//					"Please fill in all of the necessary file paths.",
//					"Missing Information", getDefaultCloseOperation());

					return false;
				}
				obomergeArgsArrayList.add("-version");
				obomergeArgsArrayList.add(outputFormatChoiceString);
				System.out.println("    -version " + outputFormatChoiceString);

				obomergeArgsArrayList.add("-original");
				obomergeArgsArrayList.add(parentFileTextFieldString);
				System.out.println("    -original " + parentFileTextFieldString);

				obomergeArgsArrayList.add("-revision");
				obomergeArgsArrayList.add(mainEditedFileTextFieldString);
				System.out.println("    -revision " + mainEditedFileTextFieldString);

				obomergeArgsArrayList.add("-revision");
				obomergeArgsArrayList.add(secondaryEditedFileTextFieldString);
				System.out.println("    -revision "
						+ secondaryEditedFileTextFieldString);

				obomergeArgsArrayList.add("-o");
				obomergeArgsArrayList.add(outputFileTextFieldString);
				System.out.println("    -o " + outputFileTextFieldString);

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
				if (saveFeedbackToFileCheckBox.isSelected()) {
					WriteFeedbackToFile();
				}
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
			private void WriteFeedbackToFile() {
				// TODO Auto-generated method stub

			};

		});
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
}	
