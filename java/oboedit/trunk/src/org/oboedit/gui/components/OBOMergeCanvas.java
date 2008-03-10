/*
 * OBOMergeCanvas.java
 *
 * Created on __DATE__, __TIME__
 */

package org.oboedit.gui.components;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.ArrayList;

import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JTextArea;

import org.bbop.dataadapter.DataAdapterException;

/**
 *
 * @author  __USER__
 */
public class OBOMergeCanvas extends javax.swing.JFrame {

	ArrayList<String> obomergeArgsArrayList = new ArrayList<String>();
	String[] obomergeArgsArray = new String[0];
	String parentFileTextFieldString;
	String mainEditedFileTextFieldString;
	String secondaryEditedFileTextFieldString;
	String outputFileTextFieldString;
	String ignoreClashOnIDsChoiceString = "NEVER";
	String failOnClashChoiceString = "NEVER"; //Default value.
	String updateIDsChoiceString = "NEVER"; //Default value;
	String outputFormatChoiceString = "OBO_1_2";
	JFileChooser fileChooser = new JFileChooser();
	JFrame missingPathFrame;
	String saveFeedbackToFileTextFieldString;

	/** Creates new form OBOMergeCanvas */
	public OBOMergeCanvas() {
		initComponents();
	}

	//GEN-BEGIN:initComponents
	// <editor-fold defaultstate="collapsed" desc="Generated Code">
	private void initComponents() {

		parentFileTextField = new javax.swing.JTextField();
		parentFileButton = new javax.swing.JButton();
		parentFileLabel = new javax.swing.JLabel();
		mainEditedFileLabel = new javax.swing.JLabel();
		secondaryEditedFileLabel = new javax.swing.JLabel();
		mergedFileLabel = new javax.swing.JLabel();
		mainEditedFileTextField = new javax.swing.JTextField();
		secondaryEditedFileTextField = new javax.swing.JTextField();
		outputFileTextField = new javax.swing.JTextField();
		mainEditedFileButton = new javax.swing.JButton();
		secondaryEditedFileButton = new javax.swing.JButton();
		outputFileButton = new javax.swing.JButton();
		mergeButton = new javax.swing.JButton();
		outputFileFormatLabel = new javax.swing.JLabel();
		fileFormatComboBox = new javax.swing.JComboBox();
		updateIDsActivatedCheckBox = new javax.swing.JCheckBox();
		updateIDsChoiceComboBox = new javax.swing.JComboBox();
		ignoreClashOnIDsActivatedCheckBox = new javax.swing.JCheckBox();
		ignoreClashOnIDsChoiceComboBox = new javax.swing.JComboBox();
		failOnClashActivatedCheckBox = new javax.swing.JCheckBox();
		failOnClashChoiceComboBox = new javax.swing.JComboBox();
		progressPanel = new javax.swing.JPanel();
		jScrollPane1 = new javax.swing.JScrollPane();
		progressTextArea = new javax.swing.JTextArea();
		showProgressCheckBox = new javax.swing.JCheckBox();
		saveFeedbackToFileCheckBox = new javax.swing.JCheckBox();
		saveFeedbackToFileTextField = new javax.swing.JTextField();
		saveFeedbackToFileBrowseButton = new javax.swing.JButton();
		jSeparator1 = new javax.swing.JSeparator();

		setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
		setTitle("OBO Merge");

		parentFileButton.setText("Browse");
		parentFileButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				parentFileButtonActionPerformed(evt);
			}
		});

		parentFileLabel.setText("Parent File");

		mainEditedFileLabel.setText("Main Edited File");

		secondaryEditedFileLabel.setText("Secondary Edited File");

		mergedFileLabel.setText("Merged File");

		outputFileTextField
				.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						outputFileTextFieldActionPerformed(evt);
					}
				});

		mainEditedFileButton.setText("Browse");
		mainEditedFileButton
				.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						mainEditedFileButtonActionPerformed(evt);
					}
				});

		secondaryEditedFileButton.setText("Browse");
		secondaryEditedFileButton
				.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						secondaryEditedFileButtonActionPerformed(evt);
					}
				});

		outputFileButton.setText("Browse");
		outputFileButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				outputFileButtonActionPerformed(evt);
			}
		});

		mergeButton.setText("Merge");
		mergeButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				mergeButtonActionPerformed(evt);
			}
		});

		outputFileFormatLabel.setText("Output File Format");

		fileFormatComboBox.setModel(new javax.swing.DefaultComboBoxModel(
				new String[] { "OBO_1_2", "OBO_1_0" }));
		fileFormatComboBox
				.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						fileFormatComboBoxActionPerformed(evt);
					}
				});

		updateIDsActivatedCheckBox.setText("Update IDs");

		updateIDsChoiceComboBox.setModel(new javax.swing.DefaultComboBoxModel(
				new String[] { "NEVER", "IF_LIKELY", "ALWAYS" }));
		updateIDsChoiceComboBox
				.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						updateIDsChoiceComboBoxActionPerformed(evt);
					}
				});

		ignoreClashOnIDsActivatedCheckBox.setText("Ignore Clash on IDs");

		ignoreClashOnIDsChoiceComboBox
				.setModel(new javax.swing.DefaultComboBoxModel(new String[] {
						"NEVER", "IF_LIKELY", "ALWAYS" }));
		ignoreClashOnIDsChoiceComboBox
				.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						ignoreClashOnIDsChoiceComboBoxActionPerformed(evt);
					}
				});

		failOnClashActivatedCheckBox.setText("Fail On Clash");

		failOnClashChoiceComboBox
				.setModel(new javax.swing.DefaultComboBoxModel(new String[] {
						"NEVER", "IF_LIKELY", "ALWAYS" }));
		failOnClashChoiceComboBox
				.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						failOnClashChoiceComboBoxActionPerformed(evt);
					}
				});

		progressTextArea.setColumns(20);
		progressTextArea.setRows(5);
		jScrollPane1.setViewportView(progressTextArea);

		showProgressCheckBox.setText("Show feedback");

		saveFeedbackToFileCheckBox.setText("Save feed back to file");

		saveFeedbackToFileBrowseButton.setText("Browse");
		saveFeedbackToFileBrowseButton
				.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						saveFeedbackToFileBrowseButtonActionPerformed(evt);
					}
				});

		org.jdesktop.layout.GroupLayout progressPanelLayout = new org.jdesktop.layout.GroupLayout(
				progressPanel);
		progressPanel.setLayout(progressPanelLayout);
		progressPanelLayout
				.setHorizontalGroup(progressPanelLayout
						.createParallelGroup(
								org.jdesktop.layout.GroupLayout.LEADING)
						.add(
								progressPanelLayout
										.createSequentialGroup()
										.add(
												progressPanelLayout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.LEADING)
														.add(
																progressPanelLayout
																		.createSequentialGroup()
																		.addContainerGap()
																		.add(
																				jScrollPane1,
																				org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																				637,
																				Short.MAX_VALUE))
														.add(
																progressPanelLayout
																		.createSequentialGroup()
																		.add(
																				showProgressCheckBox)
																		.add(
																				32,
																				32,
																				32)
																		.add(
																				saveFeedbackToFileCheckBox)
																		.add(
																				18,
																				18,
																				18)
																		.add(
																				saveFeedbackToFileTextField,
																				org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																				296,
																				Short.MAX_VALUE)
																		.addPreferredGap(
																				org.jdesktop.layout.LayoutStyle.RELATED)
																		.add(
																				saveFeedbackToFileBrowseButton)))
										.addContainerGap()));
		progressPanelLayout
				.setVerticalGroup(progressPanelLayout
						.createParallelGroup(
								org.jdesktop.layout.GroupLayout.LEADING)
						.add(
								org.jdesktop.layout.GroupLayout.TRAILING,
								progressPanelLayout
										.createSequentialGroup()
										.addContainerGap()
										.add(
												progressPanelLayout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.BASELINE)
														.add(
																showProgressCheckBox)
														.add(
																saveFeedbackToFileCheckBox)
														.add(
																saveFeedbackToFileBrowseButton)
														.add(
																saveFeedbackToFileTextField,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
										.addPreferredGap(
												org.jdesktop.layout.LayoutStyle.UNRELATED)
										.add(
												jScrollPane1,
												org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
												100, Short.MAX_VALUE)));

		org.jdesktop.layout.GroupLayout layout = new org.jdesktop.layout.GroupLayout(
				getContentPane());
		getContentPane().setLayout(layout);
		layout
				.setHorizontalGroup(layout
						.createParallelGroup(
								org.jdesktop.layout.GroupLayout.LEADING)
						.add(
								layout
										.createSequentialGroup()
										.add(
												layout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.LEADING)
														.add(
																org.jdesktop.layout.GroupLayout.TRAILING,
																layout
																		.createSequentialGroup()
																		.add(
																				28,
																				28,
																				28)
																		.add(
																				layout
																						.createParallelGroup(
																								org.jdesktop.layout.GroupLayout.LEADING)
																						.add(
																								layout
																										.createSequentialGroup()
																										.add(
																												layout
																														.createParallelGroup(
																																org.jdesktop.layout.GroupLayout.LEADING)
																														.add(
																																updateIDsActivatedCheckBox)
																														.add(
																																failOnClashActivatedCheckBox)
																														.add(
																																ignoreClashOnIDsActivatedCheckBox))
																										.add(
																												17,
																												17,
																												17)
																										.add(
																												layout
																														.createParallelGroup(
																																org.jdesktop.layout.GroupLayout.TRAILING)
																														.add(
																																org.jdesktop.layout.GroupLayout.LEADING,
																																layout
																																		.createSequentialGroup()
																																		.add(
																																				layout
																																						.createParallelGroup(
																																								org.jdesktop.layout.GroupLayout.LEADING,
																																								false)
																																						.add(
																																								updateIDsChoiceComboBox,
																																								0,
																																								org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																																								Short.MAX_VALUE)
																																						.add(
																																								ignoreClashOnIDsChoiceComboBox,
																																								0,
																																								137,
																																								Short.MAX_VALUE))
																																		.addPreferredGap(
																																				org.jdesktop.layout.LayoutStyle.RELATED,
																																				50,
																																				Short.MAX_VALUE)
																																		.add(
																																				layout
																																						.createParallelGroup(
																																								org.jdesktop.layout.GroupLayout.LEADING,
																																								false)
																																						.add(
																																								fileFormatComboBox,
																																								0,
																																								org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																																								Short.MAX_VALUE)
																																						.add(
																																								outputFileFormatLabel,
																																								org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																																								org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																																								Short.MAX_VALUE)))
																														.add(
																																org.jdesktop.layout.GroupLayout.LEADING,
																																failOnClashChoiceComboBox,
																																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																																137,
																																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
																										.add(
																												26,
																												26,
																												26)
																										.add(
																												mergeButton,
																												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																												190,
																												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
																										.add(
																												8,
																												8,
																												8))
																						.add(
																								org.jdesktop.layout.GroupLayout.TRAILING,
																								layout
																										.createSequentialGroup()
																										.add(
																												layout
																														.createParallelGroup(
																																org.jdesktop.layout.GroupLayout.LEADING)
																														.add(
																																parentFileLabel)
																														.add(
																																mainEditedFileLabel)
																														.add(
																																secondaryEditedFileLabel)
																														.add(
																																mergedFileLabel))
																										.addPreferredGap(
																												org.jdesktop.layout.LayoutStyle.RELATED)
																										.add(
																												layout
																														.createParallelGroup(
																																org.jdesktop.layout.GroupLayout.LEADING)
																														.add(
																																org.jdesktop.layout.GroupLayout.TRAILING,
																																parentFileTextField,
																																org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																																459,
																																Short.MAX_VALUE)
																														.add(
																																org.jdesktop.layout.GroupLayout.TRAILING,
																																mainEditedFileTextField,
																																org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																																459,
																																Short.MAX_VALUE)
																														.add(
																																org.jdesktop.layout.GroupLayout.TRAILING,
																																secondaryEditedFileTextField,
																																org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																																459,
																																Short.MAX_VALUE)
																														.add(
																																org.jdesktop.layout.GroupLayout.TRAILING,
																																outputFileTextField,
																																org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																																459,
																																Short.MAX_VALUE))
																										.addPreferredGap(
																												org.jdesktop.layout.LayoutStyle.RELATED)
																										.add(
																												layout
																														.createParallelGroup(
																																org.jdesktop.layout.GroupLayout.LEADING)
																														.add(
																																secondaryEditedFileButton)
																														.add(
																																mainEditedFileButton)
																														.add(
																																parentFileButton)
																														.add(
																																outputFileButton)))))
														.add(
																layout
																		.createSequentialGroup()
																		.addContainerGap()
																		.add(
																				progressPanel,
																				org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																				org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																				Short.MAX_VALUE)))
										.addContainerGap())
						.add(
								org.jdesktop.layout.GroupLayout.TRAILING,
								layout
										.createSequentialGroup()
										.addContainerGap()
										.add(
												jSeparator1,
												org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
												667, Short.MAX_VALUE)));
		layout
				.setVerticalGroup(layout
						.createParallelGroup(
								org.jdesktop.layout.GroupLayout.LEADING)
						.add(
								layout
										.createSequentialGroup()
										.addContainerGap()
										.add(
												layout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.BASELINE)
														.add(parentFileButton)
														.add(parentFileLabel)
														.add(
																parentFileTextField,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
										.addPreferredGap(
												org.jdesktop.layout.LayoutStyle.RELATED)
										.add(
												layout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.BASELINE)
														.add(
																mainEditedFileLabel)
														.add(
																mainEditedFileButton)
														.add(
																mainEditedFileTextField,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
										.addPreferredGap(
												org.jdesktop.layout.LayoutStyle.RELATED)
										.add(
												layout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.BASELINE)
														.add(
																secondaryEditedFileLabel)
														.add(
																secondaryEditedFileButton)
														.add(
																secondaryEditedFileTextField,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
										.addPreferredGap(
												org.jdesktop.layout.LayoutStyle.RELATED)
										.add(
												layout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.BASELINE)
														.add(mergedFileLabel)
														.add(
																outputFileTextField,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
														.add(outputFileButton))
										.add(
												layout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.LEADING)
														.add(
																layout
																		.createSequentialGroup()
																		.add(
																				6,
																				6,
																				6)
																		.add(
																				layout
																						.createParallelGroup(
																								org.jdesktop.layout.GroupLayout.BASELINE)
																						.add(
																								updateIDsActivatedCheckBox)
																						.add(
																								outputFileFormatLabel)
																						.add(
																								updateIDsChoiceComboBox,
																								org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																								org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																								org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
																		.add(
																				4,
																				4,
																				4)
																		.add(
																				layout
																						.createParallelGroup(
																								org.jdesktop.layout.GroupLayout.BASELINE)
																						.add(
																								ignoreClashOnIDsActivatedCheckBox)
																						.add(
																								fileFormatComboBox,
																								org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																								org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																								org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
																						.add(
																								ignoreClashOnIDsChoiceComboBox,
																								org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																								org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																								org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
																		.addPreferredGap(
																				org.jdesktop.layout.LayoutStyle.RELATED)
																		.add(
																				layout
																						.createParallelGroup(
																								org.jdesktop.layout.GroupLayout.BASELINE)
																						.add(
																								failOnClashActivatedCheckBox)
																						.add(
																								failOnClashChoiceComboBox,
																								org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																								org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																								org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)))
														.add(
																layout
																		.createSequentialGroup()
																		.add(
																				25,
																				25,
																				25)
																		.add(
																				mergeButton,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																				57,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)))
										.add(18, 18, 18)
										.add(
												jSeparator1,
												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
												10,
												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
										.add(15, 15, 15)
										.add(
												progressPanel,
												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
												org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
										.addContainerGap()));

		pack();
	}// </editor-fold>
	//GEN-END:initComponents

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

	/*
	 * ActionPerformed methods for the file path mechanism are below.
	 * If the path is pasted straight into the box rather then 
	 * using the browse buttons then that also 
	 * works because the doMerge method takes account of that. 
	 */

	private void outputFileTextFieldActionPerformed(
			java.awt.event.ActionEvent evt) {
		int returnVal = fileChooser.showOpenDialog(null);
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			outputFileTextFieldString = fileChooser.getSelectedFile()
					.getAbsolutePath();
			outputFileTextField.setText(outputFileTextFieldString);

			System.out.println("arg = " + outputFileTextFieldString);
		}

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

	private void failOnClashChoiceComboBoxActionPerformed(
			java.awt.event.ActionEvent evt) {
		failOnClashChoiceString = (String) failOnClashChoiceComboBox
				.getSelectedItem();
		System.out.println("arg = " + failOnClashChoiceString);

	}

	private void ignoreClashOnIDsChoiceComboBoxActionPerformed(
			java.awt.event.ActionEvent evt) {
		// TODO add your handling code here:
		if (ignoreClashOnIDsActivatedCheckBox.isSelected()) {
			ignoreClashOnIDsChoiceString = (String) ignoreClashOnIDsChoiceComboBox
					.getSelectedItem();
			System.out.println("arg = " + ignoreClashOnIDsChoiceString);
		}

	}

	private void updateIDsChoiceComboBoxActionPerformed(
			java.awt.event.ActionEvent evt) {
		updateIDsChoiceString = (String) updateIDsChoiceComboBox
				.getSelectedItem();
		System.out.println("arg = " + updateIDsChoiceString);
	}

	private void fileFormatComboBoxActionPerformed(
			java.awt.event.ActionEvent evt) {
		outputFormatChoiceString = (String) fileFormatComboBox
				.getSelectedItem();
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

	private void parentFileButtonActionPerformed(java.awt.event.ActionEvent evt) {
		int returnVal = fileChooser.showOpenDialog(null);
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			parentFileTextFieldString = fileChooser.getSelectedFile()
					.getAbsolutePath();
			parentFileTextField.setText(parentFileTextFieldString);

			System.out.println("arg = " + parentFileTextFieldString);
		}

	}

	private void mergeButtonActionPerformed(java.awt.event.ActionEvent evt) {
		if (showProgressCheckBox.isSelected()) {
			PrintStream progressTextAreaPrintStream = new PrintStream(
					new TextAreaOutputStream(progressTextArea));
			System.setOut(progressTextAreaPrintStream);
			System.setErr(progressTextAreaPrintStream);
		}
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

	public static void main(String args[]) {
		java.awt.EventQueue.invokeLater(new Runnable() {
			public void run() {
				new OBOMergeCanvas().setVisible(true);
			}
		});
	}

	//GEN-BEGIN:variables
	// Variables declaration - do not modify
	private javax.swing.JCheckBox failOnClashActivatedCheckBox;
	private javax.swing.JComboBox failOnClashChoiceComboBox;
	private javax.swing.JComboBox fileFormatComboBox;
	private javax.swing.JCheckBox ignoreClashOnIDsActivatedCheckBox;
	private javax.swing.JComboBox ignoreClashOnIDsChoiceComboBox;
	private javax.swing.JScrollPane jScrollPane1;
	private javax.swing.JSeparator jSeparator1;
	private javax.swing.JButton mainEditedFileButton;
	private javax.swing.JLabel mainEditedFileLabel;
	private javax.swing.JTextField mainEditedFileTextField;
	private javax.swing.JButton mergeButton;
	private javax.swing.JLabel mergedFileLabel;
	private javax.swing.JButton outputFileButton;
	private javax.swing.JLabel outputFileFormatLabel;
	private javax.swing.JTextField outputFileTextField;
	private javax.swing.JButton parentFileButton;
	private javax.swing.JLabel parentFileLabel;
	private javax.swing.JTextField parentFileTextField;
	private javax.swing.JPanel progressPanel;
	private javax.swing.JTextArea progressTextArea;
	private javax.swing.JButton saveFeedbackToFileBrowseButton;
	private javax.swing.JCheckBox saveFeedbackToFileCheckBox;
	private javax.swing.JTextField saveFeedbackToFileTextField;
	private javax.swing.JButton secondaryEditedFileButton;
	private javax.swing.JLabel secondaryEditedFileLabel;
	private javax.swing.JTextField secondaryEditedFileTextField;
	private javax.swing.JCheckBox showProgressCheckBox;
	private javax.swing.JCheckBox updateIDsActivatedCheckBox;
	private javax.swing.JComboBox updateIDsChoiceComboBox;

	// End of variables declaration//GEN-END:variables

	private Boolean makeArgArrayList() {
		// TODO Auto-generated method stub
		/*
		 * This class takes the return strings from all the
		 *  GUI controls and puts them
		 * into the array to be fed to obomerge.
		 */
		System.out.println("Arguments applied are:");
		//obomergeArgsArrayList
		if (failOnClashActivatedCheckBox.isSelected()) {
			obomergeArgsArrayList.add("-fail-on-clash");
			obomergeArgsArrayList.add(failOnClashChoiceString);
			System.out.println("    -fail-on-clash " + failOnClashChoiceString);

		}
		if (updateIDsActivatedCheckBox.isSelected()) {
			obomergeArgsArrayList.add("-update-ids");
			obomergeArgsArrayList.add(updateIDsChoiceString);
			System.out.println("    -update-ids " + updateIDsChoiceString);
		}
		if (ignoreClashOnIDsActivatedCheckBox.isSelected()) {
			obomergeArgsArrayList.add("-ignore-clash-on-id");
			obomergeArgsArrayList.add(ignoreClashOnIDsChoiceString);
			System.out.println("    -ignore-clash-on-id "
					+ ignoreClashOnIDsChoiceString);
		}

		parentFileTextFieldString = parentFileTextField.getText();
		mainEditedFileTextFieldString = mainEditedFileTextField.getText();
		secondaryEditedFileTextFieldString = secondaryEditedFileTextField
				.getText();
		outputFileTextFieldString = outputFileTextField.getText();

		if (parentFileTextFieldString.length() == 0
				|| mainEditedFileTextFieldString.length() == 0
				|| secondaryEditedFileTextFieldString.length() == 0
				|| outputFileTextFieldString.length() == 0) {

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
	};

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