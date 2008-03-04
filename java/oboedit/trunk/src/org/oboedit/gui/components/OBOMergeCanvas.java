/*
 * OBOMergeCanvas.java
 *
 * Created on __DATE__, __TIME__
 */

package org.oboedit.gui.components;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import javax.swing.JFileChooser;

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
		ignoreClashOnIDsCheckBox = new javax.swing.JCheckBox();
		ignoreClashOnIDsChoiceComboBox = new javax.swing.JComboBox();
		failOnClashActivatedCheckBox = new javax.swing.JCheckBox();
		failOnClashChoiceComboBox = new javax.swing.JComboBox();

		setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);

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
		updateIDsActivatedCheckBox
				.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						updateIDsActivatedCheckBoxActionPerformed(evt);
					}
				});

		updateIDsChoiceComboBox.setModel(new javax.swing.DefaultComboBoxModel(
				new String[] { "NEVER", "IF_LIKELY", "ALWAYS" }));
		updateIDsChoiceComboBox
				.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						updateIDsChoiceComboBoxActionPerformed(evt);
					}
				});

		ignoreClashOnIDsCheckBox.setText("Ignore Clash on IDs");
		ignoreClashOnIDsCheckBox
				.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						ignoreClashOnIDsCheckBoxActionPerformed(evt);
					}
				});

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
		failOnClashActivatedCheckBox
				.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						failOnClashActivatedCheckBoxActionPerformed(evt);
					}
				});

		failOnClashChoiceComboBox
				.setModel(new javax.swing.DefaultComboBoxModel(new String[] {
						"NEVER", "IF_LIKELY", "ALWAYS" }));
		failOnClashChoiceComboBox
				.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						failOnClashChoiceComboBoxActionPerformed(evt);
					}
				});

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
										.add(39, 39, 39)
										.add(
												layout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.LEADING)
														.add(
																layout
																		.createSequentialGroup()
																		.add(
																				outputFileFormatLabel)
																		.add(
																				18,
																				18,
																				18)
																		.add(
																				fileFormatComboBox,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																				org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
														.add(
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
																								438,
																								Short.MAX_VALUE)
																						.add(
																								org.jdesktop.layout.GroupLayout.TRAILING,
																								mainEditedFileTextField,
																								org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																								438,
																								Short.MAX_VALUE)
																						.add(
																								org.jdesktop.layout.GroupLayout.TRAILING,
																								secondaryEditedFileTextField,
																								org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																								438,
																								Short.MAX_VALUE)
																						.add(
																								org.jdesktop.layout.GroupLayout.TRAILING,
																								outputFileTextField,
																								org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																								438,
																								Short.MAX_VALUE))
																		.addPreferredGap(
																				org.jdesktop.layout.LayoutStyle.RELATED)
																		.add(
																				layout
																						.createParallelGroup(
																								org.jdesktop.layout.GroupLayout.LEADING)
																						.add(
																								outputFileButton)
																						.add(
																								secondaryEditedFileButton)
																						.add(
																								mainEditedFileButton)
																						.add(
																								parentFileButton))
																		.add(
																				1,
																				1,
																				1))
														.add(
																layout
																		.createSequentialGroup()
																		.add(
																				layout
																						.createParallelGroup(
																								org.jdesktop.layout.GroupLayout.TRAILING,
																								false)
																						.add(
																								layout
																										.createSequentialGroup()
																										.add(
																												ignoreClashOnIDsCheckBox)
																										.add(
																												18,
																												18,
																												18)
																										.add(
																												ignoreClashOnIDsChoiceComboBox,
																												0,
																												org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																												Short.MAX_VALUE))
																						.add(
																								org.jdesktop.layout.GroupLayout.LEADING,
																								layout
																										.createSequentialGroup()
																										.add(
																												updateIDsActivatedCheckBox)
																										.add(
																												18,
																												18,
																												18)
																										.add(
																												updateIDsChoiceComboBox,
																												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																												107,
																												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)))
																		.add(
																				18,
																				18,
																				18)
																		.add(
																				failOnClashActivatedCheckBox)
																		.addPreferredGap(
																				org.jdesktop.layout.LayoutStyle.RELATED)
																		.add(
																				failOnClashChoiceComboBox,
																				0,
																				108,
																				Short.MAX_VALUE)
																		.add(
																				130,
																				130,
																				130)
																		.add(
																				mergeButton)))
										.add(15, 15, 15)));
		layout
				.setVerticalGroup(layout
						.createParallelGroup(
								org.jdesktop.layout.GroupLayout.LEADING)
						.add(
								layout
										.createSequentialGroup()
										.add(24, 24, 24)
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
														.add(outputFileButton)
														.add(
																outputFileTextField,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
										.add(41, 41, 41)
										.add(
												layout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.BASELINE)
														.add(
																outputFileFormatLabel)
														.add(
																fileFormatComboBox,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
										.add(18, 18, 18)
										.add(
												layout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.BASELINE)
														.add(
																updateIDsActivatedCheckBox)
														.add(
																updateIDsChoiceComboBox,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
										.add(
												layout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.LEADING)
														.add(
																layout
																		.createSequentialGroup()
																		.addPreferredGap(
																				org.jdesktop.layout.LayoutStyle.RELATED,
																				14,
																				Short.MAX_VALUE)
																		.add(
																				mergeButton)
																		.add(
																				25,
																				25,
																				25))
														.add(
																layout
																		.createSequentialGroup()
																		.addPreferredGap(
																				org.jdesktop.layout.LayoutStyle.UNRELATED)
																		.add(
																				layout
																						.createParallelGroup(
																								org.jdesktop.layout.GroupLayout.BASELINE)
																						.add(
																								ignoreClashOnIDsCheckBox)
																						.add(
																								ignoreClashOnIDsChoiceComboBox,
																								org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																								org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																								org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
																						.add(
																								failOnClashActivatedCheckBox)
																						.add(
																								failOnClashChoiceComboBox,
																								org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																								org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																								org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
																		.addContainerGap()))));

		pack();
	}// </editor-fold>
	//GEN-END:initComponents

	JFileChooser fileChooser = new JFileChooser();

	protected void mainEditedFileButtonActionPerformed(ActionEvent evt) {

		int returnVal = fileChooser.showOpenDialog(null);
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			mainEditedFileTextFieldString = fileChooser.getSelectedFile()
					.getAbsolutePath();
			mainEditedFileTextField.setText(mainEditedFileTextFieldString);

			System.out.println("arg = " + mainEditedFileTextFieldString);

		}

	}

	String failOnClashChoice = "IF_LIKELY"; //Default value.

	private void failOnClashChoiceComboBoxActionPerformed(
			java.awt.event.ActionEvent evt) {
		// TODO add your handling code here:
		if (failOnClashActivatedCheckBox.isSelected()) {
			failOnClashChoice = (String) failOnClashChoiceComboBox
					.getSelectedItem();
			System.out.println("arg = " + failOnClashChoice);
		}

	}

	String failOnClashActivated = "";

	private void failOnClashActivatedCheckBoxActionPerformed(
			java.awt.event.ActionEvent evt) {
		// TODO add your handling code here:
		if (failOnClashActivatedCheckBox.isSelected()) {
			failOnClashActivated = "-fail-on-clash";
			System.out.println("arg = " + failOnClashActivated);

		}

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

	String updateIDsChoiceString = "never"; //Default value;

	private void updateIDsChoiceComboBoxActionPerformed(
			java.awt.event.ActionEvent evt) {
		// TODO add your handling code here:
		if (updateIDsActivatedCheckBox.isSelected()) {
			updateIDsChoiceString = (String) updateIDsChoiceComboBox
					.getSelectedItem();
			System.out.println("arg = " + updateIDsChoiceString);
		}

	}

	String updateIDsActivatedString = "";

	private void updateIDsActivatedCheckBoxActionPerformed(
			java.awt.event.ActionEvent evt) {
		// TODO add your handling code here:

		if (updateIDsActivatedCheckBox.isSelected()) {
			updateIDsActivatedString = "-update-ids";
			System.out.println("arg = " + updateIDsActivatedString);

		}

	}

	String outputFormatChoiceString = "OBO_1_2";

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

		doMerge();
	}

	private void jCheckBox3ActionPerformed(java.awt.event.ActionEvent evt) {
		// TODO add your handling code here:
	}

	private void jButton2ActionPerformed(java.awt.event.ActionEvent evt) {
		// TODO add your handling code here:
	}

	private void jTextField4ActionPerformed(java.awt.event.ActionEvent evt) {
		// TODO add your handling code here:
	}

	private void jTextField3ActionPerformed(java.awt.event.ActionEvent evt) {

	}

	/**
	 * @param args the command line arguments
	 */
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
	private javax.swing.JCheckBox ignoreClashOnIDsCheckBox;
	private javax.swing.JComboBox ignoreClashOnIDsChoiceComboBox;
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
	private javax.swing.JButton secondaryEditedFileButton;
	private javax.swing.JLabel secondaryEditedFileLabel;
	private javax.swing.JTextField secondaryEditedFileTextField;
	private javax.swing.JCheckBox updateIDsActivatedCheckBox;
	private javax.swing.JComboBox updateIDsChoiceComboBox;

	// End of variables declaration//GEN-END:variables

	private void doMerge() {
		makeArgArrayList();

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

	private void makeArgArrayList() {
		// TODO Auto-generated method stub
		/*
		 * This class takes the return strings from all the
		 *  GUI controls and puts them
		 * into the array to be fed to obomerge.
		 */

		//obomergeArgsArrayList
		if (failOnClashActivatedCheckBox.isSelected()) {
			obomergeArgsArrayList.add(failOnClashActivated);
			obomergeArgsArrayList.add(failOnClashChoice);
		}
		if (updateIDsActivatedCheckBox.isSelected()) {
			obomergeArgsArrayList.add(updateIDsActivatedString);
			obomergeArgsArrayList.add(updateIDsChoiceString);
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

			throw new IllegalArgumentException("No path specified.");

		}
		obomergeArgsArrayList.add("-version");
		obomergeArgsArrayList.add(outputFormatChoiceString);
		obomergeArgsArrayList.add("-original");
		obomergeArgsArrayList.add(parentFileTextFieldString);
		obomergeArgsArrayList.add("-revision");
		obomergeArgsArrayList.add(mainEditedFileTextFieldString);
		obomergeArgsArrayList.add("-revision");
		obomergeArgsArrayList.add(secondaryEditedFileTextFieldString);
		obomergeArgsArrayList.add("-o");
		obomergeArgsArrayList.add(outputFileTextFieldString);
		obomergeArgsArrayList.trimToSize();
		System.out.println(obomergeArgsArrayList.size());
		System.out.println(obomergeArgsArrayList.toString());
		obomergeArgsArray = obomergeArgsArrayList.toArray(obomergeArgsArray);

	};

}