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
import javax.swing.JFrame;
import javax.swing.JOptionPane;

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
		jCheckBox1 = new javax.swing.JCheckBox();
		jProgressBar2 = new javax.swing.JProgressBar();

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

		jCheckBox1.setText("jCheckBox1");
		jCheckBox1.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jCheckBox1ActionPerformed(evt);
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
										.add(
												layout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.LEADING)
														.add(
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
																																layout
																																		.createSequentialGroup()
																																		.add(
																																				47,
																																				47,
																																				47)
																																		.add(
																																				outputFileFormatLabel))
																														.add(
																																layout
																																		.createSequentialGroup()
																																		.add(
																																				layout
																																						.createParallelGroup(
																																								org.jdesktop.layout.GroupLayout.TRAILING)
																																						.add(
																																								layout
																																										.createSequentialGroup()
																																										.add(
																																												layout
																																														.createParallelGroup(
																																																org.jdesktop.layout.GroupLayout.LEADING)
																																														.add(
																																																layout
																																																		.createSequentialGroup()
																																																		.add(
																																																				updateIDsActivatedCheckBox)
																																																		.add(
																																																				44,
																																																				44,
																																																				44))
																																														.add(
																																																failOnClashActivatedCheckBox))
																																										.add(
																																												2,
																																												2,
																																												2))
																																						.add(
																																								layout
																																										.createSequentialGroup()
																																										.add(
																																												ignoreClashOnIDsActivatedCheckBox)
																																										.addPreferredGap(
																																												org.jdesktop.layout.LayoutStyle.UNRELATED)))
																																		.add(
																																				layout
																																						.createParallelGroup(
																																								org.jdesktop.layout.GroupLayout.LEADING)
																																						.add(
																																								layout
																																										.createParallelGroup(
																																												org.jdesktop.layout.GroupLayout.LEADING)
																																										.add(
																																												failOnClashChoiceComboBox,
																																												0,
																																												88,
																																												Short.MAX_VALUE)
																																										.add(
																																												ignoreClashOnIDsChoiceComboBox,
																																												0,
																																												88,
																																												Short.MAX_VALUE))
																																						.add(
																																								updateIDsChoiceComboBox,
																																								org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																																								88,
																																								org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))))
																										.addPreferredGap(
																												org.jdesktop.layout.LayoutStyle.RELATED,
																												100,
																												Short.MAX_VALUE)
																										.add(
																												fileFormatComboBox,
																												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																												org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
																										.addPreferredGap(
																												org.jdesktop.layout.LayoutStyle.RELATED,
																												159,
																												Short.MAX_VALUE)
																										.add(
																												mergeButton,
																												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																												85,
																												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
																										.add(
																												10,
																												10,
																												10))
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
																org.jdesktop.layout.GroupLayout.TRAILING,
																layout
																		.createSequentialGroup()
																		.addContainerGap()
																		.add(
																				jProgressBar2,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																				649,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
														.add(
																layout
																		.createSequentialGroup()
																		.addContainerGap()
																		.add(
																				jCheckBox1)))
										.addContainerGap()));
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
										.add(6, 6, 6)
										.add(
												layout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.TRAILING)
														.add(
																layout
																		.createSequentialGroup()
																		.add(
																				updateIDsActivatedCheckBox)
																		.add(
																				4,
																				4,
																				4)
																		.add(
																				ignoreClashOnIDsActivatedCheckBox)
																		.addPreferredGap(
																				org.jdesktop.layout.LayoutStyle.RELATED)
																		.add(
																				failOnClashActivatedCheckBox)
																		.add(
																				2,
																				2,
																				2))
														.add(
																layout
																		.createSequentialGroup()
																		.add(
																				updateIDsChoiceComboBox,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																				org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
																		.addPreferredGap(
																				org.jdesktop.layout.LayoutStyle.RELATED)))
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
																								org.jdesktop.layout.GroupLayout.LEADING,
																								false)
																						.add(
																								layout
																										.createSequentialGroup()
																										.addPreferredGap(
																												org.jdesktop.layout.LayoutStyle.RELATED)
																										.add(
																												layout
																														.createParallelGroup(
																																org.jdesktop.layout.GroupLayout.BASELINE)
																														.add(
																																fileFormatComboBox,
																																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																																org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
																														.add(
																																outputFileFormatLabel)
																														.add(
																																mergeButton,
																																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																																57,
																																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
																										.add(
																												50,
																												50,
																												50))
																						.add(
																								org.jdesktop.layout.GroupLayout.TRAILING,
																								layout
																										.createSequentialGroup()
																										.addPreferredGap(
																												org.jdesktop.layout.LayoutStyle.RELATED,
																												org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																												Short.MAX_VALUE)
																										.add(
																												jCheckBox1)
																										.add(
																												36,
																												36,
																												36)))
																		.add(
																				jProgressBar2,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																				org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
														.add(
																layout
																		.createSequentialGroup()
																		.addPreferredGap(
																				org.jdesktop.layout.LayoutStyle.RELATED)
																		.add(
																				ignoreClashOnIDsChoiceComboBox,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																				org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
																		.addPreferredGap(
																				org.jdesktop.layout.LayoutStyle.RELATED)
																		.add(
																				failOnClashChoiceComboBox,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																				org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)))
										.addContainerGap(24, Short.MAX_VALUE)));

		pack();
	}// </editor-fold>
	//GEN-END:initComponents

	private void failOnClashActivatedCheckBoxActionPerformed(
			java.awt.event.ActionEvent evt) {
		// TODO add your handling code here:
	}

	private void jCheckBox1ActionPerformed(java.awt.event.ActionEvent evt) {
		// TODO add your handling code here:
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
	private javax.swing.JCheckBox ignoreClashOnIDsActivatedCheckBox;
	private javax.swing.JComboBox ignoreClashOnIDsChoiceComboBox;
	private javax.swing.JCheckBox jCheckBox1;
	private javax.swing.JProgressBar jProgressBar2;
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

	private Boolean makeArgArrayList() {
		// TODO Auto-generated method stub
		/*
		 * This class takes the return strings from all the
		 *  GUI controls and puts them
		 * into the array to be fed to obomerge.
		 */

		//obomergeArgsArrayList
		if (failOnClashActivatedCheckBox.isSelected()) {
			obomergeArgsArrayList.add("-fail-on-clash");
			obomergeArgsArrayList.add(failOnClashChoiceString);
		}
		if (updateIDsActivatedCheckBox.isSelected()) {
			obomergeArgsArrayList.add("-update-ids");
			obomergeArgsArrayList.add(updateIDsChoiceString);
		}
		if (ignoreClashOnIDsActivatedCheckBox.isSelected()) {
			obomergeArgsArrayList.add("-ignore-clash-on-id");
			obomergeArgsArrayList.add(ignoreClashOnIDsChoiceString);
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
		return true;
	};
}