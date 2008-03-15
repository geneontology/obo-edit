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

		oboMergeTabbedPane1 = new javax.swing.JTabbedPane();
		advancedOptionPanel = new javax.swing.JPanel();
		outputFileFormatLabel = new javax.swing.JLabel();
		fileFormatComboBox = new javax.swing.JComboBox();
		updateIDsChoiceComboBox = new javax.swing.JComboBox();
		failOnClashChoiceComboBox = new javax.swing.JComboBox();
		jTextField1 = new javax.swing.JTextField();
		updateIDsLabel = new javax.swing.JLabel();
		failOnClashLabel = new javax.swing.JLabel();
		jLabel1 = new javax.swing.JLabel();
		processFeedbackPanel = new javax.swing.JPanel();
		saveFeedbackToFileBrowseButton = new javax.swing.JButton();
		saveFeedbackToFileCheckBox = new javax.swing.JCheckBox();
		saveFeedbackToFileTextField = new javax.swing.JTextField();
		showProgressPanel = new javax.swing.JPanel();
		jScrollPane2 = new javax.swing.JScrollPane();
		progressTextArea = new javax.swing.JTextArea();
		inputFilePanel = new javax.swing.JPanel();
		jLabel2 = new javax.swing.JLabel();
		jTextField2 = new javax.swing.JTextField();
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
		mergeButton = new javax.swing.JButton();

		setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);
		setTitle("OBO Merge");

		outputFileFormatLabel.setText("Output File Format");

		fileFormatComboBox.setModel(new javax.swing.DefaultComboBoxModel(
				new String[] { "OBO_1_2", "OBO_1_0" }));
		fileFormatComboBox
				.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						fileFormatComboBoxActionPerformed(evt);
					}
				});

		updateIDsChoiceComboBox.setModel(new javax.swing.DefaultComboBoxModel(
				new String[] { "", "NEVER", "IF_LIKELY", "ALWAYS" }));
		updateIDsChoiceComboBox
				.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						updateIDsChoiceComboBoxActionPerformed(evt);
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
				});

		jTextField1.setText("ignoreClashOnIDsTextField");
		jTextField1.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jTextField1ActionPerformed(evt);
			}
		});

		updateIDsLabel.setText("Update IDs");

		failOnClashLabel.setText("Fail on Clash");

		jLabel1.setText("Ignore Clash on IDs");

		org.jdesktop.layout.GroupLayout advancedOptionPanelLayout = new org.jdesktop.layout.GroupLayout(
				advancedOptionPanel);
		advancedOptionPanel.setLayout(advancedOptionPanelLayout);
		advancedOptionPanelLayout
				.setHorizontalGroup(advancedOptionPanelLayout
						.createParallelGroup(
								org.jdesktop.layout.GroupLayout.LEADING)
						.add(
								advancedOptionPanelLayout
										.createSequentialGroup()
										.add(
												advancedOptionPanelLayout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.LEADING)
														.add(
																advancedOptionPanelLayout
																		.createSequentialGroup()
																		.add(
																				37,
																				37,
																				37)
																		.add(
																				advancedOptionPanelLayout
																						.createParallelGroup(
																								org.jdesktop.layout.GroupLayout.TRAILING)
																						.add(
																								updateIDsLabel)
																						.add(
																								failOnClashLabel)))
														.add(
																advancedOptionPanelLayout
																		.createSequentialGroup()
																		.addContainerGap()
																		.add(
																				outputFileFormatLabel,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																				98,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)))
										.addPreferredGap(
												org.jdesktop.layout.LayoutStyle.RELATED)
										.add(
												advancedOptionPanelLayout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.LEADING)
														.add(
																failOnClashChoiceComboBox,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																137,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
														.add(
																fileFormatComboBox,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
														.add(
																advancedOptionPanelLayout
																		.createSequentialGroup()
																		.add(
																				updateIDsChoiceComboBox,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																				org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
																		.add(
																				99,
																				99,
																				99)
																		.add(
																				jLabel1)))
										.addPreferredGap(
												org.jdesktop.layout.LayoutStyle.RELATED)
										.add(
												jTextField1,
												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
												162,
												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
										.addContainerGap(
												org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
												Short.MAX_VALUE)));
		advancedOptionPanelLayout
				.setVerticalGroup(advancedOptionPanelLayout
						.createParallelGroup(
								org.jdesktop.layout.GroupLayout.LEADING)
						.add(
								advancedOptionPanelLayout
										.createSequentialGroup()
										.addContainerGap()
										.add(
												advancedOptionPanelLayout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.LEADING)
														.add(
																jTextField1,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																78,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
														.add(
																advancedOptionPanelLayout
																		.createSequentialGroup()
																		.add(
																				advancedOptionPanelLayout
																						.createParallelGroup(
																								org.jdesktop.layout.GroupLayout.BASELINE)
																						.add(
																								updateIDsLabel)
																						.add(
																								updateIDsChoiceComboBox,
																								org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																								org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																								org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
																						.add(
																								jLabel1))
																		.addPreferredGap(
																				org.jdesktop.layout.LayoutStyle.RELATED)
																		.add(
																				advancedOptionPanelLayout
																						.createParallelGroup(
																								org.jdesktop.layout.GroupLayout.BASELINE)
																						.add(
																								failOnClashChoiceComboBox,
																								org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																								org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																								org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
																						.add(
																								failOnClashLabel))
																		.addPreferredGap(
																				org.jdesktop.layout.LayoutStyle.RELATED)
																		.add(
																				advancedOptionPanelLayout
																						.createParallelGroup(
																								org.jdesktop.layout.GroupLayout.BASELINE)
																						.add(
																								fileFormatComboBox,
																								org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																								org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																								org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
																						.add(
																								outputFileFormatLabel))))
										.add(324, 324, 324)));

		oboMergeTabbedPane1.addTab("Advanced Options", advancedOptionPanel);

		saveFeedbackToFileBrowseButton.setText("Browse");
		saveFeedbackToFileBrowseButton
				.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						saveFeedbackToFileBrowseButtonActionPerformed(evt);
					}
				});

		saveFeedbackToFileCheckBox.setText("Save feed back to file");

		progressTextArea.setColumns(20);
		progressTextArea.setRows(5);
		jScrollPane2.setViewportView(progressTextArea);

		org.jdesktop.layout.GroupLayout showProgressPanelLayout = new org.jdesktop.layout.GroupLayout(
				showProgressPanel);
		showProgressPanel.setLayout(showProgressPanelLayout);
		showProgressPanelLayout
				.setHorizontalGroup(showProgressPanelLayout
						.createParallelGroup(
								org.jdesktop.layout.GroupLayout.LEADING)
						.add(
								showProgressPanelLayout
										.createSequentialGroup()
										.add(
												jScrollPane2,
												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
												670,
												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
										.addContainerGap(
												org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
												Short.MAX_VALUE)));
		showProgressPanelLayout.setVerticalGroup(showProgressPanelLayout
				.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
				.add(jScrollPane2,
						org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 164,
						Short.MAX_VALUE));

		org.jdesktop.layout.GroupLayout processFeedbackPanelLayout = new org.jdesktop.layout.GroupLayout(
				processFeedbackPanel);
		processFeedbackPanel.setLayout(processFeedbackPanelLayout);
		processFeedbackPanelLayout
				.setHorizontalGroup(processFeedbackPanelLayout
						.createParallelGroup(
								org.jdesktop.layout.GroupLayout.LEADING)
						.add(
								processFeedbackPanelLayout
										.createSequentialGroup()
										.addContainerGap()
										.add(
												processFeedbackPanelLayout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.LEADING)
														.add(
																processFeedbackPanelLayout
																		.createSequentialGroup()
																		.add(
																				saveFeedbackToFileCheckBox,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																				160,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
																		.addPreferredGap(
																				org.jdesktop.layout.LayoutStyle.RELATED)
																		.add(
																				saveFeedbackToFileTextField,
																				org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																				431,
																				Short.MAX_VALUE)
																		.addPreferredGap(
																				org.jdesktop.layout.LayoutStyle.UNRELATED)
																		.add(
																				saveFeedbackToFileBrowseButton,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																				77,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
														.add(
																showProgressPanel,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																Short.MAX_VALUE))
										.add(123, 123, 123)));
		processFeedbackPanelLayout
				.setVerticalGroup(processFeedbackPanelLayout
						.createParallelGroup(
								org.jdesktop.layout.GroupLayout.LEADING)
						.add(
								processFeedbackPanelLayout
										.createSequentialGroup()
										.addContainerGap()
										.add(
												showProgressPanel,
												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
												org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
										.addPreferredGap(
												org.jdesktop.layout.LayoutStyle.RELATED,
												9, Short.MAX_VALUE)
										.add(
												processFeedbackPanelLayout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.TRAILING)
														.add(
																processFeedbackPanelLayout
																		.createParallelGroup(
																				org.jdesktop.layout.GroupLayout.BASELINE)
																		.add(
																				saveFeedbackToFileCheckBox)
																		.add(
																				saveFeedbackToFileTextField,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																				21,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
														.add(
																saveFeedbackToFileBrowseButton))
										.addContainerGap()));

		oboMergeTabbedPane1.addTab("Process Feedback", processFeedbackPanel);

		jLabel2.setText("Stored adapter settings");

		jTextField2.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				jTextField2ActionPerformed(evt);
			}
		});

		ontologyPathPanel.setBorder(javax.swing.BorderFactory
				.createTitledBorder("Ontology Paths"));

		parentFileButton.setText("Browse");
		parentFileButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				parentFileButtonActionPerformed(evt);
			}
		});

		outputFileTextField
				.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						outputFileTextFieldActionPerformed(evt);
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
				});

		parentFileLabel.setText("Parent File");

		mainEditedFileButton.setText("Browse");
		mainEditedFileButton
				.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						mainEditedFileButtonActionPerformed(evt);
					}
				});

		outputFileButton.setText("Browse");
		outputFileButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				outputFileButtonActionPerformed(evt);
			}
		});

		mainEditedFileTextField
				.addActionListener(new java.awt.event.ActionListener() {
					public void actionPerformed(java.awt.event.ActionEvent evt) {
						mainEditedFileTextFieldActionPerformed(evt);
					}
				});

		secondaryEditedFileLabel.setText("Secondary Edited File");

		org.jdesktop.layout.GroupLayout ontologyPathPanelLayout = new org.jdesktop.layout.GroupLayout(
				ontologyPathPanel);
		ontologyPathPanel.setLayout(ontologyPathPanelLayout);
		ontologyPathPanelLayout
				.setHorizontalGroup(ontologyPathPanelLayout
						.createParallelGroup(
								org.jdesktop.layout.GroupLayout.LEADING)
						.add(
								ontologyPathPanelLayout
										.createSequentialGroup()
										.addContainerGap()
										.add(
												ontologyPathPanelLayout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.LEADING)
														.add(
																ontologyPathPanelLayout
																		.createSequentialGroup()
																		.addPreferredGap(
																				org.jdesktop.layout.LayoutStyle.RELATED)
																		.add(
																				ontologyPathPanelLayout
																						.createParallelGroup(
																								org.jdesktop.layout.GroupLayout.LEADING)
																						.add(
																								ontologyPathPanelLayout
																										.createSequentialGroup()
																										.add(
																												mergedFileLabel,
																												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																												107,
																												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
																										.add(
																												50,
																												50,
																												50)
																										.add(
																												outputFileTextField,
																												org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																												466,
																												Short.MAX_VALUE)
																										.addPreferredGap(
																												org.jdesktop.layout.LayoutStyle.RELATED)
																										.add(
																												outputFileButton,
																												org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																												69,
																												Short.MAX_VALUE))
																						.add(
																								ontologyPathPanelLayout
																										.createSequentialGroup()
																										.add(
																												secondaryEditedFileLabel,
																												org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																												153,
																												Short.MAX_VALUE)
																										.addPreferredGap(
																												org.jdesktop.layout.LayoutStyle.RELATED)
																										.add(
																												secondaryEditedFileTextField,
																												org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																												466,
																												Short.MAX_VALUE)
																										.addPreferredGap(
																												org.jdesktop.layout.LayoutStyle.RELATED)
																										.add(
																												secondaryEditedFileButton,
																												org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																												69,
																												Short.MAX_VALUE)))
																		.addPreferredGap(
																				org.jdesktop.layout.LayoutStyle.RELATED))
														.add(
																org.jdesktop.layout.GroupLayout.TRAILING,
																ontologyPathPanelLayout
																		.createSequentialGroup()
																		.add(
																				mainEditedFileLabel,
																				org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																				128,
																				Short.MAX_VALUE)
																		.add(
																				29,
																				29,
																				29)
																		.add(
																				mainEditedFileTextField,
																				org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																				466,
																				Short.MAX_VALUE)
																		.addPreferredGap(
																				org.jdesktop.layout.LayoutStyle.RELATED)
																		.add(
																				mainEditedFileButton,
																				org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																				69,
																				Short.MAX_VALUE)
																		.addPreferredGap(
																				org.jdesktop.layout.LayoutStyle.RELATED))
														.add(
																ontologyPathPanelLayout
																		.createSequentialGroup()
																		.addPreferredGap(
																				org.jdesktop.layout.LayoutStyle.RELATED)
																		.add(
																				parentFileLabel,
																				org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																				153,
																				Short.MAX_VALUE)
																		.addPreferredGap(
																				org.jdesktop.layout.LayoutStyle.RELATED)
																		.add(
																				parentFileTextField,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																				466,
																				org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
																		.addPreferredGap(
																				org.jdesktop.layout.LayoutStyle.RELATED)
																		.add(
																				parentFileButton,
																				org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																				69,
																				Short.MAX_VALUE)))
										.add(0, 0, 0)));

		ontologyPathPanelLayout.linkSize(new java.awt.Component[] {
				mainEditedFileButton, outputFileButton, parentFileButton,
				secondaryEditedFileButton },
				org.jdesktop.layout.GroupLayout.HORIZONTAL);

		ontologyPathPanelLayout.linkSize(new java.awt.Component[] {
				mainEditedFileTextField, outputFileTextField,
				secondaryEditedFileTextField },
				org.jdesktop.layout.GroupLayout.HORIZONTAL);

		ontologyPathPanelLayout
				.setVerticalGroup(ontologyPathPanelLayout
						.createParallelGroup(
								org.jdesktop.layout.GroupLayout.LEADING)
						.add(
								ontologyPathPanelLayout
										.createSequentialGroup()
										.addContainerGap()
										.add(
												ontologyPathPanelLayout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.BASELINE,
																false)
														.add(parentFileButton)
														.add(
																parentFileLabel,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																23,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
														.add(
																parentFileTextField,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
										.addPreferredGap(
												org.jdesktop.layout.LayoutStyle.RELATED)
										.add(
												ontologyPathPanelLayout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.BASELINE,
																false)
														.add(
																mainEditedFileLabel,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																23,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
														.add(
																mainEditedFileTextField,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
														.add(
																mainEditedFileButton))
										.addPreferredGap(
												org.jdesktop.layout.LayoutStyle.RELATED)
										.add(
												ontologyPathPanelLayout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.BASELINE,
																false)
														.add(
																secondaryEditedFileLabel,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																23,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
														.add(
																secondaryEditedFileTextField,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
														.add(
																secondaryEditedFileButton))
										.addPreferredGap(
												org.jdesktop.layout.LayoutStyle.RELATED)
										.add(
												ontologyPathPanelLayout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.BASELINE,
																false)
														.add(
																mergedFileLabel,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																23,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
														.add(outputFileButton)
														.add(
																outputFileTextField,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
										.addContainerGap()));

		ontologyPathPanelLayout.linkSize(new java.awt.Component[] {
				mainEditedFileLabel, mergedFileLabel, parentFileButton,
				parentFileLabel, secondaryEditedFileLabel },
				org.jdesktop.layout.GroupLayout.VERTICAL);

		org.jdesktop.layout.GroupLayout inputFilePanelLayout = new org.jdesktop.layout.GroupLayout(
				inputFilePanel);
		inputFilePanel.setLayout(inputFilePanelLayout);
		inputFilePanelLayout
				.setHorizontalGroup(inputFilePanelLayout
						.createParallelGroup(
								org.jdesktop.layout.GroupLayout.LEADING)
						.add(
								inputFilePanelLayout
										.createSequentialGroup()
										.addContainerGap()
										.add(jLabel2)
										.add(49, 49, 49)
										.add(
												jTextField2,
												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
												468,
												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
										.addContainerGap(83, Short.MAX_VALUE))
						.add(ontologyPathPanel,
								org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
								org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
								Short.MAX_VALUE));
		inputFilePanelLayout
				.setVerticalGroup(inputFilePanelLayout
						.createParallelGroup(
								org.jdesktop.layout.GroupLayout.LEADING)
						.add(
								inputFilePanelLayout
										.createSequentialGroup()
										.add(24, 24, 24)
										.add(
												inputFilePanelLayout
														.createParallelGroup(
																org.jdesktop.layout.GroupLayout.BASELINE)
														.add(jLabel2)
														.add(
																jTextField2,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
																org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
																org.jdesktop.layout.GroupLayout.PREFERRED_SIZE))
										.add(13, 13, 13)
										.add(
												ontologyPathPanel,
												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE,
												org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
												org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
										.addContainerGap(
												org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
												Short.MAX_VALUE)));

		oboMergeTabbedPane1.addTab("Ontology Files", inputFilePanel);

		mergeButton.setText("Merge");
		mergeButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				mergeButtonActionPerformed(evt);
			}
		});

		org.jdesktop.layout.GroupLayout layout = new org.jdesktop.layout.GroupLayout(
				getContentPane());
		getContentPane().setLayout(layout);
		layout.setHorizontalGroup(layout.createParallelGroup(
				org.jdesktop.layout.GroupLayout.LEADING).add(
				org.jdesktop.layout.GroupLayout.TRAILING, oboMergeTabbedPane1,
				org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, 729,
				Short.MAX_VALUE).add(
				org.jdesktop.layout.GroupLayout.TRAILING,
				layout.createSequentialGroup().addContainerGap(656,
						Short.MAX_VALUE).add(mergeButton).addContainerGap()));
		layout.setVerticalGroup(layout.createParallelGroup(
				org.jdesktop.layout.GroupLayout.LEADING).add(
				layout.createSequentialGroup().add(oboMergeTabbedPane1,
						org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 246,
						org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
						.addPreferredGap(
								org.jdesktop.layout.LayoutStyle.RELATED).add(
								mergeButton).addContainerGap(
								org.jdesktop.layout.GroupLayout.DEFAULT_SIZE,
								Short.MAX_VALUE)));

		pack();
	}// </editor-fold>
	//GEN-END:initComponents

	private void jTextField2ActionPerformed(java.awt.event.ActionEvent evt) {
		// TODO add your handling code here:
	}

	private void mainEditedFileTextFieldActionPerformed(
			java.awt.event.ActionEvent evt) {
		// TODO add your handling code here:
	}

	private void jTextField1ActionPerformed(java.awt.event.ActionEvent evt) {
		// TODO add your handling code here:
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

	public static void main(String args[]) {
		java.awt.EventQueue.invokeLater(new Runnable() {
			public void run() {
				new OBOMergeCanvas().setVisible(true);
			}
		});
	}

	//GEN-BEGIN:variables
	// Variables declaration - do not modify
	private javax.swing.JPanel advancedOptionPanel;
	private javax.swing.JComboBox failOnClashChoiceComboBox;
	private javax.swing.JLabel failOnClashLabel;
	private javax.swing.JComboBox fileFormatComboBox;
	private javax.swing.JPanel inputFilePanel;
	private javax.swing.JLabel jLabel1;
	private javax.swing.JLabel jLabel2;
	private javax.swing.JScrollPane jScrollPane2;
	private javax.swing.JTextField jTextField1;
	private javax.swing.JTextField jTextField2;
	private javax.swing.JButton mainEditedFileButton;
	private javax.swing.JLabel mainEditedFileLabel;
	private javax.swing.JTextField mainEditedFileTextField;
	private javax.swing.JButton mergeButton;
	private javax.swing.JLabel mergedFileLabel;
	private javax.swing.JTabbedPane oboMergeTabbedPane1;
	private javax.swing.JPanel ontologyPathPanel;
	private javax.swing.JButton outputFileButton;
	private javax.swing.JLabel outputFileFormatLabel;
	private javax.swing.JTextField outputFileTextField;
	private javax.swing.JButton parentFileButton;
	private javax.swing.JLabel parentFileLabel;
	private javax.swing.JTextField parentFileTextField;
	private javax.swing.JPanel processFeedbackPanel;
	private javax.swing.JTextArea progressTextArea;
	private javax.swing.JButton saveFeedbackToFileBrowseButton;
	private javax.swing.JCheckBox saveFeedbackToFileCheckBox;
	private javax.swing.JTextField saveFeedbackToFileTextField;
	private javax.swing.JButton secondaryEditedFileButton;
	private javax.swing.JLabel secondaryEditedFileLabel;
	private javax.swing.JTextField secondaryEditedFileTextField;
	private javax.swing.JPanel showProgressPanel;
	private javax.swing.JComboBox updateIDsChoiceComboBox;
	private javax.swing.JLabel updateIDsLabel;

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