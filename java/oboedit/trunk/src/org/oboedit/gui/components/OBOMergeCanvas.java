package org.oboedit.gui.components;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.Reader;
import java.util.ArrayList;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

import org.apache.batik.ext.awt.image.renderable.RedRable;
import org.bbop.dataadapter.DataAdapterException;
import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.GUIComponent;
import org.bbop.framework.GUIManager;
import org.bbop.framework.dock.LayoutAdapter;
import org.bbop.framework.dock.LayoutListener;
import org.bbop.swing.MinusIcon;
import org.bbop.swing.PlusIcon;
import org.oboedit.launcher.OBOMerge;

import javax.swing.JFrame;


/*
* By Jennifer Deegan and Nicolas Rodriguez
* EMBL-EBI
* January to April 2008
*/

public class OBOMergeCanvas extends AbstractGUIComponent {

   public static final int HIDE_ON_CLOSE = 1;
   private static void addAComponentXAlignment(JComponent componentName, Container container) {
       container.add(componentName);
   }
   private static void addAComponentYAlignment(JComponent componentName, Container container) {
       container.add(componentName);
   }
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
   JPanel inputFilePanel = new JPanel();
   JPanel filePathPanel = new JPanel();
   JPanel mergeOptionPanel = new JPanel();
   JPanel saveProfilePanel = new JPanel();
   JPanel parentFilePanel = new JPanel();
   JPanel liveFilePanel = new JPanel();
   JPanel branchFilePanel = new JPanel();
   JPanel mergedFilePanel = new JPanel();
   JLabel parentFileLabel = new JLabel("Parent File   ");
   JTextField parentFileTextField = new JTextField(5);
   JButton parentFileBrowseButton = new JButton("Browse");
   JLabel branchFileLabel = new JLabel("Branch File  ");
   JTextField branchFileTextField = new JTextField(5);
   JButton branchFileBrowseButton = new JButton("Browse");
   JLabel liveFileLabel =   new JLabel("Live File       ");
   JTextField liveFileTextField = new JTextField(5);
   JButton liveFileBrowseButton = new JButton("Browse");
   JLabel mergedFileLabel = new JLabel("Merged File ");
   JTextField mergedFileTextField = new JTextField(5);
   JButton mergedFileBrowseButton = new JButton("Browse");
   JLabel saveProfileLabel = new JLabel("Save Profile");
   String[] savedProfiles = {"new profile", ""};
   JComboBox saveProfileComboBox = new JComboBox(savedProfiles);
   JButton addProfilePlusButton = new JButton(new PlusIcon(1.5f, 8, 8));
   JButton removeProfileMinusButton = new JButton(new MinusIcon(1.5f, 8, 8));
   JPanel topLinePanel = new JPanel();
   JPanel bottomLinePanel = new JPanel();
   JLabel updateIDsLabel = new JLabel("Update IDs");
   String[] idOptionsFailOnClash = { "IF_LIKELY", "   ", "NEVER", "ALWAYS" };
   String[] idOptionsUpdateIDs = { "   ", "NEVER", "ALWAYS", "IF_LIKELY" };
   JComboBox updateIDsCombobox = new JComboBox(idOptionsUpdateIDs);
   JLabel mergedFileFormatLabel = new JLabel("Output File Format");
   String[] fileFormatOptions = { "OBO_1_2", "OBO_1_0" };
   JComboBox mergedFileFormatCombobox = new JComboBox(fileFormatOptions);
   JComponent failOnClashLabel = new JLabel("Fail On Clash");
   JComboBox failOnClashCombobox = new JComboBox(idOptionsFailOnClash);
   JLabel ignoreClashOnIDsLabel = new JLabel("Ignore Clash on ID");
   JTextArea ignoreClashOnIDsTextArea = new JTextArea();
   String id;
   JTabbedPane oboMergeTabbedPane = new JTabbedPane();
   JPanel processFeedbackPanel = new JPanel();
   JButton saveFeedbackToFileBrowseButton = new JButton("Browse");
   JLabel saveFeedbackToFileLabel = new JLabel("Save Feedback to File");
   JTextField saveFeedbackToFileTextField = new JTextField();
   JTextField ProgressTextField = new JTextField();
   String saveFeedbackToFileTextFieldString = new String("C:\\output.txt");
   JFileChooser fileChooser = new JFileChooser();
   String updateIDsChoiceString = new String("   ");
   String failOnClashChoiceString = new String("IF_LIKELY");
   String outputFormatChoiceString = new String("OBO_1_2");
   String[] obomergeArgsArray = new String[0];
   JButton mergeButton = new JButton("Merge");
   JFrame missingPathFrame = new JFrame();
   ArrayList<String> obomergeArgsArrayList = new ArrayList<String>();
   String parentFileTextFieldString = new String();
   String liveFileTextFieldString = new String();
   String branchFileTextFieldString = new String();
   String mergedFileTextFieldString = new String();
   JPanel mergeAndAdvancedButtonPanel = new JPanel();
   JButton advancedButton = new JButton("Advanced");
   JPanel showProgressPanel = new JPanel();
   JPanel saveProgressToFilePanel = new JPanel();
   JPanel centerPanel = new JPanel();
   JPanel saveFeedbackToFileDetailPanel = new JPanel();
   String feedbackTextAreaString = new String();
   JTextArea feedbackTextArea = new JTextArea(feedbackTextAreaString);
   JLabel feedbackFilePathLabel = new JLabel("Feedback File Path");
   JTextField feedbackFileTextField = new JTextField();
   String ignoreClashOnIDsChoiceString = new String();
   PrintStream feedbackFileOutputStream;
   JButton saveFeedbackToFileSaveButton = new JButton("Save");

   //Properties
   //GUIManager.getPrefsDir().getPath()

   public OBOMergeCanvas(String id) {
       super(id);
   }

   @Override
   public void init() {

       setLayout(new BorderLayout());
       JPanel mainGUIPanel = new JPanel();
       JScrollPane mainGUIPanelScrollPane = new JScrollPane(mainGUIPanel);

//        Add tabs.

       add(oboMergeTabbedPane, "Center");
       oboMergeTabbedPane.addTab("Ontology Files", null, mainGUIPanelScrollPane, "Ontology Files");
       oboMergeTabbedPane.addTab("Process Feedback", null, processFeedbackPanel, "Process Feedback");

//        End of tabs.

//        Layout of main input panel.              
       mainGUIPanel.setLayout(new GridBagLayout());
       GridBagConstraints mainGUIPanelGBC = new GridBagConstraints();

       mainGUIPanelGBC.fill = GridBagConstraints.HORIZONTAL;
       mainGUIPanelGBC.gridx = 0;
       mainGUIPanelGBC.gridy = 0;
       mainGUIPanelGBC.weightx = 1;
       mainGUIPanelGBC.insets = new Insets(5,5,5,5);
       mainGUIPanel.add(saveProfilePanel, mainGUIPanelGBC);
       saveProfilePanel.setBorder(new TitledBorder ("Saved Profiles"));

       mainGUIPanelGBC.gridy = 1;
       mainGUIPanel.add(inputFilePanel, mainGUIPanelGBC);
       inputFilePanel.setBorder(new TitledBorder ("File Paths"));

       mainGUIPanelGBC.gridy = 2;
       mainGUIPanel.add(mergeOptionPanel, mainGUIPanelGBC);
       mergeOptionPanel.setBorder(new TitledBorder ("Merge Options"));

       mainGUIPanelGBC.gridy = 3;
       mainGUIPanel.add(mergeAndAdvancedButtonPanel, mainGUIPanelGBC);
       mergeOptionPanel.setBorder(new TitledBorder ("Merge Options"));

//        End of layout of main input panel

//        Start of basic panel where the four file paths go.
//        Add a panel for each file.       
       inputFilePanel.setLayout(new GridBagLayout());
       GridBagConstraints inputFilePanelGBC = new GridBagConstraints();

//        Parent file panel

       inputFilePanelGBC.fill = GridBagConstraints.HORIZONTAL;
       inputFilePanelGBC.gridx = 0;
       inputFilePanelGBC.gridy = 0;
       inputFilePanelGBC.anchor = GridBagConstraints.FIRST_LINE_START;
       inputFilePanelGBC.weightx = 1;
       inputFilePanelGBC.insets = new Insets(5,5,5,5);
       inputFilePanel.add(parentFilePanel, inputFilePanelGBC);

//        Live file panel              
       inputFilePanelGBC.gridy = 1;
       inputFilePanel.add(liveFilePanel, inputFilePanelGBC);

//        Branch file panel
       inputFilePanelGBC.gridy = 2;
       inputFilePanel.add(branchFilePanel, inputFilePanelGBC);

//        Merged file panel              
       inputFilePanelGBC.gridy = 3;
       inputFilePanel.add(mergedFilePanel, inputFilePanelGBC);

//        End of setting up a panel for each input file.       

//        Start setting up the contents of the file input panels.

//        Start Parent file panel contents.

       parentFilePanel.setLayout(new GridBagLayout());
       GridBagConstraints parentFilePanelGBC = new GridBagConstraints();

//        Parent file label
       parentFilePanelGBC.fill = GridBagConstraints.NONE;
       parentFilePanelGBC.gridx = 0;
       parentFilePanelGBC.gridy = 0;
       parentFilePanelGBC.anchor = GridBagConstraints.LINE_START;
       parentFilePanelGBC.insets = new Insets(0,2,2,0);
       parentFilePanelGBC.weightx = 0;
       parentFilePanel.add(parentFileLabel, parentFilePanelGBC);

//        Parent file text field.              
       parentFilePanelGBC.fill = GridBagConstraints.HORIZONTAL;
       parentFilePanelGBC.gridx = 1;
       parentFilePanelGBC.anchor = GridBagConstraints.CENTER;
       parentFilePanelGBC.weightx = 1;
       parentFilePanel.add(parentFileTextField, parentFilePanelGBC);

//        Parent file Browse button.               
       parentFilePanelGBC.fill = GridBagConstraints.NONE;
       parentFilePanelGBC.gridx = 2;
       parentFilePanelGBC.anchor = GridBagConstraints.LINE_END;
       parentFilePanelGBC.weightx = 0;
       parentFilePanel.add(parentFileBrowseButton, parentFilePanelGBC);
//        End of Parent file Panel

//        Start of Live file Panel contents.              
       liveFilePanel.setLayout(new GridBagLayout());
       GridBagConstraints liveFilePanelGBC = new GridBagConstraints();

//        Live file Label.              
       liveFilePanelGBC.fill = GridBagConstraints.NONE;
       liveFilePanelGBC.gridx = 0;
       liveFilePanelGBC.gridy = 0;
       liveFilePanelGBC.anchor = GridBagConstraints.LINE_START;
       liveFilePanelGBC.insets = new Insets(0,2,2,0);
       liveFilePanel.add(liveFileLabel, liveFilePanelGBC);

//        Live file Text Field.              
       liveFilePanelGBC.fill = GridBagConstraints.HORIZONTAL;
       liveFilePanelGBC.gridx = 1;
       liveFilePanelGBC.anchor = GridBagConstraints.CENTER;
       liveFilePanelGBC.weightx = 1;
       liveFilePanel.add(liveFileTextField, liveFilePanelGBC);

//        Live file Browse Button.               
       liveFilePanelGBC.fill = GridBagConstraints.NONE;
       liveFilePanelGBC.gridx = 2;
       liveFilePanelGBC.anchor = GridBagConstraints.LINE_END;
       liveFilePanelGBC.weightx = 0;
       liveFilePanel.add(liveFileBrowseButton, liveFilePanelGBC);
//        End of live file panel contents.

//        Start of Branch file panel contents.
       branchFilePanel.setLayout(new GridBagLayout());
       GridBagConstraints branchFilePanelGBC = new GridBagConstraints();

//        Branch file label
       branchFilePanelGBC.fill = GridBagConstraints.NONE;
       branchFilePanelGBC.gridx = 0;
       branchFilePanelGBC.gridy = 0;
       branchFilePanelGBC.weightx = 0;
       branchFilePanelGBC.anchor = GridBagConstraints.LINE_START;
       branchFilePanelGBC.insets = new Insets(0,2,2,0);
       branchFilePanel.add(branchFileLabel, branchFilePanelGBC);

//        Branch file text field.               
       branchFilePanelGBC.fill = GridBagConstraints.HORIZONTAL;
       branchFilePanelGBC.weightx = 1;
       branchFilePanelGBC.gridx = 1;
       branchFilePanelGBC.anchor = GridBagConstraints.CENTER;
       branchFilePanel.add(branchFileTextField, branchFilePanelGBC);

//        Branch file browse button              
       branchFilePanelGBC.fill = GridBagConstraints.NONE;
       branchFilePanelGBC.gridx = 2;
       branchFilePanelGBC.anchor = GridBagConstraints.LINE_END;
       branchFilePanelGBC.weightx = 0;
       branchFilePanel.add(branchFileBrowseButton, branchFilePanelGBC);
//        End of branch file panel contents.

//        Start of merged file panel contents.
       mergedFilePanel.setLayout(new GridBagLayout());
       GridBagConstraints mergedFilePanelGBC = new GridBagConstraints();

//        Merged file label.              
       mergedFilePanelGBC.fill = GridBagConstraints.NONE;
       mergedFilePanelGBC.gridx = 0;
       mergedFilePanelGBC.gridy = 0;
       mergedFilePanelGBC.anchor = GridBagConstraints.LINE_START;
       mergedFilePanelGBC.insets = new Insets(0,2,2,0);
       mergedFilePanel.add(mergedFileLabel, mergedFilePanelGBC);

//        Merged file Text field.
       mergedFilePanelGBC.fill = GridBagConstraints.HORIZONTAL;
       mergedFilePanelGBC.gridx = 1;
       mergedFilePanelGBC.anchor = GridBagConstraints.CENTER;
       mergedFilePanelGBC.weightx = 1;
       mergedFilePanel.add(mergedFileTextField, mergedFilePanelGBC);

//        Merged file browse button.               
       mergedFilePanelGBC.fill = GridBagConstraints.NONE;
       mergedFilePanelGBC.gridx = 2;
       mergedFilePanelGBC.anchor = GridBagConstraints.LINE_END;
       mergedFilePanelGBC.weightx = 0;
       mergedFilePanel.add(mergedFileBrowseButton, mergedFilePanelGBC);
//        End of merged file panel contents.


//        Start of save profile panel contents.
       saveProfilePanel.setLayout(new GridBagLayout());
       GridBagConstraints saveProfilePanelGBC = new GridBagConstraints();

//        Save profile label.              
       saveProfilePanelGBC.fill = GridBagConstraints.NONE;
       saveProfilePanelGBC.gridx = 0;
       saveProfilePanelGBC.gridy = 0;
       saveProfilePanelGBC.anchor = GridBagConstraints.LINE_START;
       saveProfilePanelGBC.insets = new Insets(5,5,5,5);
       saveProfilePanel.add(saveProfileLabel, saveProfilePanelGBC);

//        Save profile editable combobox.              
       saveProfilePanelGBC.fill = GridBagConstraints.HORIZONTAL;
       saveProfilePanelGBC.gridx = 1;
       saveProfilePanelGBC.anchor = GridBagConstraints.CENTER;
       saveProfilePanelGBC.weightx = 1;
       saveProfilePanel.add(saveProfileComboBox, saveProfilePanelGBC);
       saveProfileComboBox.setEditable(true);

//        Save profile plus button.               
       saveProfilePanelGBC.fill = GridBagConstraints.NONE;
       saveProfilePanelGBC.gridx = 2;
       saveProfilePanelGBC.weightx = 0;
       saveProfilePanel.add(addProfilePlusButton, saveProfilePanelGBC);

//        Save profile minus button.
       saveProfilePanelGBC.gridx = 3;
       saveProfilePanelGBC.anchor = GridBagConstraints.LINE_END;
       saveProfilePanelGBC.weightx = 0;
       saveProfilePanel.add(removeProfileMinusButton, saveProfilePanelGBC);
//        End of save profile panel contents.

//        Start of merge option panel contents.
       mergeOptionPanel.setLayout(new GridBagLayout());
       GridBagConstraints mergeOptionPanelGBC = new GridBagConstraints();

//        Update ID control label.                
       mergeOptionPanelGBC.fill = GridBagConstraints.NONE;
       mergeOptionPanelGBC.gridx = 0;
       mergeOptionPanelGBC.gridy = 0;
       mergeOptionPanelGBC.ipadx = 5;
       mergeOptionPanelGBC.ipady = 5;
       mergeOptionPanelGBC.insets = new Insets(5,5,5,5);
       mergeOptionPanel.add(updateIDsLabel, mergeOptionPanelGBC);

//        Update ID combobox
       mergeOptionPanelGBC.gridx = 1;
       mergeOptionPanel.add(updateIDsCombobox, mergeOptionPanelGBC);

//        Merged file format label.
       mergeOptionPanelGBC.gridx = 2;
       mergeOptionPanel.add(mergedFileFormatLabel, mergeOptionPanelGBC);

//        Merged file format combobox.              mergeOptionPanelGBC.gridx = 3;
       mergeOptionPanel.add(mergedFileFormatCombobox, mergeOptionPanelGBC);

//        Fail on clash option label.               mergeOptionPanelGBC.gridx = 0;
       mergeOptionPanelGBC.gridy = 1;
       mergeOptionPanel.add(failOnClashLabel, mergeOptionPanelGBC);

//        Fail on clash combobox              mergeOptionPanelGBC.gridx = 1;
       mergeOptionPanelGBC.gridy = 1;
       mergeOptionPanel.add(failOnClashCombobox, mergeOptionPanelGBC);

//        If we ever want to implement the IgnoreClashOnIDs option in the script then this commented
//        out code will produce the GUI items required. The processing code has not been written.        
       //        mergeOptionPanelGBC.gridx = 2;
//        mergeOptionPanelGBC.gridy = 1;
//        mergeOptionPanel.add(ignoreClashOnIDsLabel, mergeOptionPanelGBC);

//        mergeOptionPanelGBC.gridx = 3;
//        mergeOptionPanelGBC.gridy = 1;
//        mergeOptionPanel.add(ignoreClashOnIDsTextArea, mergeOptionPanelGBC);

//        End of merge option panel contents.       
//        Start of contents of panel with merge button and advanced button. (Bottom panel)              
       mergeAndAdvancedButtonPanel.setLayout(new GridBagLayout());
       GridBagConstraints mergeAndAdvancedButtonPanelGBC = new GridBagConstraints();

//        Advanced button.              
       mergeAndAdvancedButtonPanelGBC.fill = GridBagConstraints.NONE;
       mergeAndAdvancedButtonPanelGBC.gridx = 0;
       mergeAndAdvancedButtonPanelGBC.gridy = 0;
       mergeAndAdvancedButtonPanelGBC.weightx = 1;
       mergeAndAdvancedButtonPanel.add(advancedButton, mergeAndAdvancedButtonPanelGBC);

//        Merge button              
       mergeAndAdvancedButtonPanelGBC.gridx = 1;
       mergeAndAdvancedButtonPanel.add(mergeButton, mergeAndAdvancedButtonPanelGBC);
//        End of contents of panel with advanced button and merge button on it.

//        Start of contents of panel in other tab that shows progress.              
       processFeedbackPanel.setLayout(new GridBagLayout());
       GridBagConstraints processFeedbackPanelGBC = new GridBagConstraints();

//        Scroll pane enclosing progress window.               
       JScrollPane feedbackTextAreaScrollPane = new JScrollPane(feedbackTextArea);

//        Scroll pane enclosing progress window now on tab.               
       processFeedbackPanelGBC.fill = GridBagConstraints.BOTH;
       processFeedbackPanelGBC.gridx = 0;
       processFeedbackPanelGBC.gridy = 0;
       processFeedbackPanelGBC.anchor = GridBagConstraints.PAGE_START;
       processFeedbackPanelGBC.weightx = 1;
       processFeedbackPanelGBC.weighty = 1;
       processFeedbackPanelGBC.insets = new Insets(5,5,5,5);
       processFeedbackPanel.add(feedbackTextAreaScrollPane, processFeedbackPanelGBC);

//        Panel to contain part to let you select where progress feedback is saved to a file.               
       processFeedbackPanelGBC.fill = GridBagConstraints.HORIZONTAL;
       processFeedbackPanelGBC.gridy = 1;
       processFeedbackPanelGBC.weightx = 1;
       processFeedbackPanelGBC.weighty = 0;
       processFeedbackPanelGBC.anchor = GridBagConstraints.PAGE_END;
       processFeedbackPanel.add(saveFeedbackToFileDetailPanel, processFeedbackPanelGBC);

//        Contents of panel for saving feedback to file.
       saveFeedbackToFileDetailPanel.setLayout(new GridBagLayout());
       GridBagConstraints saveFeedbackToFileDetailPanelGBC = new GridBagConstraints();

//        Save feedback to file label.
       saveFeedbackToFileDetailPanelGBC.fill = GridBagConstraints.NONE;
       saveFeedbackToFileDetailPanelGBC.gridx = 0;
       saveFeedbackToFileDetailPanelGBC.gridy = 0;
       saveFeedbackToFileDetailPanelGBC.weightx = 0;
       saveFeedbackToFileDetailPanelGBC.insets = new Insets(5,5,5,5);
       saveFeedbackToFileDetailPanel.add(feedbackFilePathLabel, saveFeedbackToFileDetailPanelGBC);

//        Save feedback to file text field.
       saveFeedbackToFileDetailPanelGBC.fill = GridBagConstraints.HORIZONTAL;
       saveFeedbackToFileDetailPanelGBC.gridx = 1;
       saveFeedbackToFileDetailPanelGBC.weightx = 1;
       saveFeedbackToFileDetailPanel.add(saveFeedbackToFileTextField, saveFeedbackToFileDetailPanelGBC);

//        Save feedback to file browse button.
       saveFeedbackToFileDetailPanelGBC.fill = GridBagConstraints.NONE;
       saveFeedbackToFileDetailPanelGBC.gridx = 2;
       saveFeedbackToFileDetailPanelGBC.gridy = 0;
       saveFeedbackToFileDetailPanelGBC.weightx = 0;
       saveFeedbackToFileDetailPanel.add(saveFeedbackToFileBrowseButton, saveFeedbackToFileDetailPanelGBC);

//        Save feedback to file save button.
       saveFeedbackToFileDetailPanelGBC.fill = GridBagConstraints.NONE;
       saveFeedbackToFileDetailPanelGBC.gridx = 3;
       saveFeedbackToFileDetailPanelGBC.gridy = 0;
       saveFeedbackToFileDetailPanelGBC.weightx = 0;
       saveFeedbackToFileDetailPanel.add(saveFeedbackToFileSaveButton, saveFeedbackToFileDetailPanelGBC);
//        End of save feedback to file panel contents.

//        These to lines are needed or the GUI components will not show up.               
       validate();
       repaint();

//        Read the profiles from the profile file.
       //Not sure how to do this yet.
            //        Start of add action listeners.              
       saveFeedbackToFileBrowseButton.addActionListener(new java.awt.event.ActionListener() {
           public void actionPerformed(java.awt.event.ActionEvent evt) {
               saveFeedbackToFileBrowseButtonActionPerformed(evt);
           }
       });

       addProfilePlusButton.addActionListener(new java.awt.event.ActionListener() {
           public void actionPerformed(java.awt.event.ActionEvent evt) {
               addProfilePlusButtonActionPerformed(evt);
           }
       });

       mergedFileFormatCombobox
       .addActionListener(new java.awt.event.ActionListener() {
           public void actionPerformed(java.awt.event.ActionEvent evt) {
               mergedFileFormatComboboxActionPerformed(evt);
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

       saveFeedbackToFileSaveButton
       .addActionListener(new java.awt.event.ActionListener() {
           public void actionPerformed(java.awt.event.ActionEvent evt) {
               try {
                   saveFeedbackToFileSaveButtonActionPerformed(evt);
               } catch (IOException e) {
                   // TODO Auto-generated catch block
                   e.printStackTrace();
               }
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

       mergeButton
       .addActionListener(new java.awt.event.ActionListener() {
           public void actionPerformed(java.awt.event.ActionEvent evt) {
               mergeButtonActionPerformed(evt);
           }
       });

       mergedFileBrowseButton.addActionListener(new java.awt.event.ActionListener() {
           public void actionPerformed(java.awt.event.ActionEvent evt) {
               mergedFileBrowseButtonActionPerformed(evt);
           }
       });



       advancedButton.addActionListener(new java.awt.event.ActionListener() {
           public void actionPerformed(java.awt.event.ActionEvent evt) {
               advancedButtonActionPerformed(evt);
           }
//            End of add action listeners

//            Start of action performeds.

//            Advance button action                      
           private void advancedButtonActionPerformed(ActionEvent evt) {
               boolean saveProfilePanelVisibility = saveProfilePanel.isVisible();
               saveProfilePanel.setVisible(!saveProfilePanelVisibility);

               boolean mergeOptionPanelVisibility = mergeOptionPanel.isVisible();
               mergeOptionPanel.setVisible(!mergeOptionPanelVisibility);

               if (saveProfilePanelVisibility) {
                   advancedButton.setText("Advanced");
               } else {
                   advancedButton.setText("Basic");
               }
           }
       });
       saveProfilePanel.setVisible(false);
       mergeOptionPanel.setVisible(false);
   }

   protected void addProfilePlusButtonActionPerformed(ActionEvent evt) {

            //        setProperty(String key, String value)
//        Puts the key/value pair in the Properties object.
//    remove(Object key)
//        Removes the key/value pair associated with key.
               }
   //    This is part of the mechanism to close the dialog now that shows if someone has not filled
//    in all the file paths.       
   public void save(){
//        System.out.println("all going well so far.");
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

//            System.out.println("arg = " + branchFileTextFieldString);

       }

   }
   protected void saveFeedbackToFileSaveButtonActionPerformed(ActionEvent evt) throws IOException {

       File feedbackFile = new File(saveFeedbackToFileTextFieldString);
       String feedbackTextAreaContents = feedbackTextArea.getText();
       BufferedWriter out = new BufferedWriter(new FileWriter(feedbackFile));
       out.write(feedbackTextAreaContents);
       out.flush();
       out.close();


   }


   private void failOnClashChoiceComboBoxActionPerformed(
           java.awt.event.ActionEvent evt) {
       failOnClashChoiceString = (String) failOnClashCombobox
       .getSelectedItem();
//        System.out.println("arg = " + failOnClashChoiceString);

   }

   //    This is part of the mechanism to close the dialog now that shows if someone has not filled
//    in all the file paths.       
   private int getDefaultCloseOperation() {
       // TODO Auto-generated method stub
       return defaultCloseOperation;

   };

   //    This method takes all the different variables from the various controls on the file input page
//    and makes them into an array that can be fed to the OBOMerge script which is in a totally
//    different place.
   private Boolean makeArgArrayList() {
       /*
        * This class takes the return strings from all the
        *  GUI controls and puts them
        * into the array to be fed to obomerge.
        */

       obomergeArgsArrayList.clear();

       System.out.println("Arguments applied were:\n");

       if ( ! failOnClashChoiceString.trim().equals("")) {
           obomergeArgsArrayList.add("-fail-on-clash");
           obomergeArgsArrayList.add(failOnClashChoiceString);
           System.out.println("    -fail-on-clash " + failOnClashChoiceString);

       }

       if ( ! updateIDsChoiceString.trim().equals("")) {
           obomergeArgsArrayList.add("-update-ids");
           obomergeArgsArrayList.add(updateIDsChoiceString);
           System.out.println("    -update-ids " + updateIDsChoiceString);
       }
       //This feature not implemented.
//        if (ignoreClashOnIDsChoiceString != "") {
//        obomergeArgsArrayList.add("-ignore-clash-on-id");
//        obomergeArgsArrayList.add(ignoreClashOnIDsChoiceString);
//        System.out.println("    -ignore-clash-on-id "
//        + ignoreClashOnIDsChoiceString);
//        }


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

//        System.out.println(obomergeArgsArrayList);
       obomergeArgsArrayList.trimToSize();
       obomergeArgsArray = obomergeArgsArrayList.toArray(obomergeArgsArray);

       System.out.println("\nThe merge process gave the following feedback:\n");
       return true;
   }

   //    Mechanism fired when the merge button is pressed. Take the array and sends it to the obomerge
//    script.
   private void mergeButtonActionPerformed(java.awt.event.ActionEvent evt) {

       oboMergeTabbedPane.setSelectedIndex(1);
       repaint();      
       WriteFeedbackToTextArea();

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
           catch (RuntimeException e) {
               e.printStackTrace();
           }          }
             //Write list of profiles out now.
                     }
   private void mergedFileBrowseButtonActionPerformed(java.awt.event.ActionEvent evt) {
       int returnVal = fileChooser.showOpenDialog(null);
       if (returnVal == JFileChooser.APPROVE_OPTION) {
           mergedFileTextFieldString = fileChooser.getSelectedFile()
           .getAbsolutePath();
           mergedFileTextField.setText(mergedFileTextFieldString);

//            System.out.println("arg = " + mergedFileTextFieldString);
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

           //        System.out.println("arg = " + mergedFileTextFieldString);
       }

   }



   private void parentFileButtonActionPerformed(java.awt.event.ActionEvent evt) {
       int returnVal = fileChooser.showOpenDialog(null);
       if (returnVal == JFileChooser.APPROVE_OPTION) {
           parentFileTextFieldString = fileChooser.getSelectedFile()
           .getAbsolutePath();
           parentFileTextField.setText(parentFileTextFieldString);

//            System.out.println("arg = " + parentFileTextFieldString);
       }

   }


   private void saveFeedbackToFileBrowseButtonActionPerformed(
           java.awt.event.ActionEvent evt) {
       int returnVal = fileChooser.showOpenDialog(null);
       if (returnVal == JFileChooser.APPROVE_OPTION) {
           saveFeedbackToFileTextFieldString = fileChooser.getSelectedFile().getAbsolutePath();
           saveFeedbackToFileTextField.setText(saveFeedbackToFileTextFieldString);
       }
   }

   private void ShowFeedbackInWindow() {
       feedbackTextArea.setText(feedbackFileOutputStream.toString());
   }

   private void updateIDsComboboxActionPerformed(
           java.awt.event.ActionEvent evt) {
       updateIDsChoiceString = (String) updateIDsCombobox
       .getSelectedItem();
       //    System.out.println("arg = " + updateIDsChoiceString);
   }

   private void WriteFeedbackToTextArea() {

       PrintStream toTextArea = new PrintStream( new TextAreaOutputStream( feedbackTextArea ) );

       System.setOut( toTextArea );
       System.setErr( toTextArea );

   }


   protected void liveFileBrowseButtonActionPerformed(ActionEvent evt) {

       int returnVal = fileChooser.showOpenDialog(null);
       if (returnVal == JFileChooser.APPROVE_OPTION) {
           liveFileTextFieldString = fileChooser.getSelectedFile()
           .getAbsolutePath();
           liveFileTextField.setText(liveFileTextFieldString);

           //        System.out.println("arg = " + liveFileTextFieldString);

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



















