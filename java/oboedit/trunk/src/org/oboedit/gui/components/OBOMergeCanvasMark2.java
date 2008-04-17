package org.oboedit.gui.components;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.ComponentManager;
import org.bbop.framework.GUIComponent;
import org.bbop.framework.dock.LayoutAdapter;
import org.bbop.framework.dock.LayoutListener;
import org.bbop.swing.MinusIcon;
import org.bbop.swing.PlusIcon;
import javax.swing.JFileChooser;

public class OBOMergeCanvasMark2 extends AbstractGUIComponent{

	public OBOMergeCanvasMark2(String id) {
		super(id);
		// TODO Auto-generated constructor stub
	}

	public void save(){
		System.out.println("all going well so far.");
	}

	protected LayoutListener layoutListener = new LayoutAdapter() {
		public boolean closing(GUIComponent c) {
			if (c.equals(OBOMergeCanvasMark2.this)) {
				save();
			}
			return true;
		}

	};


	@Override
	public void init() {
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
		JLabel outputFileFormatLabel = new JLabel("Output File Format");
		String[] fileFormatOptions = { "OBO_1_2", "OBO_1_0" };
		JComponent outputFileFormatCombobox = new JComboBox(fileFormatOptions);
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

		
		//not sure what this is for:
		ComponentManager.getManager().addLayoutListener(layoutListener);

		setLayout(new BorderLayout());

		
		JPanel mainGUIPanel = new JPanel();

		add(oboMergeTabbedPane, "Center");
		oboMergeTabbedPane.addTab("Process Feedback", null, processFeedbackPanel, "Process Feedback");
		oboMergeTabbedPane.addTab("Ontology Files", null, mainGUIPanel, "Ontology Files");

		mainGUIPanel.setLayout(new BorderLayout());
		
		mainGUIPanel.add(saveProfilePanel, BorderLayout.NORTH);
		mainGUIPanel.add(inputFilePanel, BorderLayout.CENTER);
		mainGUIPanel.add(mergeOptionPanel, BorderLayout.SOUTH);
		
		inputFilePanel.setBorder(new TitledBorder ("Ontology File Paths"));
		mergeOptionPanel.setBorder(new TitledBorder ("Merge Options"));
		saveProfilePanel.setBorder(new TitledBorder ("Saved Profiles"));
	      		
		//Make GridBag layout for the contents of the inputFilePanel. 
		inputFilePanel.setLayout(new GridBagLayout());
		GridBagConstraints inputFilePanelGBC = new GridBagConstraints();

		inputFilePanelGBC.fill = GridBagConstraints.HORIZONTAL;
		inputFilePanelGBC.gridx = 0;
		inputFilePanelGBC.gridy = 0;
		inputFilePanelGBC.anchor = GridBagConstraints.FIRST_LINE_START;
		inputFilePanelGBC.weightx = 1;
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
		saveProfilePanel.setLayout(new BoxLayout(saveProfilePanel, BoxLayout.X_AXIS));

		saveProfilePanel.setLayout(new GridBagLayout());
		GridBagConstraints saveProfilePanelGBC = new GridBagConstraints();

		saveProfilePanelGBC.fill = GridBagConstraints.NONE;
		saveProfilePanelGBC.gridx = 0;
		saveProfilePanelGBC.gridy = 0;
		saveProfilePanelGBC.anchor = GridBagConstraints.LINE_START;
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
		mergeOptionPanel.add(outputFileFormatLabel, mergeOptionPanelGBC);
		
		mergeOptionPanelGBC.gridx = 3;
		mergeOptionPanelGBC.gridy = 0;
		mergeOptionPanel.add(outputFileFormatCombobox, mergeOptionPanelGBC);

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

		mainGUIPanel.add(Box.createVerticalGlue());
	


		
	}


	private static void addAComponentXAlignment(JComponent componentName, Container container) {
//		componentName.setAlignmentX(Component.CENTER_ALIGNMENT);
		container.add(componentName);

	}
	private static void addAComponentYAlignment(JComponent componentName, Container container) {
//		componentName.setAlignmentY(Component.CENTER_ALIGNMENT);
		container.add(componentName);

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
}


