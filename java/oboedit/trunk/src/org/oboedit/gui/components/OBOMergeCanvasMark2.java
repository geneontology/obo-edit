package org.oboedit.gui.components;

import java.awt.Component;
import java.awt.Container;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;

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
		JTextField ignoreClashOnIDsTextField = new JTextField();
		String id;
		JTabbedPane oboMergeTabbedPane = new JTabbedPane();
		JPanel processFeedbackPanel = new JPanel();
		


		//not sure what this is for:
		ComponentManager.getManager().addLayoutListener(layoutListener);
		removeAll();

		add(oboMergeTabbedPane, "Center");
		oboMergeTabbedPane.addTab("Process Feedback", null, processFeedbackPanel, "Process Feedback");
		oboMergeTabbedPane.addTab("Ontology Files", null, inputFilePanel, "Ontology Files");

		
		inputFilePanel.setLayout(new BoxLayout(inputFilePanel, BoxLayout.Y_AXIS));
		addAComponentYAlignment(saveProfilePanel, inputFilePanel);
		addAComponentYAlignment(filePathPanel, inputFilePanel);
		addAComponentYAlignment(mergeOptionPanel, inputFilePanel);

		inputFilePanel.setBorder(new TitledBorder ("Ontology File Paths"));
		mergeOptionPanel.setBorder(new TitledBorder ("Merge Options"));
		saveProfilePanel.setBorder(new TitledBorder ("Saved Profiles"));


		filePathPanel.setLayout(new BoxLayout(filePathPanel, BoxLayout.Y_AXIS));

		//Add four lines in which the file paths will be loaded. 		
		addAComponentXAlignment(parentFilePanel, filePathPanel);
		addAComponentXAlignment(liveFilePanel, filePathPanel);
		addAComponentXAlignment(branchFilePanel, filePathPanel);
		addAComponentXAlignment(mergedFilePanel, filePathPanel);

		parentFilePanel.setLayout(new BoxLayout(parentFilePanel, BoxLayout.X_AXIS));

		addAComponentYAlignment(parentFileLabel, parentFilePanel);
		addAComponentYAlignment(parentFileTextField, parentFilePanel);
		addAComponentYAlignment(parentFileBrowseButton, parentFilePanel);

		liveFilePanel.setLayout(new BoxLayout(liveFilePanel, BoxLayout.X_AXIS));

		addAComponentYAlignment(liveFileLabel, liveFilePanel);
		addAComponentYAlignment(liveFileTextField, liveFilePanel);
		addAComponentYAlignment(liveFileBrowseButton, liveFilePanel);

		branchFilePanel.setLayout(new BoxLayout(branchFilePanel, BoxLayout.X_AXIS));

		addAComponentYAlignment(branchFileLabel, branchFilePanel);
		addAComponentYAlignment(branchFileTextField, branchFilePanel);
		addAComponentYAlignment(branchFileBrowseButton, branchFilePanel);

		mergedFilePanel.setLayout(new BoxLayout(mergedFilePanel, BoxLayout.X_AXIS));

		addAComponentYAlignment(mergedFileLabel, mergedFilePanel);
		addAComponentYAlignment(mergedFileTextField, mergedFilePanel);
		addAComponentYAlignment(mergedFileBrowseButton, mergedFilePanel);

		saveProfilePanel.setLayout(new BoxLayout(saveProfilePanel, BoxLayout.X_AXIS));

		//Add four lines in which the file paths will be loaded. 		
		addAComponentXAlignment(saveProfileLabel, saveProfilePanel);
		addAComponentXAlignment(saveProfileComboBox, saveProfilePanel);
		saveProfileComboBox.setEditable(true);
		//look at this to complete:
		//http://72.5.124.55/docs/books/tutorial/uiswing/components/combobox.html#editable
		addAComponentXAlignment(addProfilePlusButton, saveProfilePanel);
		addAComponentXAlignment(removeProfileMinusButton, saveProfilePanel);

		mergeOptionPanel.setLayout(new BoxLayout(mergeOptionPanel, BoxLayout.Y_AXIS));

		//make a grid for four comboboxes.		
		addAComponentYAlignment(topLinePanel, mergeOptionPanel);
		addAComponentYAlignment(bottomLinePanel, mergeOptionPanel);

		topLinePanel.setLayout(new BoxLayout(topLinePanel, BoxLayout.X_AXIS));

		//add two option		
		addAComponentXAlignment(updateIDsLabel, topLinePanel);
		addAComponentXAlignment(updateIDsCombobox, topLinePanel);
		addAComponentXAlignment(outputFileFormatLabel, topLinePanel);
		addAComponentXAlignment(outputFileFormatCombobox, topLinePanel);

		bottomLinePanel.setLayout(new BoxLayout(bottomLinePanel, BoxLayout.X_AXIS));

		//add other two options	
		addAComponentXAlignment(failOnClashLabel, bottomLinePanel);
		addAComponentXAlignment(failOnClashCombobox, bottomLinePanel);
		addAComponentXAlignment(ignoreClashOnIDsLabel, bottomLinePanel);
		addAComponentXAlignment(ignoreClashOnIDsTextField, bottomLinePanel);


//		mainGUIPanel.add(Box.createVerticalGlue());

//		addAComponentYAlignment(liveFileLabel, parentFilePanel);
//		addAComponentYAlignment(branchFileLabel, parentFilePanel);
//		addAComponentYAlignment(mergedFileLabel, parentFilePanel);

	}


	private static void addAComponentXAlignment(JComponent componentName, Container container) {
//		componentName.setAlignmentX(Component.CENTER_ALIGNMENT);
		container.add(componentName);

	}
	private static void addAComponentYAlignment(JComponent componentName, Container container) {
//		componentName.setAlignmentY(Component.CENTER_ALIGNMENT);
		container.add(componentName);

	}


	//









	private static void addAPanel(JPanel panelName, Container container) {
//		JPanel panel = new JPanel();
		panelName.setAlignmentX(Component.CENTER_ALIGNMENT);
		container.add(panelName);
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

