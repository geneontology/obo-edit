package org.oboedit.gui.components.treeView;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;

import org.apache.log4j.Logger;
import org.bbop.framework.ConfigurationPanel;
import org.bbop.framework.GUIComponent;
import org.bbop.swing.BackgroundEventQueue;
import org.bbop.swing.ProgressBarUpdateRunnable;



/**
  * @author John Day-Richter, Jennifer Deegan, and Nicolas Rodriguez.<br>
  * Docs by Jennifer Deegan and Nicolas Rodriguez.
  *
  * Makes the GUI for adjusting the configuration settings of the Tree Viewer.
  *
 */
public class TreeViewConfigPanel  extends ConfigurationPanel {

	private static final long serialVersionUID = 1L;

	// initialize logger
	protected final static Logger logger = Logger.getLogger(TreeViewConfigPanel.class);

	TreeViewSettings treeViewSettingsInstance;
	TreeView treeViewInstance;
	protected BackgroundEventQueue eventQueue;
	public RestrictedJTree tree;
//	protected JScrollPane pane;
	protected JLabel emptyLabel = new JLabel("No terms selected");
	protected JLabel statusLabel = new JLabel("No paths loaded");
	protected JButton configButton = new JButton("Config");
	protected JProgressBar progressBar = new JProgressBar();
	JCheckBox multiTermCheckbox = new JCheckBox("Show paths to multiple selected terms");
	JCheckBox trimPathsCheckbox = new JCheckBox("Collapse already shown paths");
	JCheckBox showNonTransitiveCheckbox = new JCheckBox("Show non-transitive paths");
	JLabel multiTermLabel = new JLabel();
	JLabel trimPathsLabel = new JLabel();
	JLabel showNonTransitiveLabel = new JLabel();
	JButton closeButton = new JButton("Close");



	/**
	 * Sets up the GUI elements of the configuration screen. 
	 * 
	 * @param treeViewInstance
	 */
	public TreeViewConfigPanel(TreeView treeViewInstance) {

		this.treeViewInstance = treeViewInstance;

		this.treeViewSettingsInstance = treeViewInstance.treeViewSettingsInstance;
		JPanel mainPanel = new JPanel();

		add(mainPanel);
		
		mainPanel.setLayout(new GridBagLayout());
		GridBagConstraints constraints = new GridBagConstraints();

		//logger.debug("TreeViewConfigPanel: constructor called, building GUI.");



		constraints.anchor = GridBagConstraints.LINE_START;
		constraints.gridx = 0;
		constraints.gridy = 0;
		mainPanel.add(multiTermCheckbox, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		//mainPanel.add(trimPathsCheckbox, constraints); //Uncomment this to bring the option back. 
		constraints.gridx = 0;
		constraints.gridy = 2;
		mainPanel.add(showNonTransitiveCheckbox, constraints);
		constraints.gridx = 1;
		constraints.gridy = 0;
		mainPanel.add(multiTermLabel, constraints);
		constraints.gridx = 1;
		constraints.gridy = 1;		
		//mainPanel.add(trimPathsLabel, constraints);   //Uncomment this to bring the option back. 
		constraints.gridx = 1;
		constraints.gridy = 2;
		mainPanel.add(showNonTransitiveLabel, constraints);
	
		

		eventQueue = new BackgroundEventQueue();
		trimPathsCheckbox.setToolTipText("Collapse parts of paths that have already been shown to greatly speed up Tree Viewer redraws.");
		multiTermCheckbox.setToolTipText("If multiple terms are selected, show paths to all of them.");
		showNonTransitiveCheckbox.setToolTipText("Show paths that contain non-transitive relationships. Enabling this option will cause the view to become very confusing in some ontologies");
		progressBar.setStringPainted(true);
		multiTermCheckbox.setOpaque(false);
		trimPathsCheckbox.setOpaque(false);
		showNonTransitiveCheckbox.setOpaque(false);
	
		multiTermCheckbox.setSelected(treeViewSettingsInstance.getMultiSelect());
		//logger.debug("TreeViewConfigurationPanel: Constructor, MultiSelect() = " + treeViewSettingsInstance.getMultiSelect());
		trimPathsCheckbox.setSelected(treeViewSettingsInstance.getTrimPaths());
		showNonTransitiveCheckbox.setSelected(treeViewSettingsInstance.getShowNonTransitive());
	
		
		//This actionListener below caused the main panel to reload every time a config checkbox was checked or unchecked,
		//but it was not necessary to reload and was wasteful so I have commented it out. 
		
//		ActionListener updateListener = new ActionListener() {
//			public void actionPerformed(ActionEvent e) {
//				TreeViewConfigPanel.this.treeViewInstance.update();
//			}
//		};
//		multiTermCheckbox.addActionListener(updateListener);
//		trimPathsCheckbox.addActionListener(updateListener);
//		showNonTransitiveCheckbox.addActionListener(updateListener);			

		eventQueue.addStartupNotifier(new ProgressBarUpdateRunnable(eventQueue, progressBar));

	}


	/**
	 * Return information about whether the component is configured to show non-transitive relationships.
	 * 
	 * @return information about whether the component is configured to show non-transitive relationships. 
	 */
	public boolean showNonTransitive() {
		logger.debug("TreeView: showNonTransitive method.");
		return showNonTransitiveCheckbox.isSelected();
	}


	
	
	/** 
	 * @see org.bbop.framework.ConfigurationPanel#commit()
	 * 
	 * Sets the configurable variables in the settings object once the settings have been chosen by the user in the 
	 * configuration panel. Commit() is run when the configuration panel is closed (by clicking tick icon)
	 * and the new settings are 'committed' for use in the component. 
	 * 
	 */
	//Overrides commit() in ConfigurationPanel, but that one has no variables, so all information needed is here.  
	@Override
	public void commit() {

		//logger.debug("TreeViewConfigPanel: commit() run.");
		//logger.debug("TreeViewConfigPanel: treeViewSettingsInstance = " + treeViewSettingsInstance);
		
		//These lines set the variables in the settings object.
		treeViewSettingsInstance.setMultiSelect(multiTermCheckbox.isSelected());
		treeViewSettingsInstance.setShowNonTransitive(showNonTransitiveCheckbox.isSelected());
		treeViewSettingsInstance.setTrimPaths(trimPathsCheckbox.isSelected());
		
		treeViewInstance.treeViewSettingsInstance = treeViewSettingsInstance;
		
		//This line updates the view in the component according to the new variables. 
		treeViewInstance.update();

		
	}

	/** (non-Javadoc)
	 * @see org.bbop.framework.ConfigurationPanel#init()
	 * 
	 * Gets the configurable variables from the settings object when the configuration window is opened.
	 * init() is run in response to clicking the wrench icon. 
	 */
	//Overrides init() in ConfigurationPanel, but that one has no variables, so all information needed is here.  
	@Override
	public void init() {

		this.treeViewSettingsInstance = treeViewInstance.treeViewSettingsInstance;
		
		//logger.debug("TreeViewConfigPanel: init() run.");
		//logger.debug("TreeViewConfigPanel, init method: variable treeViewInstance = " + treeViewInstance);

		//Gets the settings. 
		multiTermCheckbox.setSelected(treeViewSettingsInstance.getMultiSelect());
		trimPathsCheckbox.setSelected(treeViewSettingsInstance.getTrimPaths());
		showNonTransitiveCheckbox.setSelected(treeViewSettingsInstance.getShowNonTransitive());

	}

	
	

	@Override
	public GUIComponent getComponent() {
		//logger.debug("Config panel New : getComponent.");
		return treeViewInstance;
	}	

	@Override
	public void setComponent(GUIComponent comp) {
		if (comp instanceof TreeView) {
			treeViewInstance = (TreeView)comp;
			this.treeViewSettingsInstance = treeViewInstance.treeViewSettingsInstance;

			//logger.debug("TreeViewConfigPanel, setComponent method:  variable treeViewInstance = " + treeViewInstance);
			//logger.debug("TreeViewConfigPanel: treeViewSettingsInstance = " + treeViewSettingsInstance);
			//logger.debug("Config panel New : setComponent.");
		}
	}
}