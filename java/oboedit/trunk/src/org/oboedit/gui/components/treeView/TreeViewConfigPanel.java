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



public class TreeViewConfigPanel  extends ConfigurationPanel {

	private static final long serialVersionUID = 1L;

	// initialize logger
	protected final static Logger logger = Logger.getLogger(TreeViewConfigPanel.class);

	TreeViewSettings treeViewSettings;
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



	public TreeViewConfigPanel(TreeView treeViewInstance) {

		this.treeViewInstance = treeViewInstance;

		this.treeViewSettings = treeViewInstance.treeViewSettingsInstance;
		JPanel mainPanel = new JPanel();

		add(mainPanel);
		
		mainPanel.setLayout(new GridBagLayout());
		GridBagConstraints constraints = new GridBagConstraints();

		logger.debug("TreeViewConfigPanel: constructor called, building GUI.");



		constraints.anchor = GridBagConstraints.LINE_START;
		constraints.gridx = 0;
		constraints.gridy = 0;
		mainPanel.add(multiTermCheckbox, constraints);
		constraints.gridx = 0;
		constraints.gridy = 1;
		mainPanel.add(trimPathsCheckbox, constraints);
		constraints.gridx = 0;
		constraints.gridy = 2;
		mainPanel.add(showNonTransitiveCheckbox, constraints);
		constraints.gridx = 1;
		constraints.gridy = 0;
		mainPanel.add(multiTermLabel, constraints);
		constraints.gridx = 1;
		constraints.gridy = 1;		
		mainPanel.add(trimPathsLabel, constraints);
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
		ActionListener updateListener = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				TreeViewConfigPanel.this.treeViewInstance.update();
			}
		};
		multiTermCheckbox.addActionListener(updateListener);
		trimPathsCheckbox.addActionListener(updateListener);
		showNonTransitiveCheckbox.addActionListener(updateListener);			

		eventQueue.addStartupNotifier(new ProgressBarUpdateRunnable(eventQueue, progressBar));

	}


	public boolean showNonTransitive() {
		logger.debug("TreeView: showNonTransitive method.");
		return showNonTransitiveCheckbox.isSelected();
	}



	
	@Override
	public void commit() {

		logger.debug("TreeViewConfigPanel: commit() run.");
		logger.debug("TreeViewConfigPanel: treeViewSettings = " + treeViewSettings);

		
		
		treeViewSettings.setMultiSelect(multiTermCheckbox.isSelected());
		treeViewSettings.setShowNonTransitive(showNonTransitiveCheckbox.isSelected());
		treeViewSettings.setTrimPaths(trimPathsCheckbox.isSelected());
		
		treeViewInstance.treeViewSettingsInstance = treeViewSettings;
		
		treeViewInstance.update();

		
	}

	@Override
	public void init() {

		this.treeViewSettings = treeViewInstance.treeViewSettingsInstance;

		logger.debug("TreeViewConfigPanel: init() run.");
		logger.debug("TreeViewConfigPanel, init method: variable graphvizCanvasInstance = " + treeViewInstance);

		multiTermCheckbox.setSelected(treeViewSettings.getMultiSelect());
		trimPathsCheckbox.setSelected(treeViewSettings.getTrimPaths());
		showNonTransitiveCheckbox.setSelected(treeViewSettings.getShowNonTransitive());

	}

	
	

	@Override
	public GUIComponent getComponent() {
		logger.debug("Config panel New : getComponent.");
		return treeViewInstance;
	}	

	@Override
	public void setComponent(GUIComponent comp) {
		if (comp instanceof TreeView) {
			treeViewInstance = (TreeView)comp;
			this.treeViewSettings = treeViewInstance.treeViewSettingsInstance;

			logger.debug("TreeViewConfigPanel, setComponent method:  variable treeViewInstance = " + treeViewInstance);
			logger.debug("TreeViewConfigPanel: treeViewSettings = " + treeViewSettings);
			logger.debug("Config panel New : setComponent.");
		}
	}
}