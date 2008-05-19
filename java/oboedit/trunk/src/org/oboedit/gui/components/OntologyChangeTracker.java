package org.oboedit.gui.components;

import org.bbop.dataadapter.*;
import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.GUIManager;
import org.bbop.framework.IOManager;
import org.bbop.swing.*;
import org.bbop.util.*;
import org.obo.dataadapter.*;
import org.obo.datamodel.*;
import org.obo.history.HistoryGenerator;
import org.obo.history.HistoryList;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;

import javax.swing.*;
import javax.swing.tree.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;

import org.apache.log4j.*;

public class OntologyChangeTracker extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OntologyChangeTracker.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/*
	 * protected Set oldRoots = null; protected Set newRoots = null;
	 */
	protected OBOSession oldHistory = null;

	protected OBOSession newHistory = null;

	protected HistoryList generatedHistory = null;

	protected boolean loadedOld = false;

	protected boolean loadedNew = false;

	protected JButton loadOldRootButton = new JButton("Load ontology...");

	protected JButton currentOldRootButton = new JButton("Use current ontology");

	protected JButton loadNewRootButton = new JButton("Load ontology...");

	protected JButton currentNewRootButton = new JButton("Use current ontology");

	protected JLabel oldRootLabel = new JLabel();

	protected JLabel newRootLabel = new JLabel();

	protected JButton findChangeButton = new JButton("Find Changes");

	protected JButton saveChangesButton = new JButton("Save Changes");

	protected JTree changeTree = new JTree(new DefaultMutableTreeNode(
			"No changes loaded"));

	protected Box progressBox = Box.createVerticalBox();

	protected JProgressBar progressBar = new JProgressBar(0, 100);

	public OntologyChangeTracker(String id) {
		super(id);

		// buildGUI();
		attachListeners();
	}

	@Override
	public void init() {
		buildGUI();
		update();
	}

	public void buildGUI() {
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		saveChangesButton.setEnabled(false);
		JScrollPane changeTreePane = new JScrollPane(changeTree,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

		changeTree.setEnabled(false);
		changeTree.putClientProperty("JTree.lineStyle", "Angled");
		DefaultTreeCellRenderer rend = new DefaultTreeCellRenderer() {
			
			@Override
			public Component getTreeCellRendererComponent(JTree tree,
					Object value, boolean sel, boolean expanded, boolean leaf,
					int row, boolean hasFocus) {
//				logger.info("painting "+value);
				return super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf,
						row, hasFocus);
			}
			@Override
			public void paint(Graphics g) {
				super.paint(g);
			}
		};
		rend.setLeafIcon(null);
		rend.setClosedIcon(null);
		rend.setOpenIcon(null);
		changeTree.setCellRenderer(rend);

		SwingUtil.setPreferredHeight(changeTreePane, 200);

		TitledBorder oldRootBorder = new TitledBorder("Old Ontology");
		TitledBorder newRootBorder = new TitledBorder("New Ontology");

		progressBar.setStringPainted(true);

		findChangeButton.setAlignmentX(LEFT_ALIGNMENT);
		saveChangesButton.setAlignmentX(LEFT_ALIGNMENT);

		Box rootBox = Box.createHorizontalBox();

		JPanel oldRootPanel = getButtonPanel(loadOldRootButton,
				currentOldRootButton, oldRootLabel);

		JPanel newRootPanel = getButtonPanel(loadNewRootButton,
				currentNewRootButton, newRootLabel);

		oldRootPanel.setBorder(oldRootBorder);
		newRootPanel.setBorder(newRootBorder);

		rootBox.add(oldRootPanel);
		rootBox.add(newRootPanel);

		Box findChangeBox = Box.createHorizontalBox();
		findChangeBox.add(Box.createHorizontalGlue());
		findChangeBox.add(findChangeButton);
		findChangeBox.add(Box.createHorizontalStrut(10));
		findChangeBox.add(saveChangesButton);
		findChangeBox.add(Box.createHorizontalGlue());

		progressBar.setValue(0);

		progressBox.add(Box.createVerticalStrut(5));
		progressBox.add(progressBar);
		progressBox.add(Box.createVerticalStrut(5));

		removeAll();
		add(rootBox);
		add(changeTreePane);
		add(findChangeBox);
		validate();
	}

	protected Runnable comparisonRunnable = new Runnable() {
		public void run() {
			Runnable dispatcher = new Runnable() {
				public void run() {
					add(progressBox);
					revalidate();
					loadOldRootButton.setEnabled(false);
					currentOldRootButton.setEnabled(false);
					loadNewRootButton.setEnabled(false);
					currentNewRootButton.setEnabled(false);
					findChangeButton.setEnabled(false);
					saveChangesButton.setEnabled(false);
					changeTree.setEnabled(false);
					paintImmediately(new Rectangle(0, 0, getWidth(),
							getHeight()));
				}
			};
			try {
				SwingUtilities.invokeLater(dispatcher);
			} catch (Exception ex) {
				ex.printStackTrace();
			}

			generatedHistory = HistoryGenerator.getHistory(oldHistory,
					newHistory, null);

			final TreeModel model = new HistoryTreeModel(generatedHistory);

			dispatcher = new Runnable() {
				public void run() {
					changeTree.setModel(model);

					loadOldRootButton.setEnabled(true);
					currentOldRootButton.setEnabled(true);
					loadNewRootButton.setEnabled(true);
					currentNewRootButton.setEnabled(true);
					findChangeButton.setEnabled(true);
					saveChangesButton.setEnabled(true);
					changeTree.setEnabled(true);

					remove(progressBox);
				}
			};
			try {
				SwingUtilities.invokeLater(dispatcher);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
	};

	public void doComparison() {
		Thread comparisonThread = new Thread(comparisonRunnable);
		comparisonThread.start();
	}

	protected void saveHistory() {
		if (generatedHistory != null) {
			try {
				DataAdapterRegistry registry = IOManager.getManager()
						.getAdapterRegistry();

				GraphicalAdapterChooser gac = new GraphicalAdapterChooser(
						registry, OBOAdapter.WRITE_HISTORY, GUIManager
								.getManager().getScreenLockQueue(), GUIManager
								.getManager().getFrame(), true,
						generatedHistory);
				gac.setHistoryPath(IOManager.getManager()
						.getHistoryFilePath());
				boolean worked = gac.showDialog("Save history", GUIManager
						.getManager().getFrame());
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		} else
			JOptionPane.showMessageDialog(GUIManager.getManager().getFrame(),
					"You cannot save until you've "
							+ "clicked the \"Find Changes\" " + "button.");
	}

	protected void attachListeners() {
		findChangeButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				doComparison();
			}
		});
		saveChangesButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				saveHistory();
			}
		});
		currentOldRootButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				oldHistory = SessionManager.getManager().getSession();
				loadedOld = false;
				update();
			}
		});
		currentNewRootButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				newHistory = SessionManager.getManager().getSession();
				loadedNew = false;
				update();
			}
		});
		loadOldRootButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					oldHistory = IOManager.getManager().doOperation(
							OBOAdapter.READ_ONTOLOGY, null, false);
				} catch (DataAdapterException e1) {
					e1.printStackTrace();
				}
				loadedOld = true;
				update();
			}
		});
		loadNewRootButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					newHistory = IOManager.getManager().doOperation(
							OBOAdapter.READ_ONTOLOGY, null, false);
					loadedNew = true;
					update();
				} catch (DataAdapterException e1) {
					e1.printStackTrace();
				}
			}
		});
	}

	protected void update() {
		if (oldHistory == null)
			oldRootLabel.setText("<no root chosen>");
		else if (oldHistory.getLoadRemark() != null)
			oldRootLabel.setText(oldHistory.getLoadRemark());
		else if (!loadedOld)
			oldRootLabel.setText("using current root");
		else
			oldRootLabel.setText("root loaded from adapter");

		if (newHistory == null)
			newRootLabel.setText("<no root chosen>");
		else if (newHistory.getLoadRemark() != null)
			newRootLabel.setText(newHistory.getLoadRemark());
		else if (!loadedNew)
			newRootLabel.setText("using current root");
		else
			newRootLabel.setText("root loaded from adapter");

		findChangeButton.setEnabled(oldHistory != null && newHistory != null);
	}

	private JPanel getButtonPanel(JButton loadButton, JButton currentButton,
			JLabel label) {

		loadButton.setAlignmentX(LEFT_ALIGNMENT);
		currentButton.setAlignmentX(LEFT_ALIGNMENT);
		label.setAlignmentX(LEFT_ALIGNMENT);

		Box labels = Box.createHorizontalBox();
		Box buttons = Box.createHorizontalBox();

		buttons.add(Box.createHorizontalGlue());
		buttons.add(loadButton);
		buttons.add(Box.createHorizontalGlue());
		buttons.add(Box.createHorizontalStrut(10));
		buttons.add(Box.createHorizontalGlue());
		buttons.add(currentButton);
		buttons.add(Box.createHorizontalGlue());

		labels.add(label);
		labels.add(Box.createHorizontalGlue());

		JPanel panel = new JPanel();
		panel.setOpaque(false);
		panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
		panel.add(labels);
		panel.add(buttons);
		return panel;
	}

	@Override
	public String getName() {
		return "Term Change Tracker Plugin";
	}
}
