package org.oboedit.gui.menu;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Frame;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.tree.TreePath;

import org.bbop.dataadapter.DataAdapterException;
import org.bbop.dataadapter.DataAdapterRegistry;
import org.bbop.dataadapter.GraphicalAdapterChooser;
import org.bbop.framework.GUIManager;
import org.bbop.framework.IOManager;
import org.bbop.swing.AbstractDynamicMenuItem;
import org.bbop.swing.DynamicMenu;
import org.bbop.util.CollectionUtil;
import org.obo.dataadapter.OBOAdapter;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.impl.OBOSessionImpl;
import org.obo.history.HistoryList;
import org.obo.history.SessionHistoryList;
import org.oboedit.controller.IDManager;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;
import org.obo.identifier.DefaultIDGenerator;
import org.obo.identifier.IDGenerator;

import org.apache.log4j.*;

public class FileMenu extends DynamicMenu {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FileMenu.class);

	public FileMenu() {
		super("File");
		JMenuItem saveItem;

		JMenuItem newItem = new JMenuItem("New Ontology");
		JMenuItem loadItem = new JMenuItem("Load Ontologies...") {
			@Override
			public KeyStroke getAccelerator() {
				// TODO Auto-generated method stub
				return super.getAccelerator();
			}
		};
		JMenuItem importItem = new JMenuItem("Import Terms...");
		saveItem = new JMenuItem("Save");
		JMenuItem saveAsItem = new JMenuItem("Save As...");

		JMenuItem saveHistoryItem = new JMenuItem("Save History...");
		JMenuItem applyHistoryItem = new JMenuItem("Load History...");
		// Moved to EditMenu
//		JMenuItem resolveItem = new JMenuItem("Fix IDs...");
		JMenuItem exitItem = new JMenuItem("Quit");  // Was Exit

		loadItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, Toolkit
				.getDefaultToolkit().getMenuShortcutKeyMask()));
//		saveItem.setAccelerator(KeyStroke.getKeyStroke("control S"));
		    // For now, until we get Save implemented, 
		    // let's have command-S be a shortcut for Save As
		saveAsItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, Toolkit
								 .getDefaultToolkit().getMenuShortcutKeyMask()));


		add(newItem);
		add(loadItem);
		add(importItem);
//		add(resolveItem);
		// removed until the correct behavior is figured out
		// fileMenu.add(saveItem);

		add(saveAsItem);

		add(applyHistoryItem);
		add(saveHistoryItem);
		addSeparator();
		add(exitItem);

		newItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				newOntology();
			}
		});

		importItem.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				doImport();
			}

		});

// 		resolveItem.addActionListener(new ActionListener() {
// 
// 			public void actionPerformed(ActionEvent e) {
// 				resolveIDs();
// 			}
// 
// 		});

		loadItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (SessionManager.getManager().needsSave()) {
					int proceed = JOptionPane
							.showConfirmDialog(
									GUIManager.getManager().getFrame(),
									"There are unsaved changes to this "
											+ "ontology. If you load this file, your "
											+ "changes will be discarded. Are you sure "
											+ "you want to proceed?",
									"Unsaved changes",
									JOptionPane.YES_NO_OPTION);
					if (proceed != JOptionPane.YES_OPTION)
						return;
				}
				try {
					OBOSession session = IOManager.getManager().doOperation(
							OBOAdapter.READ_ONTOLOGY, null, true);
					if (session != null) {
						session.setCurrentUser(Preferences.getPreferences()
								.getUserName());
						SessionManager.getManager().setSession(session);
					}
				} catch (DataAdapterException e1) {
					e1.printStackTrace();
				}
			}
		});

		saveItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				logger.info("Not currently implemented");
			}
		});

		saveAsItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					OBOSession session = IOManager.getManager().doOperation(
							OBOAdapter.WRITE_ONTOLOGY,
							SessionManager.getManager().getSession(), true);
					if (session != null) {
						SessionManager.getManager().markChangesFlushed();
					}
				} catch (DataAdapterException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
			}
		});

		saveHistoryItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				saveHistory();
			}
		});
		applyHistoryItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				applyHistory();
			}
		});

		exitItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				GUIManager.exit(0);
			}
		});

	}

	protected void doImport() {
		int proceed = JOptionPane
				.showConfirmDialog(
						GUIManager.getManager().getFrame(),
						"Once you import a new ontology, the undo history will be "
								+ "reset.\nYou will be unable to undo any changes made before "
								+ "this point.\n" + " Are you sure "
								+ "you want to proceed?", "Undo warning",
						JOptionPane.YES_NO_OPTION);
		if (proceed != JOptionPane.YES_OPTION)
			return;
		try {
			OBOSession session = IOManager.getManager().doOperation(
					OBOAdapter.READ_ONTOLOGY, null, true);
			if (session != null) {
				SessionManager.getManager().getSession().importSession(session,
						true);
				SessionManager.getManager().reloadSession();
			}
		} catch (DataAdapterException e1) {
			e1.printStackTrace();
		}
	}

	// Moved to EditMenu
// 	protected boolean resolveIDs() {
// 		try {
// 			IDUtil.updateIDs(SessionManager.getManager().getSession(),
// 					new ArrayList<LinkIDResolution>(), true);
// 			return true;
// 		} catch (UnresolvedIDsException e) {
// 			ComponentManager.getManager().showComponent(
// 					new IDResolutionComponentFactory(), true);
// 			return false;
// 		}
// 	}

	public static void newOntology() {
		if (SessionManager.getManager().needsSave()) {
			int proceed = JOptionPane
					.showConfirmDialog(
							GUIManager.getManager().getFrame(),
							"There are unsaved changes to this ontology. If you create a new ontology, your changes will be discarded. Are you sure you want to proceed?",
							"Unsaved changes", JOptionPane.YES_NO_OPTION);

			if (proceed != JOptionPane.YES_OPTION)
				return;
		}

		OBOSession history = new OBOSessionImpl();
		Namespace ns = new Namespace("default_namespace", System
				.getProperty("user.home"));
		history.addNamespace(ns);
		history.setDefaultNamespace(ns);
		IDGenerator idGen = IDManager.getManager().getIDAdapter();
		if (idGen instanceof DefaultIDGenerator) {
			history.setIDProfile(((DefaultIDGenerator) idGen).getProfile());
		}
		SessionManager.getManager().setSession(history);
	}

	public static void saveHistory() {
		try {
			DataAdapterRegistry registry = IOManager.getManager()
					.getAdapterRegistry();

			SessionHistoryList historyList = SessionManager.getManager()
					.getSession().getCurrentHistory();
			historyList.setUser(Preferences.getPreferences().getUserName());
			GraphicalAdapterChooser<HistoryList, Void> gac = new GraphicalAdapterChooser<HistoryList, Void>(
					registry, OBOAdapter.WRITE_HISTORY, GUIManager.getManager()
							.getScreenLockQueue(), GUIManager.getManager()
							.getFrame(), Preferences.getPreferences()
							.getUseModalProgressMonitors(), historyList);
			gac.setHistoryPath(IOManager.getManager().getHistoryFilePath());
			gac.showDialog("Save history", GUIManager.getManager().getFrame());
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}

	public void applyHistory() {
		try {
			DataAdapterRegistry registry = IOManager.getManager()
					.getAdapterRegistry();

			GraphicalAdapterChooser<Void, List<HistoryList>> gac = new GraphicalAdapterChooser<Void, List<HistoryList>>(
					registry, OBOAdapter.READ_HISTORY, GUIManager.getManager()
							.getScreenLockQueue(), GUIManager.getManager()
							.getFrame(), Preferences.getPreferences()
							.getUseModalProgressMonitors(), null);
			gac.setHistoryPath(IOManager.getManager().getHistoryFilePath());
			boolean worked = gac.showDialog("Load history", GUIManager
					.getManager().getFrame());
			if (worked) {
				java.util.List<HistoryList> histories = gac.getResult();
				for (HistoryList historyList : histories) {
					SessionManager.getManager().applyList(historyList);
				}
			}
		} catch (Exception ex) {
			ex.printStackTrace();
		}

	}
}
