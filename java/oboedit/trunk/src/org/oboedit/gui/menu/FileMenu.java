package org.oboedit.gui.menu;

import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;
import org.bbop.dataadapter.AdapterConfiguration;
import org.bbop.dataadapter.DataAdapter;
import org.bbop.dataadapter.DataAdapterException;
import org.bbop.dataadapter.DataAdapterRegistry;
import org.bbop.dataadapter.FileAdapterConfiguration;
import org.bbop.dataadapter.GraphicalAdapterChooser;
import org.bbop.framework.GUIManager;
import org.bbop.framework.IOManager;
import org.bbop.swing.DynamicMenu;
import org.obo.dataadapter.OBOAdapter;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.impl.OBOSessionImpl;
import org.obo.history.HistoryList;
import org.obo.history.SessionHistoryList;
import org.obo.identifier.DefaultIDGenerator;
import org.obo.identifier.IDGenerator;
import org.oboedit.controller.IDManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.event.ReconfigEvent;
import org.oboedit.gui.tasks.FrameNameUpdateTask;

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
                    //                saveItem.setAccelerator(KeyStroke.getKeyStroke("control S"));
		saveAsItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_W, Toolkit
								 .getDefaultToolkit().getMenuShortcutKeyMask()));
		saveItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, Toolkit
								 .getDefaultToolkit().getMenuShortcutKeyMask()));


		add(newItem);
		add(loadItem);
		add(importItem);
		add(saveItem);
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

		loadItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
                            doLoad();
                        }
		});

		saveItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
                            doSave();
                            // This is so CheckOriginalFileTask knows that we saved the file ourselves--
                            // it didn't change on disk out from under us
                            // (It's not in doSave because we can't reference "this" staticly there)
                            Preferences.getPreferences().fireReconfigEvent(new ReconfigEvent(this));
			}
		});

		saveAsItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
                            doSaveAs();
                            Preferences.getPreferences().fireReconfigEvent(new ReconfigEvent(this));
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


    /** Made this method public so it can be called by CheckOriginalFileTask to prompt user
      * to reload the file if it's changed on disk. */
            public static void doLoad() {
                if (SessionManager.getManager().needsSave()) {
                    int proceed = JOptionPane
                        .showConfirmDialog(GUIManager.getManager().getFrame(),
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
                    // How can we make it open with the last read/saved file? There's
                    // not a straightforward way to do this because of the way various doOperations
                    // are defined.
                    // (This call is to the one in IOManager.)
                    OBOSession session = IOManager.getManager().
                        doOperation(OBOAdapter.READ_ONTOLOGY, null, true);
                    if (session != null) {
                        session.setCurrentUser(Preferences.getPreferences()
                                               .getUserName());
                        SessionManager.getManager().setSession(session);
                    }
                } catch (DataAdapterException e1) {
                    e1.printStackTrace();
                }
            }

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

    public static void doSaveAs() {
        try {
            OBOSession session = IOManager.getManager().doOperation(
							OBOAdapter.WRITE_ONTOLOGY,
							SessionManager.getManager().getSession(), true);
                if (session != null) {
                    SessionManager.getManager().markChangesFlushed();
                    // When the adapter writes out the ontology, a ReconfigEvent is fired. FrameNameUpdateTask listens for this event and
                    // updates the main OE titlebar to show the name of the saved file.
                }
          } catch (DataAdapterException e1) {
                // TODO Auto-generated catch block
                e1.printStackTrace();
                }
          }

    public static void doSave() {
        // Figure out whether we can do a silent save--if user hasn't done a save yet
        // this session, then they have to do a Save As.
        OBOSession session = SessionManager.getManager().getSession();
        if (session == null) // Why would this happen?
            return;

        DataAdapter adapter = IOManager.getManager().getCurrentAdapter();
        if (adapter == null) {
	    cantDoQuickSaveYet();
	    return;
        }

        // AdapterConfiguration config = ui.getConfig(op, adapter, input);
        AdapterConfiguration config = adapter.getConfiguration();
	//	logger.debug("config = " + config); // DEL
        if (config == null || (config instanceof FileAdapterConfiguration && ((FileAdapterConfiguration)config).getWritePath() == null)) {
	    cantDoQuickSaveYet();
	    return;
        }
        try {
            Object output = adapter.doOperation(OBOAdapter.WRITE_ONTOLOGY, config, SessionManager.getManager().getSession());
            // Are we supposed to then do something with output?? Doesn't seem like it...
	    SessionManager.getManager().markChangesFlushed(); // ?
            logger.debug("Did quick-save");
        } catch (DataAdapterException e1) {
            e1.printStackTrace();
        }
    }

    private static void cantDoQuickSaveYet() {
	String message = "The first time you save in an OBO-Edit session, you need to do a Save As\nso that OBO-Edit knows how you want to set the various save options.\nAfter that, you will be able to do a quick Save (keyboard shortcut is Command-s).";
	JOptionPane.showMessageDialog(GUIManager.getManager().getFrame(),
				      message, "Can't do quick save yet", JOptionPane.WARNING_MESSAGE);
	doSaveAs();
    }

}
