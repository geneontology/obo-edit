package org.oboedit.gui.menu;

import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.JMenuItem;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import org.bbop.dataadapter.AdapterConfiguration;
import org.bbop.dataadapter.DataAdapter;
import org.bbop.dataadapter.DataAdapterException;
import org.bbop.dataadapter.DataAdapterRegistry;
import org.bbop.dataadapter.FileAdapterConfiguration;
import org.bbop.dataadapter.GraphicalAdapterChooser;
import org.bbop.framework.GUIManager;
import org.bbop.framework.IOManager;
import org.bbop.swing.BackgroundUtil;
import org.bbop.swing.DynamicMenu;
import org.bbop.swing.HTMLLabel;
import org.bbop.swing.SwingUtil;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter.OBOAdapterConfiguration;
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

	protected static ActionListener saveAsActionListener;
        protected static ActionListener saveActionListener;

        // This becomes true if user does a save as after loading the current file
        private static boolean didSaveAs = false;

	public FileMenu() {
		super("File");

		JMenuItem newItem = new JMenuItem("New Ontology");
		JMenuItem loadItem = new JMenuItem("Load Ontologies...") {
			@Override
			public KeyStroke getAccelerator() {
				// TODO Auto-generated method stub
				return super.getAccelerator();
			}
		};
		JMenuItem importItem = new JMenuItem("Import Terms...");
		JMenuItem saveAsItem = new JMenuItem("Save As...");
		JMenuItem saveItem = new JMenuItem("Save");

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

		saveActionListener = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
                            doSave();
			}
		};
		saveItem.addActionListener(saveActionListener);

		saveAsActionListener = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
			    doSaveAs();
			}
		    };
		saveAsItem.addActionListener(saveAsActionListener);

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
            didSaveAs = false;
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


    /** Made this method public so it can be called by CheckOriginalFileTask to prompt user
      * to reload the file if it's changed on disk. */
            public static void doLoad() {
                didSaveAs = false;
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
            didSaveAs = false;
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
			// This is so CheckOriginalFileTask knows that we saved the file ourselves--
			// it didn't change on disk out from under us
			// (It will also cause FrameNameUpdateTask to update the titlebar.)
			Preferences.getPreferences().fireReconfigEvent(new ReconfigEvent(saveAsActionListener));
                        didSaveAs = true;
		    }
			} catch (DataAdapterException e1) {
		// TODO Auto-generated catch block
		e1.printStackTrace();
		    }
    }

    public static void doSave() {
        // Figure out whether we can do a silent save--if user hasn't done a save yet
        // this session, then they have to do a Save As.
        if (!didSaveAs) {
	    cantDoQuickSaveYet();
	    return;
        }

        OBOSession session = SessionManager.getManager().getSession();
        if (session == null) // Why would this happen?
            return;

        DataAdapter adapter = IOManager.getManager().getCurrentAdapter();
        if (adapter == null) {
	    cantDoQuickSaveYet();
	    return;
        }

        AdapterConfiguration config = adapter.getConfiguration();
	//	logger.debug("config = " + config); // DEL
        if (config == null) {
	    cantDoQuickSaveYet();
	    return;
        }

        // Figure out if we have a valid write path yet
        if (config instanceof OBOAdapterConfiguration) {
            OBOAdapterConfiguration oboconfig = (OBOAdapterConfiguration)config;
            if ((oboconfig.getBasicSave() && (oboconfig.getWritePath() == null)) ||
                // If user saved via the OBO Advanced interface, the write path will be in SaveRecord
                (!oboconfig.getBasicSave() && (oboconfig.getSaveRecords() == null || oboconfig.getSaveRecords().size() == 0))) {
                //                logger.debug("doSave: oboconfig.getBasicSave() = " + oboconfig.getBasicSave() +
                //                             ", oboconfig.getWritePath() = " + oboconfig.getWritePath() +
                //                             ", oboconfig.getSaveRecords() = " + oboconfig.getSaveRecords()); // DEL
                cantDoQuickSaveYet();
                return;
            }
        }
        else if (config instanceof FileAdapterConfiguration) {
            FileAdapterConfiguration fileconfig = (FileAdapterConfiguration)config;
            if (fileconfig.getWritePath() == null) {
                cantDoQuickSaveYet();
                return;
            }
        }
            
        // Ok, now we can do the silent save
        try {
	    // This is to, for example, alert TextEditor that the user wants to save, so it can warn if there are uncommitted changes
	    boolean cancelled = IOManager.getManager().justFireEvents(OBOAdapter.WRITE_ONTOLOGY, SessionManager.getManager().getSession());
	    if (cancelled) {
		// logger.debug("User canceled save"); // DEL
		return; // User decided not to save
	    }

	    Object output = adapter.doOperation(OBOAdapter.WRITE_ONTOLOGY, config, SessionManager.getManager().getSession());
            if (output == null || SessionManager.getManager().getSession().getLoadRemark() == null) {
                String message = "Quick save to " + SessionManager.getManager().getSession().getLoadRemark() + " failed!";
                JOptionPane.showMessageDialog(GUIManager.getManager().getFrame(),
					      message, "Save failed", JOptionPane.ERROR_MESSAGE);
                return;
            }
	    SessionManager.getManager().markChangesFlushed();
	    Preferences.getPreferences().fireReconfigEvent(new ReconfigEvent(saveActionListener));

            showQuickSaveDoneDialog();
            logger.debug("Did quick-save to " + SessionManager.getManager().getSession().getLoadRemark());
        } catch (DataAdapterException e1) {
            e1.printStackTrace();
        }
    }

    /* Tell user that they can't do quick save yet and then do a Save As. */
    private static void cantDoQuickSaveYet() {
        String message = "The first time you save with a particular data adapter in an OBO-Edit session,\n you need to do a Save As so that OBO-Edit knows how you want to set\nthe save options.\nAfter that, you will be able to do a quick Save (keyboard shortcut is Command-s).";

        // Is this popup too annoying? For now, just send message to log (where they will never look at it, of course).
        //        JOptionPane.showMessageDialog(GUIManager.getManager().getFrame(),
        //                                      message, "Can't do quick save yet", JOptionPane.WARNING_MESSAGE);
        logger.info(message);

        // I wanted to make this message come up in a dialog window that would then
        // go away by itself (without having to click OK) after a few seconds, but this
        // proved problematic. Leaving the not-working-right code here in case I want to
        // revisit this later.
        // Thread warningThread = new Thread() {
        //         public void run() {
        //             try {
        //                 SwingUtilities.invokeAndWait(new Runnable(){
        //                         @Override
        //                             public void run(){
        //                             String message = "The first time you save in an OBO-Edit session, you need to do a Save As<br>so that OBO-Edit knows how you want to set the various save options.<br>After that, you will be able to do a quick Save (keyboard shortcut is Command-s).";
        //                             createAndShowDialog("Can't do quick save yet", message, 3000); // wait 3 seconds before disposing dialog
        //                         }
        //                     });
        //             } catch (Exception e1) { }
        //         }
        //     };
        // warningThread.run();
        doSaveAs(); // Want to do this after message is done being shown
    }

    private static void showQuickSaveDoneDialog() {
        SwingUtilities.invokeLater(new Runnable(){
                    public void run(){
                    String message=" Saved to " + SessionManager.getManager().getSession().getLoadRemark() + " ";
                    createAndShowDialog("", message, 1500); // wait 1.5 seconds before disposing dialog
                }
            });
    }

    /** Show a simple dialog with a text message that disappears by itself after specified time */
    private static void createAndShowDialog(final String title, final String content, final int time){
        final JDialog dialog = new JDialog();
        dialog.setTitle(title);

        HTMLLabel label = new HTMLLabel("<html><body bgcolor=\"#ffffff\"><p>&nbsp;&nbsp;" + content + "&nbsp;&nbsp;</p></body></html>");

        dialog.add(label);
        dialog.pack();
        SwingUtil.center(dialog);
        dialog.toFront();
        dialog.setVisible(true);

        // start timer
        Timer t = new Timer(time, new ActionListener() {
                    public void actionPerformed(ActionEvent arg0) {
                    dialog.dispose();
                }
            });
        t.setRepeats(false);
        t.start();
    }
}
