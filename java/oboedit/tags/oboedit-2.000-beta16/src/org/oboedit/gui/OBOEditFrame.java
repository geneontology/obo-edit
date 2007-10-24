package org.oboedit.gui;

import javax.help.CSH;
import javax.help.HelpBroker;
import javax.help.HelpSet;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.awt.event.*;
import java.io.File;
import java.util.*;

import org.bbop.dataadapter.*;
import org.bbop.framework.DockPanelFactory;
import org.bbop.framework.GUIManager;
import org.bbop.framework.MainFrame;
import org.bbop.framework.GUIComponent;
import org.bbop.swing.*;
import org.obo.dataadapter.*;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.history.HistoryList;
import org.obo.history.SessionHistoryList;
import org.obo.identifier.DefaultIDGenerator;
import org.obo.identifier.IDGenerator;
import org.oboedit.controller.EditActionManager;
import org.oboedit.controller.IDManager;
import org.oboedit.controller.IOManager;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.event.*;
import org.oboedit.gui.widget.SplashScreen;

public class OBOEditFrame extends MainFrame {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	JMenu changeMenu;

	JMenu helpMenu;

	JMenu editMenu;

	JMenuItem undoItem;

	JMenuItem redoItem;

	JMenuItem changeDefaultNamespaceItem;

	JMenuItem newItem;

	JMenuItem loadItem;

	JMenuItem saveItem;

	JMenuItem saveAsItem;

	JMenuItem saveHistoryItem;

	JMenuItem applyHistoryItem;

	JMenuItem importItem;

	JMenuItem exportItem;

	JMenuItem exitItem;

	XMLLayout xmllayout;

	protected HelpBroker helpBroker;

	public HelpBroker getHelpBroker() {
		if (helpBroker == null) {
			HelpSet hs;
			File docsDir = new File(Preferences.getInstallationDirectory(),
					"docs/OBO-Edit.hs");

			try {
				hs = new HelpSet(null, docsDir.toURL());
			} catch (Exception ee) {
				System.out.println("HelpSet " + ee.getMessage());
				System.out.println("HelpSet " + docsDir + " not found");
				return null;
			}
			helpBroker = hs.createHelpBroker();
		}

		return helpBroker;
	}

	protected void changeDefaultNamespace(Namespace ns) {
		SessionManager.getManager().getSession().setDefaultNamespace(ns);
	}

	public OBOEditFrame() {
		super("OBO-Edit version " + Preferences.getVersion());
		setConfirmOnExit(Preferences.getPreferences().getConfirmOnExit());
	}

	@Override
	protected void addMenus() {
		addMenu(buildFileMenu());
		editMenu = buildEditMenu();
		addMenu(editMenu);
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

	public static interface NamespaceChanger {
		public void changeNamespace(Namespace ns);

		public boolean isChosen(Namespace ns);

		public boolean isEnabled(Namespace ns);

		public boolean isMenuEnabled();
	}

	public static JMenu getNamespaceChangeMenu(String title,
			final boolean noNamespaceAllowed, final NamespaceChanger nc) {
		final JMenu changeNamespace = new JMenu(title);
		changeNamespace.addMenuListener(new MenuListener() {
			public void menuSelected(MenuEvent e) {
				changeNamespace.setEnabled(nc.isMenuEnabled());
				changeNamespace.removeAll();
				if (noNamespaceAllowed) {
					final Namespace ns = null;
					boolean setBold = nc.isChosen(ns);

					JMenuItem item = new JMenuItem("<no namespace>");
					if (setBold)
						item.setFont(item.getFont().deriveFont(Font.BOLD));
					item.addActionListener(new ActionListener() {
						public void actionPerformed(ActionEvent e) {
							nc.changeNamespace(ns);
						}
					});
					item.setEnabled(nc.isEnabled(ns));
					changeNamespace.add(item);
				}
				Iterator it = SessionManager.getManager().getSession()
						.getNamespaces().iterator();
				while (it.hasNext()) {
					final Namespace ns = (Namespace) it.next();
					boolean setBold = nc.isChosen(ns);

					JMenuItem item = new JMenuItem(ns.toString());
					if (setBold)
						item.setFont(item.getFont().deriveFont(Font.BOLD));
					item.addActionListener(new ActionListener() {
						public void actionPerformed(ActionEvent e) {
							nc.changeNamespace(ns);
						}
					});
					item.setEnabled(nc.isEnabled(ns));
					changeNamespace.add(item);
				}
			}

			public void menuDeselected(MenuEvent e) {
			}

			public void menuCanceled(MenuEvent e) {
			}
		});
		return changeNamespace;
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
			gac.setHistoryPath(Preferences.getPreferences()
					.getHistoryFilePath());
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
			gac.setHistoryPath(Preferences.getPreferences()
					.getHistoryFilePath());
			boolean worked = gac.showDialog("Load history", this);
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

	public JMenu buildFileMenu() {
		JMenu fileMenu = new JMenu("File");

		JMenuItem newItem = new JMenuItem("New Ontology");
		JMenuItem loadItem = new JMenuItem("Load Terms...");
		saveItem = new JMenuItem("Save");
		JMenuItem saveAsItem = new JMenuItem("Save As...");

		JMenuItem saveHistoryItem = new JMenuItem("Save History...");
		JMenuItem applyHistoryItem = new JMenuItem("Load History...");
		importItem = new JMenuItem("Import Terms...");
		exportItem = new JMenuItem("Export Terms...");
		exitItem = new JMenuItem("Exit");

		loadItem.setAccelerator(KeyStroke.getKeyStroke("control O"));
		saveItem.setAccelerator(KeyStroke.getKeyStroke("control S"));

		fileMenu.add(newItem);
		fileMenu.add(loadItem);

		// removed until the correct behavior is figured out
		// fileMenu.add(saveItem);

		fileMenu.add(saveAsItem);
		fileMenu.addSeparator();
		fileMenu.add(applyHistoryItem);
		fileMenu.add(saveHistoryItem);
		fileMenu.addSeparator();
		// removed until the correct behavior is figured out
		/*
		 * fileMenu.add(importItem); fileMenu.add(importAndTrimItem);
		 * fileMenu.add(exportItem); fileMenu.addSeparator();
		 */
		fileMenu.add(exitItem);

		newItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				newOntology();
			}
		});

		loadItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				IOManager.getManager().load();
			}
		});

		saveItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				System.err.println("Not currently implemented");
			}
		});

		saveAsItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				IOManager.getManager().saveAs();
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
				if (!Preferences.getPreferences().getConfirmOnExit()
						|| JOptionPane.showConfirmDialog(OBOEditFrame.this,
								"Really quit OBO-Edit?", "Exit?",
								JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION)
					GUIManager.exit(0);

			}
		});
		fileMenu.addMenuListener(new FileMenuListener(this));
		return fileMenu;
	}

	public JMenu buildTypeChangeMenu() {
		JMenu changeMenu = new JMenu("Change relationship type to");
		return changeMenu;
	}

	public JMenu buildHelpMenu() {
		JMenu helpMenu = new JMenu("Help");
		JMenuItem helpItem = new JMenuItem("User Guide");

		if (getHelpBroker() != null) {
			helpItem.addActionListener(new CSH.DisplayHelpFromSource(
					getHelpBroker()));
		} else {
			helpItem.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					JOptionPane.showMessageDialog(OBOEditFrame.this,
							"No help files found!");
				}
			});
		}
		/*
		 * helpItem.addActionListener(new ActionListener() { public void
		 * actionPerformed(ActionEvent e) { showHelpMenu(); } });
		 */
		JMenuItem aboutItem = new JMenuItem("About");
		aboutItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				final JDialog aboutDialog = new JDialog(OBOEditFrame.this,
						"About OBO-Edit");
				BackgroundImagePanel bip = SplashScreen.getSplashPanel();
				bip.addMouseListener(new MouseAdapter() {
					@Override
					public void mousePressed(MouseEvent e) {
						aboutDialog.dispose();
					}
				});
				bip.setMaximumSize(new Dimension(400, 400));
				bip.setMinimumSize(new Dimension(400, 400));
				bip.setPreferredSize(new Dimension(400, 400));
				aboutDialog.setResizable(false);
				aboutDialog
						.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
				aboutDialog.setContentPane(bip);
				aboutDialog.pack();
				SwingUtil.center(aboutDialog);
				aboutDialog.setVisible(true);
			}
		});
		helpMenu.add(helpItem);
		helpMenu.add(aboutItem);
		return helpMenu;
	}

	public JMenu buildEditMenu() {
		JMenu editMenu = new JMenu("Edit");
		buildEditMenu(editMenu);
		editMenu.addMenuListener(new EditMenuListener(this));
		return editMenu;
	}

	public void buildEditMenu(JMenu editMenu) {
		undoItem = new JMenuItem("Undo");
		undoItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Z, Toolkit
				.getDefaultToolkit().getMenuShortcutKeyMask()));
		redoItem = new JMenuItem("Redo");
		redoItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Z, Toolkit
				.getDefaultToolkit().getMenuShortcutKeyMask()
				| KeyEvent.SHIFT_DOWN_MASK));

		changeDefaultNamespaceItem = getNamespaceChangeMenu(
				"Set default namespace ", false, new NamespaceChanger() {
					public void changeNamespace(Namespace ns) {
						changeDefaultNamespace(ns);
					}

					public boolean isChosen(Namespace ns) {
						return ns.equals(SessionManager.getManager()
								.getSession().getDefaultNamespace());
					}

					public boolean isEnabled(Namespace ns) {
						return true;
					}

					public boolean isMenuEnabled() {
						return true;
					}
				});

		editMenu.removeAll();
		editMenu.add(undoItem);
		editMenu.add(redoItem);
		editMenu.addSeparator();

		DefaultInputHandler.buildMenu(editMenu, EditActionManager.getManager()
				.getEditActions(), false);

		editMenu.addSeparator();
		editMenu.add(changeDefaultNamespaceItem);

		try {

			undoItem.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					undo();
				}
			});

			redoItem.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					redo();
				}
			});

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public void formatFileMenu() {
		saveItem.setEnabled(false);

		TreePath[] paths = SelectionManager.getGlobalSelection().getPaths();
		importItem.setEnabled(paths.length == 1);
		exportItem.setEnabled(paths.length == 1);
	}

	public void formatEditMenu() {

		buildEditMenu(editMenu);

		if (SessionManager.getManager().canUndo()) {

			undoItem.setText("Undo "
					+ SessionManager.getManager().getUndoItem().getShortName());
			undoItem.setEnabled(true);
		} else {
			undoItem.setText("Undo");
			undoItem.setEnabled(false);
		}
		if (SessionManager.getManager().canRedo()) {
			redoItem.setText("Redo "
					+ SessionManager.getManager().getRedoItem().getShortName());
			redoItem.setEnabled(true);
		} else {
			redoItem.setText("Redo");
			redoItem.setEnabled(false);
		}
	}

	public void undo() {
		SessionManager.getManager().undo();
		// listEditor.refresh();
		repaint();
	}

	public void redo() {
		SessionManager.getManager().redo();
		// listEditor.refresh();
		repaint();
	}

	protected void createListeners() {
		SessionManager.getManager().addRootChangeListener(
				new RootChangeListener() {

					public void changeRoot(RootChangeEvent e) {
						updateFrameTitle();
						repaint();
					}

				});
		Preferences.getPreferences().addReconfigListener(
				new ReconfigListener() {

					public void configReloaded(ReconfigEvent e) {
						setConfirmOnExit(Preferences.getPreferences()
								.getConfirmOnExit());
					}

				});
	}

	protected void updateFrameTitle() {
		setTitle(getFrameTitle());
	}

	protected String getFrameTitle() {
		String out = "OBO-Edit version " + Preferences.getVersion();
		OBOSession session = SessionManager.getManager().getSession();
		if (session != null && session.getLoadRemark() != null
				&& session.getLoadRemark().length() > 0)
			return out + ": " + session.getLoadRemark();
		else
			return out;
	}

}

class EditMenuListener implements MenuListener {
	OBOEditFrame frame;

	public EditMenuListener(OBOEditFrame frame) {
		this.frame = frame;
	}

	public void menuSelected(MenuEvent e) {
		frame.formatEditMenu();
	}

	public void menuCanceled(MenuEvent e) {
	}

	public void menuDeselected(MenuEvent e) {
	}
}

class FileMenuListener implements MenuListener {
	OBOEditFrame frame;

	public FileMenuListener(OBOEditFrame frame) {
		this.frame = frame;
	}

	public void menuSelected(MenuEvent e) {
		frame.formatFileMenu();
	}

	public void menuCanceled(MenuEvent e) {
	}

	public void menuDeselected(MenuEvent e) {
	}
}
