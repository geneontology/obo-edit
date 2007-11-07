package org.oboedit.gui.menu;

import java.awt.Component;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.List;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;
import javax.swing.tree.TreePath;

import org.bbop.dataadapter.DataAdapterRegistry;
import org.bbop.dataadapter.GraphicalAdapterChooser;
import org.bbop.framework.GUIManager;
import org.bbop.swing.AbstractDynamicMenuItem;
import org.bbop.swing.DynamicMenu;
import org.bbop.util.CollectionUtil;
import org.obo.dataadapter.OBOAdapter;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.impl.OBOSessionImpl;
import org.obo.history.HistoryList;
import org.obo.history.SessionHistoryList;
import org.obo.identifier.DefaultIDGenerator;
import org.obo.identifier.IDGenerator;
import org.oboedit.controller.IDManager;
import org.oboedit.controller.IOManager;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;

public class FileMenu extends DynamicMenu {

	public FileMenu() {
		super("File");
		JMenuItem saveItem;

		JMenuItem newItem = new JMenuItem("New Ontology");
		JMenuItem loadItem = new JMenuItem("Load Ontologies...");
		saveItem = new JMenuItem("Save");
		JMenuItem saveAsItem = new JMenuItem("Save As...");

		JMenuItem saveHistoryItem = new JMenuItem("Save History...");
		JMenuItem applyHistoryItem = new JMenuItem("Load History...");
		JMenuItem exitItem = new JMenuItem("Exit");

		loadItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, Toolkit
				.getDefaultToolkit().getMenuShortcutKeyMask()));
		saveItem.setAccelerator(KeyStroke.getKeyStroke("control S"));

		add(newItem);
		add(loadItem);
		// removed until the correct behavior is figured out
		// fileMenu.add(saveItem);

		add(saveAsItem);
		add(new AbstractDynamicMenuItem("import/export submenu", true, true,
				true) {

			public List<? extends Component> getItems() {
				JMenuItem importItem = new JMenuItem("Import Terms...");
				JMenuItem exportItem = new JMenuItem("Export Terms...");
				boolean enable = !SelectionManager.getGlobalSelection()
						.getTerms().isEmpty();
				importItem.setEnabled(enable);
				exportItem.setEnabled(enable);
				return CollectionUtil.list(importItem, exportItem);
			}

		});
		add(applyHistoryItem);
		add(saveHistoryItem);
		addSeparator();
		// removed until the correct behavior is figured out
		/*
		 * fileMenu.add(importItem); fileMenu.add(importAndTrimItem);
		 * fileMenu.add(exportItem); fileMenu.addSeparator();
		 */
		add(exitItem);

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
				GUIManager.exit(0);
			}
		});

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

	public JMenu buildFileMenu() {
		JMenu fileMenu = new JMenu("File");

		return fileMenu;
	}

	public JMenu buildTypeChangeMenu() {
		JMenu changeMenu = new JMenu("Change relationship type to");
		return changeMenu;
	}
}
