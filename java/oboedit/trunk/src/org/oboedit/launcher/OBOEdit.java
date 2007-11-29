package org.oboedit.launcher;

import org.bbop.dataadapter.*;
import org.bbop.framework.GUIManager;
import org.bbop.framework.IOManager;
import org.bbop.io.AuditedPrintStream;
import org.bbop.io.MultiPrintStream;
import org.bbop.swing.*;
import org.bbop.util.*;
import java.util.*;
import java.io.*;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.AbstractAction;
import javax.swing.SwingUtilities;

import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.*;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.gui.tasks.DefaultGUIStartupTask;
import org.oboedit.gui.menu.OEHelpMenu;
import org.oboedit.gui.menu.FileMenu;

import org.simplericity.macify.eawt.*;  // Lets us control top-level menu on Macs (w/o affecting other platforms)

public class OBOEdit {

	/**
	 * Creates a TagSpec object for the command line.
	 */
	private static TagSpec getCommandLineSpec(DataAdapterRegistry registry) {

		TagSpec loadAdapterSpec = CommandLineWidget.getTagSpec(registry,
				new OBOFileAdapter(),
				OBOAdapter.READ_ONTOLOGY, null);
		TagSpec vSpec = new TagSpec("-v");
		TagSpec verboseSpec = new TagSpec("-verbose");
		TagSpec listSpec = new TagSpec("--listadapters");

		TagSpec loadSpec = new TagSpec("-load");
		loadSpec.addArgumentSpec(loadAdapterSpec, 1);
		loadSpec.setImpliedSpec(loadAdapterSpec, 1);
		loadSpec.addArgumentSpec(listSpec, 1);

		/*
		 * TagSpec applyAdapterSpec = CommandLineWidget.getTagSpec(registry,
		 * defaultWriteAdapter, OBOEditAdapter.WRITE_ONTOLOGY);
		 * 
		 * TagSpec applySpec = new TagSpec("-apply");
		 * applySpec.addArgumentSpec(applyAdapterSpec, 1);
		 * applySpec.setImpliedSpec(applyAdapterSpec, 1);
		 * 
		 * loadSpec.addArgumentSpec(applySpec, -1);
		 */

		TagSpec spec = new TagSpec();
		spec.setImpliedSpec(loadSpec, 1);
		spec.addArgumentSpec(loadSpec, 1);
		spec.addArgumentSpec(vSpec, 1);
		spec.addArgumentSpec(verboseSpec, 1);
		return spec;
	}

	/**
	 * CommandLineActions basically holds an OBOSession that was specified from
	 * a file on the command line.
	 */
	private static class CommandLineActions {
		protected OBOSession loadMe;

		public CommandLineActions() {
		}

		public void setLoadMe(OBOSession loadMe) {
			this.loadMe = loadMe;
		}

		public OBOSession getLoadMe() {
			return loadMe;
		}
	}

	/**
	 * Handle the command line arguments (passed in as topLevel) and read any
	 * files
	 */
	public static CommandLineActions getActions(DataAdapterRegistry registry,
			Tag topLevel) throws DataAdapterException, DataAdapterUIException {

		Class[] classes = { ParameterUI.class };

		CommandLineActions actions = new CommandLineActions();

		Tag loadTag = null;
		boolean verbose = false;

		Iterator it = topLevel.getArguments().iterator();
		while (it.hasNext()) {
			Tag tag = (Tag) it.next();
			if (tag.getName().equals("-load")) {
				Iterator it2 = tag.getArguments().iterator();
				while (it2.hasNext()) {
					Tag t = (Tag) it2.next();
					if (t.getName().equals("-adapter")) {
						if (loadTag != null)
							throw new DataAdapterUIException("Can't do two "
									+ "load commands " + "at once.");
						loadTag = t;

						break;
					} else if (t.getName().equals("--listadapters")) {
						DataAdapter[] adapters = DataAdapterUtil.getAdapters(
								registry, OBOAdapter.READ_ONTOLOGY, classes);
						System.err.println("Available load adapters...");
						for (int i = 0; i < adapters.length; i++)
							System.err.println("   -" + adapters[i].getID());
						System.err.println();
						System.exit(0);
					}
				}
			} else if (tag.getName().equals("-v")
					|| tag.getName().equals("-verbose")) {
				verbose = true;
			}
		}
		if (loadTag != null) {
			java.util.List progressListeners = new Vector();

			// Actually read in the command line file
			OBOSession o = (OBOSession) CommandLineWidget.execute(registry,
					OBOAdapter.READ_ONTOLOGY, loadTag, null);
			actions.setLoadMe(o);
			System.err.println("done");
		}

		return actions;
	}

	/**
	 * Initialize some things, read the command line and start
	 */
	public static void main(final String[] args) throws Exception {
		Runnable r = new Runnable() {
			public void run() {
				try {
//					GUIManager.getManager().addStartupTask(
//							new BasicInitStartupTask());
//					GUIManager.getManager().addStartupTask(
//							new PluginInitStartupTask());
					GUIManager.getManager().addStartupTask(
							new DefaultGUIStartupTask());
					GUIManager.getManager().start();

					System.err.println("Starting OBO-Edit "
							+ Preferences.getVersion() + ": " + (new Date()));

					DataAdapterRegistry registry = IOManager.getManager()
							.getAdapterRegistry();

					TagSpec spec = getCommandLineSpec(registry);

					Tag topLevel = CommandLineParser.parse(spec, args);

					CommandLineActions actions = getActions(registry, topLevel);

					if (actions.getLoadMe() != null)
						SessionManager.getManager().setSession(
								actions.getLoadMe());
				} catch (Throwable ex) {
					ex.printStackTrace();
				}
			}
		};
		SwingUtilities.invokeAndWait(r);
		(new OBOEditWithMenu()).initTopLevelMenus();
	}


    /** 
     *  This internal class fixes the issue on Macs where the MacOS adds a top-level menu for
     *  each application (such as OBO-Edit) that includes menu items such as "About" and "Quit"
     *  (shortcut command-Q).  If the Mac user selected Quit from that top-level menu (or typed
     *  command-Q), OBO-Edit would instantly quit without going through the standard OBO-Edit
     *  exit method (asking "Really quit?").
     *  This class uses an Open Source library, org.simplericity.macify.eawt, that lets us
     *  control that top-level menu on Macs without requiring any Mac-specific code that would
     *  cause runtime or compile-time problems on non-Macs.
     */
    public static class OBOEditWithMenu implements ApplicationListener {
	private AboutAction aboutAction;
	private ExitAction exitAction;
	private Application application = new DefaultApplication();
	
	public void initTopLevelMenus() {
	    application.addApplicationListener(this);
	    aboutAction = new AboutAction("About");
	    exitAction = new ExitAction("Exit");
	}

	public void handleAbout(ApplicationEvent event) {
	    aboutAction.actionPerformed(null);
	    event.setHandled(true);
	}

	public void handleQuit(ApplicationEvent event) {
	    exitAction.actionPerformed(null);
	}

	/* These have to be declared even if we're not going to do anything with them */
	public void handleOpenApplication(ApplicationEvent event) {
	}

	public void handleOpenFile(ApplicationEvent event) {
	}

	public void handlePreferences(ApplicationEvent event) {
	}

	public void handlePrintFile(ApplicationEvent event) {
	}

	public void handleReopenApplication(ApplicationEvent event) {
	}

	class AboutAction extends AbstractAction {
	    public AboutAction(String title) {
		super(title);
	    }

	    public void actionPerformed(ActionEvent actionEvent) {
		(new OEHelpMenu()).showAboutFrame();
	    }
	}

	class ExitAction extends AbstractAction {
	    public ExitAction(String title) {
		super(title);
	    }

	    public void actionPerformed(ActionEvent actionEvent) {
		(new FileMenu()).askQuit();
	    }
	}
    }
}
