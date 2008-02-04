package org.oboedit.launcher;

import org.bbop.dataadapter.*;
import org.bbop.framework.CheckMemoryThread;
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

public class OBOEdit {

	/**
	 * Creates a TagSpec object for the command line.
	 */
	private static TagSpec getCommandLineSpec(DataAdapterRegistry registry) {

		TagSpec loadAdapterSpec = CommandLineWidget.getTagSpec(registry,
				new OBOFileAdapter(), OBOAdapter.READ_ONTOLOGY, null);
		TagSpec vSpec = new TagSpec("-v");
		TagSpec verboseSpec = new TagSpec("-verbose");
		TagSpec listSpec = new TagSpec("--listadapters");
		TagSpec helpSpec = new TagSpec("-help");
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
		spec.addArgumentSpec(helpSpec, 1);
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
			if (tag.getName().equals("-help")) {
			    printUsage();
			    System.exit(0);
			}
			else if (tag.getName().equals("-load")) {
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
					    System.err.println("Available load adapters...");
					    printAdapters();
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
			System.out.println("Loading file " + loadTag);
			OBOSession o = (OBOSession) CommandLineWidget.execute(registry,
					OBOAdapter.READ_ONTOLOGY, loadTag, null);
			actions.setLoadMe(o);
			System.err.println("done");
		}

		return actions;
	}

    public static String getAppName() {
	return "OBO-Edit2";
    }

	/**
	 * Initialize some things, read the command line and start
	 */
	public static void main(final String[] args) throws Exception {
		Runnable r = new Runnable() {
			public void run() {
				try {
					// GUIManager.getManager().addStartupTask(
					// new BasicInitStartupTask());
					// GUIManager.getManager().addStartupTask(
					// new PluginInitStartupTask());
					GUIManager.getManager().addStartupTask(
							new DefaultGUIStartupTask());
					GUIManager.getManager().start();

					System.err.println("Starting " + getAppName() + " "
							   + Preferences.getVersion() + ": " + (new Date()));

					DataAdapterRegistry registry = IOManager.getManager()
							.getAdapterRegistry();

					TagSpec spec = getCommandLineSpec(registry);

					Tag topLevel = CommandLineParser.parse(spec, args);

					CommandLineActions actions = getActions(registry, topLevel);

					if (actions.getLoadMe() != null)
						SessionManager.getManager().setSession(
								actions.getLoadMe());
					
					// Also start thread to check free memory
					CheckMemoryThread cmt = new CheckMemoryThread();
					cmt.start();
				} catch (Throwable ex) {
					ex.printStackTrace();
				}
			}
		};
		System.setProperty("com.apple.mrj.application.apple.menu.about.name", getAppName());

		SwingUtilities.invokeAndWait(r);
	}
	public static void printUsage() {
	    System.err.println("OBO-Edit supports the following command-line options:\n -verbose - Displays verbose status messages while OBO-Edit is running\n --listadapters - Lists all the available data adapters and exits\n -load <adapter name> <file name> (default) - Loads a file on startup.\nThis parameter is the default parameter, meaning that it is implicit, and does not need to be specified.\nIf no adapter name is provided, -OBO_EDIT:OBO_Adapter is assumed.\nHence, if you want to load an OBO file, all you need to provide is the file name with no other arguments.\nBy default, the following adapters are available:");
	    printAdapters();
	    System.err.println();
	}

    private static void printAdapters() {
	DataAdapterRegistry registry = IOManager.getManager().getAdapterRegistry();
	Class[] classes = { ParameterUI.class };
	DataAdapter[] adapters = DataAdapterUtil.getAdapters(
	    registry, OBOAdapter.READ_ONTOLOGY, classes);
	for (int i = 0; i < adapters.length; i++)
	    System.err.println("   -" + adapters[i].getID());
    }

}
