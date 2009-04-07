package org.oboedit.launcher;

import java.util.Date;
import java.util.Iterator;
import java.util.Properties;
import java.util.Vector;

import javax.swing.SwingUtilities;

import org.apache.log4j.Logger;
import org.apache.log4j.PropertyConfigurator;
import org.bbop.dataadapter.CommandLineWidget;
import org.bbop.dataadapter.DataAdapter;
import org.bbop.dataadapter.DataAdapterException;
import org.bbop.dataadapter.DataAdapterRegistry;
import org.bbop.dataadapter.DataAdapterUIException;
import org.bbop.dataadapter.DataAdapterUtil;
import org.bbop.dataadapter.ParameterUI;
import org.bbop.framework.CheckMemoryThread;
import org.bbop.framework.GUIManager;
import org.bbop.framework.IOManager;
import org.bbop.util.CommandLineParser;
import org.bbop.util.Tag;
import org.bbop.util.TagSpec;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.OBOSession;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.tasks.DefaultGUIStartupTask;

public class OBOEdit {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBOEdit.class);

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
		TagSpec helpSpec2 = new TagSpec("--help"); // synonym for -help
		TagSpec helpSpec3 = new TagSpec("-usage"); // synonym for -help
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
		spec.addArgumentSpec(helpSpec2, 1);
		spec.addArgumentSpec(helpSpec3, 1);
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
			Tag topLevel,
			String[] args) throws DataAdapterException, DataAdapterUIException {

		Class[] classes = { ParameterUI.class };

		CommandLineActions actions = new CommandLineActions();

		Tag loadTag = null;
		boolean verbose = false;

		// I can't figure out how to fix this argument parsing so that
		// it can load a file specified on the command line, and that
		// problem was annoying me, so I'm circumventing the weird
		// argument parsing and checking whether the first argument
		// appears to be an OBO file, and if so, parsing it.
		// THIS IS A KLUDGE.
		if (args.length == 1 && !(args[0].startsWith("-")) && args[0].endsWith(".obo")) {
			String path = args[0];
			OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
			config.getReadPaths().add(path);
			String oboVersion = "OBO_1_2";  // current default
			logger.info("Assuming " + path + " is an obo file of obo version " + oboVersion);
			config.setSerializer(oboVersion);
			OBOFileAdapter adapter = new OBOFileAdapter();
			OBOSession o = (OBOSession) adapter.doOperation(OBOAdapter.READ_ONTOLOGY, config, null);
			actions.setLoadMe(o);
			return actions;
		}

		Iterator it = topLevel.getArguments().iterator();
		while (it.hasNext()) {
			Tag tag = (Tag) it.next();
//			logger.info("getActions: tag = " + tag.getName() + ", args = " + tag.getArguments()); // DEL
			if (tag.getName().endsWith("-help") ||
					tag.getName().endsWith("-usage")) {
				printUsage();
				System.exit(0);
			}
			else if (tag.getName().equals("-load")) {
				Iterator it2 = tag.getArguments().iterator();
//				logger.info("getActions: tag is load, args = " + tag.getArguments()); // DEL
				while (it2.hasNext()) {
					Tag t = (Tag) it2.next();
//					logger.info("t = " + t.getName() + ", t.args = " + t.getArguments()); // DEL
					if (t.getName().equals("-adapter") && t.getArguments().size() >= 1) {
						if (loadTag != null)
							throw new DataAdapterUIException("Can't do two "
									+ "load commands " + "at once.");
						loadTag = t;

						break;
					} else if (t.getName().equals("--listadapters")) {
						logger.info("Available load adapters...");
						printAdapters();

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
			logger.info("Loading file " + loadTag);
			OBOSession o = (OBOSession) CommandLineWidget.execute(registry,
					OBOAdapter.READ_ONTOLOGY, loadTag, null);
			actions.setLoadMe(o);
			logger.info("done");
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
					long time = System.currentTimeMillis(); // DEL
					// Figure out where logfile will go before starting GUI (so Configuration Manager can
					// show the logfile path)
					String configDir = Preferences.getOBOEditPrefsDir().toString();
					String logFile = configDir + "/log/oboedit.log";
					Preferences.getPreferences().setLogfile(logFile);
					Preferences.setBatchMode(false);  // we're running with a GUI, not in batch mode

					// Start GUI
					GUIManager.getManager().addStartupTask(
							new DefaultGUIStartupTask());
					GUIManager.getManager().start();

					// Configure logging
					PropertyConfigurator.configure("log4j.properties");
					logger.info("Starting " + getAppName() + " "
							+ Preferences.getVersion() + ": " + (new Date()));

					logger.info("Config directory: " + Preferences.getOBOEditPrefsDir());
					logger.info("Saving logfile to " + logFile);

					// Set up Data Adapter registry
					DataAdapterRegistry registry = IOManager.getManager()
					.getAdapterRegistry();

					TagSpec spec = getCommandLineSpec(registry);
					Tag topLevel = CommandLineParser.parse(spec, args);
					CommandLineActions actions = getActions(registry, topLevel, args);  // save args for manual parsing

					if (actions.getLoadMe() != null)
						SessionManager.getManager().setSession(
								actions.getLoadMe());

					// Also start thread to check free memory
					CheckMemoryThread cmt = new CheckMemoryThread();
					cmt.start();
//					splash.dispose();
					logger.info("Loading took " + (System.currentTimeMillis() - time) + " ms"); // DEL
				} catch (Throwable ex) {
					ex.printStackTrace();
				}
			}
		};

//		// Put up splash screen
//		// This is not working--it doesn't seem to be able to show the contents of the splash screen
//		// until the main OE panel is already displayed, at which point it's useless.
//		JDialog splash = new JDialog();
//		splash.setTitle(getAppName() + " " + Preferences.getVersion());
//		BackgroundImagePanel bip = (new SplashScreen()).getSplashPanel();
//		bip.setMaximumSize(new Dimension(400, 400));
//		bip.setMinimumSize(new Dimension(400, 400));
//		bip.setPreferredSize(new Dimension(400, 400));
//		//		splash.setContentPane(bip);
//		JPanel contentPane = new JPanel(); // del
//		splash.setContentPane(contentPane); // del
//		contentPane.add(new JLabel("this is a test")); // del
//		contentPane.repaint(); // DEL
//		//		bip = new BackgroundImagePanel(); // DEL
//		//		bip.add(new JLabel("this is a test")); // del
//		//		bip.repaint();
//		splash.pack();
//		logger.debug("Displaying splash screen"); // DEL
//		SwingUtil.center(splash);
//		splash.setVisible(true);

		System.setProperty("com.apple.mrj.application.apple.menu.about.name", getAppName());
		SwingUtilities.invokeAndWait(r);
	}

	public static void printUsage() {
		logger.info("OBO-Edit supports the following command-line options:\n -help (or --help or -usage) - Print this usage message and exit\n -verbose - Displays verbose status messages while OBO-Edit is running\n --listadapters - Lists all the available data adapters and exits\n -load <adapter name> <file name> (default) - Loads a file on startup.\nThis parameter is the default parameter, meaning that it is implicit, and does not need to be specified.\nIf no adapter name is provided, -OBO_EDIT:OBO_Adapter is assumed.\nHence, if you want to load an OBO file, all you need to provide is the file name\nwith no other arguments, e.g., oboedit test_resources/camphor_catabolism.obo\n(The obo file is assumed to be in the default obo version of OBO_1_2).\n The following adapters are available:");
		printAdapters();

	}

	private static void printAdapters() {
		DataAdapterRegistry registry = IOManager.getManager().getAdapterRegistry();
		Class[] classes = { ParameterUI.class };
		DataAdapter[] adapters = DataAdapterUtil.getAdapters(
				registry, OBOAdapter.READ_ONTOLOGY, classes);
		for (int i = 0; i < adapters.length; i++)
			logger.info("   -" + adapters[i].getID());
	}

}
