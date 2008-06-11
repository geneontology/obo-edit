package org.oboedit.launcher;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.apache.log4j.Logger;
import org.bbop.expression.ExpressionException;
import org.bbop.expression.ExpressionUtil;
import org.bbop.expression.JexlContext;
import org.bbop.io.IOUtil;
import org.obo.dataadapter.OBDSQLDatabaseAdapter;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.dataadapter.OBOSerializationEngine;
import org.obo.dataadapter.OBDSQLDatabaseAdapter.OBDSQLDatabaseAdapterConfiguration;
import org.obo.datamodel.OBOSession;
import org.obo.filters.Filter;
import org.obo.util.FilterUtil;
import org.oboedit.controller.ExpressionManager;
import org.oboedit.gui.Preferences;

public class OBO2Database {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBO2Database.class);


	protected static class ScriptWrapper {
		protected String script;

		protected LinkedList args = new LinkedList();

		public LinkedList getArgs() {
			return args;
		}

		public void setArgs(LinkedList args) {
			this.args = args;
		}

		public String getScript() {
			return script;
		}

		public void setScript(String script) {
			this.script = script;
		}

	}

	public static void convertFiles(
			OBOFileAdapter.OBOAdapterConfiguration readConfig,
			OBDSQLDatabaseAdapter.OBDSQLDatabaseAdapterConfiguration writeConfig,
			boolean parseObsoleteComments, boolean writeObsoleteComments,
			boolean fixDbxrefs, List scripts) throws Exception {
		OBOFileAdapter adapter = new OBOFileAdapter();
		OBOSession session = (OBOSession) adapter.doOperation(OBOAdapter.READ_ONTOLOGY,
				readConfig, null);
		Iterator it = scripts.iterator();
		while (it.hasNext()) {
			ScriptWrapper wrapper = (ScriptWrapper) it.next();
			runScript(session, wrapper.getScript(), wrapper.getArgs());
		}
		logger.info("About to write files..., session object count = "
				+ session.getObjects().size());
		logger.info("writePath = " + writeConfig.getWritePath());
		logger.info("savePath = " + writeConfig.getSaveRecords());
		
		OBDSQLDatabaseAdapter writer = new OBDSQLDatabaseAdapter();
		writer.doOperation(OBOAdapter.WRITE_ONTOLOGY, writeConfig, session);
	}

	public static void runScript(OBOSession session, String script, List args)
			throws ExpressionException {
		JexlContext context = ExpressionManager.getManager().getContext();
		context.setGlobalVariable("session", session, false);
		context.setLocalVariable("args", args, true);
		ExpressionUtil.exec(script, context);
	}



	public static void main(String[] args) throws Exception {
		logger.info("version = "+Preferences.getVersion());
		if (args.length == 0)
			printUsage(1);
		OBOFileAdapter.OBOAdapterConfiguration readConfig = new OBOFileAdapter.OBOAdapterConfiguration();
		readConfig.setBasicSave(false);
		OBDSQLDatabaseAdapterConfiguration writeConfig = new OBDSQLDatabaseAdapter.OBDSQLDatabaseAdapterConfiguration();
		
		writeConfig.setBasicSave(false);
		boolean parseObsoleteComments = false;
		boolean writeObsoleteComments = false;
		boolean fixDbxrefs = false;
		LinkedList scripts = new LinkedList();
		String formatVersion = "OBO_1_2";
		for (int i = 0; i < args.length; i++)
			logger.info("args[" + i + "] = |" + args[i] + "|");

		for (int i = 0; i < args.length; i++) {
			if (args[i].equals("-formatversion")) {
				if (i >= args.length - 1)
					printUsage(1);
				i++;
				formatVersion = args[i];
				if (!(formatVersion.equals("OBO_1_2") || formatVersion
						.equals("OBO_1_0")))
					printUsage(1);
			} else if (args[i].equals("-allowdangling")) {
				readConfig.setAllowDangling(true);
			} else if (args[i].equals("-runscript")) {
				if (i >= args.length - 1)
					printUsage(1);
				i++;
				String scriptFile = args[i];
				String script = IOUtil.readFile(scriptFile);
				ScriptWrapper wrapper = new ScriptWrapper();
				wrapper.setScript(script);
				for (i = i + 1; i < args.length; i++) {
					if (args[i].equals(";")) {
						break;
					}
					wrapper.getArgs().add(args[i]);
				}
				scripts.add(wrapper);
			} else if (args[i].equals("-o")) {
				if (i >= args.length - 1)
					printUsage(1);
				i++;

				OBOSerializationEngine.FilteredPath path = new OBOSerializationEngine.FilteredPath();
				path.setUseSessionReasoner(false);

				for (; i < args.length; i++) {
					if (args[i].equals("-f")) {
						if (i >= args.length - 1)
							printUsage(1);
						i++;
						String filterFile = args[i];
						Filter filter = FilterUtil.loadFilter(filterFile);

						path.setDoFilter(filter != null);
						path.setObjectFilter(filter);
					} else if (args[i].equals("-allowdangling")) {
						path.setAllowDangling(true);
					} else if (args[i].equals("-strictrootdetection")) {
						path.setRootAlgorithm("STRICT");
					} else if (args[i].equals("-saveimpliedlinks")) {
						path.setSaveImplied(true);
						path
								.setImpliedType(OBOSerializationEngine.SAVE_FOR_PRESENTATION);
					} else if (args[i].equals("-saveallimpliedlinks")) {
						path.setSaveImplied(true);
						path.setImpliedType(OBOSerializationEngine.SAVE_ALL);
					} else if (args[i].equals("-realizeimpliedlinks")) {
						path.setRealizeImpliedLinks(true);
					} else if (args[i].equals("-p")) {
						if (i >= args.length - 1)
							printUsage(1);
						i++;
						String prefilterProperty = args[i];
						path.setPrefilterProperty(prefilterProperty);
					} else {
						path.setPath(args[i]);
						break;
					}
				}
				System.err
						.println("Allowdangling = " + path.getAllowDangling());
				if (path.getPath() == null)
					printUsage(1);
				else
					writeConfig.getSaveRecords().add(path);
			} else if (args[i].equals("-?")) {
				printUsage(0);
			} else if (args[i].equals("-u")){
				if (i >= args.length - 1)
					printUsage(1);
				i++;
				writeConfig.setDbUsername(args[i]);
			} else if (args[i].equals("-p")){
				if (i >= args.length - 1)
					printUsage(1);
				i++;
				writeConfig.setDbPassword(args[i]);
			} else {
				readConfig.getReadPaths().add(args[i]);
			}
		}
		if (readConfig.getReadPaths().size() < 1) {
			logger.info("You must specify at least one file to load.");
			printUsage(1);
		}
		if (writeConfig.getSaveRecords().size() < 1) {
			if (scripts.size() == 0) {
				System.err
						.println("You must specify at least one file to save.");
				printUsage(1);
			}
		}
		writeConfig.setSerializer(formatVersion);
		convertFiles(readConfig, writeConfig, parseObsoleteComments,
				writeObsoleteComments, fixDbxrefs, scripts);
	}

	protected static void printUsage(int exitCode) {
		System.err
				.println("obo2database [-?] [-formatversion <versionid>] <filename 1> ... <filename N> \\\n"
						+ "    [-parsecomments] [-writecomments] \\\n"
						+ "     [-script <scriptname> [arg1 arg2 ... argN] \\;] \\\n"
						+ "   [-o [-f <filterfile1.xml>] <jdbcPath>] ... \\\n"
						+ "   [-o [-f <filterfileN.xml>] <jdbcPathN>]");
		System.err
				.println("  -?                         - Writes this page to stderr and exits.");
		System.err
				.println("  -formatversion <versionid> - The version of OBO to write. Allowed values are\n"
						+ "                               OBO_1_0 and OBO_1_2. The default is OBO_1_2.\n"
						+ "                               Optional.");
		System.err
				.println("  -script <scriptname> <args> \\; - Runs an OSL script on the ontology. A script tag's "
						+ "                                             arguments MUST be followed by a \\; sequence.");

		System.err
		.println("  <jdbcpathN>                - A JDBC location");
		System.err
				.println("  <filenameN>                - An obo file to load. Any number of OBO files may\n"
						+ "                               be loaded");
		System.err
				.println("  -o [-f <filterfile.xml>] [-allowdangling] [-p <prefilter property id>] [-strictrootdetection] [-saveimpliedlinks|-saveallimpliedlinks] [-realizeimpliedlinks] <outputfile.obo> - \n"
						+ "        An output file to write. The optional -f flag may be used to specify a\n"
						+ "        filter file to apply to the output file before writing. If the \n"
						+ "        -allowdangling flag is specified, dangling links will not be written.\n"
						+ "        The optional -p flag specifies the id of a property to use for \n"
						+ "        reasoner pre-filtering. The optional -strict-root-detection flag\n"
						+ "        applies filters using strict root detection.");
		System.exit(exitCode);
	}
}
