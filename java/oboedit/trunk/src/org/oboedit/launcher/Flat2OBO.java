package org.oboedit.launcher;

import java.util.*;

import org.bbop.dataadapter.*;
import org.bbop.util.*;
import org.obo.dataadapter.*;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.DefaultOperationModel;
import org.obo.history.*;

import org.apache.log4j.*;

public class Flat2OBO {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(Flat2OBO.class);

	protected static String ROOT_ID = "GO:0003673";

	protected static String FUNCTION_ID = "GO:0003674";
	protected static String COMPONENT_ID = "GO:0005575";
	protected static String PROCESS_ID = "GO:0008150";

	protected static String FUNCTION_OBS_ID = "GO:0008369";
	protected static String COMPONENT_OBS_ID = "GO:0008370";
	protected static String PROCESS_OBS_ID = "GO:0008371";

	private Flat2OBO() {
	}

	public static void main(String[] args) throws Exception {

		ConvertRecord record = getRecord(args);

		convert(record.flatfiles, record.obsHolders, record.newroots,
				record.dels, record.defFile, record.outFile,
				record.defaultNamespace, record.verbose, record.allowDangling,
				record.translateTypes, record.allowCycles);
	}

	private static ConvertRecord getRecord(String[] args) {
		if (args.length == 0) {
			System.err
					.println("Usage: flat2obo [options] flatfile1 flatfile2 -out outputfile");
			logger.error("IO Switches:");
			logger.error("  -def\tPath to the definition file");
			logger.error("  -out, -o\tPath to the ouput file");
			logger.error("Graph rearrangement:");
			logger.error("  -del\tThe id of a term to be deleted");
			System.err
					.println("  -root\tThe id of a term to be made into a root");
			System.err
					.println("  -ns\tRenames the namespace of the last node given by the -root switch");
			System.err
					.println("  -obs\tThe id of a holder node for obsolete terms");
			logger.error("  -defaultns\tSets the default namespace");
			logger.error("Adapter options:");
			System.err
					.println("  -dangling\tAllows dangling relationships in the input file");
			System.err
					.println("  -notranslate\tDoes not translate type names ");
			logger.error("  -cycles\tAllows cycles in the input file");
			logger.error("Presets:");
			System.err
					.println("  --gopresets\tSets all options to suit the Gene Ontology flat files");
			logger.error("Other switches:");
			logger.error("  -v, -verbose\tVerbose mode");
			System.exit(1);
		}
		ConvertRecord record = new ConvertRecord();
		String lastroot = null;
		for (int i = 0; i < args.length; i++) {
			if (args[i].equals("-def")) {
				i++;
				record.defFile = args[i];
				continue;
			} else if (args[i].equals("-del")) {
				i++;
				record.dels.add(args[i]);
				continue;
			} else if (args[i].equals("-obs")) {
				i++;
				record.obsHolders.add(args[i]);
				continue;
			} else if (args[i].equals("-root")) {
				i++;
				lastroot = args[i];
				record.newroots.put(lastroot, null);
				continue;
			} else if (args[i].equals("-defaultns")) {
				i++;
				record.defaultNamespace = args[i];
			} else if (args[i].equals("-dangling")) {
				record.allowDangling = true;
			} else if (args[i].equals("-notranslate")) {
				record.translateTypes = false;
			} else if (args[i].equals("-cycles")) {
				record.allowCycles = true;
			} else if (args[i].equals("-ns")) {
				i++;
				String ns = args[i];
				if (lastroot == null) {
					logger.error("Assigned namespace " + ns
							+ "without specifying root");
					System.exit(1);
				}
				record.newroots.put(lastroot, ns);
			} else if (args[i].equals("--gopresets")) {
				record.obsHolders.add(FUNCTION_OBS_ID);
				record.obsHolders.add(COMPONENT_OBS_ID);
				record.obsHolders.add(PROCESS_OBS_ID);

				record.newroots.put(FUNCTION_ID, "molecular_function");
				record.newroots.put(COMPONENT_ID, "cellular_component");
				record.newroots.put(PROCESS_ID, "biological_process");

				record.dels.add(ROOT_ID);

				record.defaultNamespace = "gene_ontology";
			} else if (args[i].equals("-o") || args[i].equals("-out")) {
				i++;
				String outfile = args[i];
				if (record.outFile == null) {
					record.outFile = outfile;
				} else {
					logger.error("Tried to specify "
							+ "multiple output files");
					System.exit(1);
				}
			} else if (args[i].equals("-v") || args[i].equals("-verbose")) {
				record.verbose = true;
			} else {
				record.flatfiles.add(args[i]);
			}
		}
		return record;
	}

	private static class ConvertRecord {
		Vector flatfiles = new Vector();
		Vector obsHolders = new Vector();
		Vector dels = new Vector();
		HashMap newroots = new HashMap();
		String defFile;
		String outFile;
		String defaultNamespace;
		boolean verbose = false;
		boolean allowDangling = false;
		boolean translateTypes = true;
		boolean allowCycles = false;
	}

	protected static double oldValue;
	protected static boolean starting = true;

	public static void convert(Vector flatfiles, Vector obsHolders,
			HashMap newroots, Vector dels, String defFile, String outfile,
			String defaultNamespace, boolean verbose, boolean allowDangling,
			boolean translateTypes, boolean allowCycles) throws Exception {
		if (verbose) {
			System.err.print("Setting up flat file adapter...");
			System.err.flush();
		}
		GOFlatFileAdapter indriver = new GOFlatFileAdapter();
		GOFlatFileAdapter.GOFlatFileConfiguration config = new GOFlatFileAdapter.GOFlatFileConfiguration();
		config.setHideDownstream(true);
		config.setAllowCycles(allowCycles);
		config.setAllowDangling(allowDangling);
		config.setTranslateTypes(translateTypes);
		config.setDefFilename(defFile);
		config.setReadPaths(flatfiles);

		oldValue = 0;
		starting = true;

		OBOSession history = (OBOSession) indriver.doOperation(
				indriver.READ_ONTOLOGY, config, null);

		if (verbose) {
			logger.error("done");
		}

		if (verbose) {
			System.err.print("Applying transformations...");
			System.err.flush();
		}

		DefaultOperationModel opmodel = new DefaultOperationModel();
		opmodel.setSession(history);
		opmodel.apply(getChangeItem(history, newroots, obsHolders, dels));

		if (defaultNamespace != null)
			history.setDefaultNamespace(new Namespace(defaultNamespace));

		Iterator it = newroots.keySet().iterator();
		while (it.hasNext()) {
			String id = (String) it.next();
			OBOClass t = (OBOClass) history.getObject(id);
			String ns_name = (String) newroots.get(id);
			if (t.getNamespace() == null) {
				Namespace ns = new Namespace(ns_name);
				if (!history.getNamespaces().contains(ns))
					history.addNamespace(ns);
				t.setNamespace(ns);
			} else
				t.getNamespace().setID(ns_name);
		}
		history.getCurrentHistory().setComment("");

		if (verbose) {
			logger.error("done");
		}

		oldValue = 0;
		starting = true;

		OBOFileAdapter outdriver = new OBOFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration outconfig = new OBOFileAdapter.OBOAdapterConfiguration();

		logger.error("outfile = " + outfile);

		outconfig.setAllowDangling(allowDangling);
		/*
		 * OBOSerializationEngine.FilteredPath path = new
		 * OBOSerializationEngine.FilteredPath(null, outfile);
		 */
		outconfig.setWritePath(outfile);
		outdriver.doOperation(outdriver.WRITE_ONTOLOGY, outconfig, history);
		/*
		 * GOBOAdapter.IOProfile profile = GOBOAdapter.
		 * getDefaultIOProfile(history, outfile);
		 * profile.setAllowDangling(allowDangling);
		 * outdriver.setIOProfile(profile); if (verbose)
		 * outdriver.addProgressListener(plistener); outdriver.write(history);
		 * if (verbose) { logger.error("done"); }
		 */
	}

	public static TermMacroHistoryItem getChangeItem(OBOSession history,
			HashMap newroots, Vector obsHolders, Vector dels) {
		TermMacroHistoryItem item = new TermMacroHistoryItem(
				"Flat File conversion");

		Iterator it = newroots.keySet().iterator();
		while (it.hasNext()) {
			String id = (String) it.next();
			OBOClass t = (OBOClass) history.getObject(id);
			addDeleteMacro(item, t.getParents());
		}

		for (int i = 0; i < obsHolders.size(); i++) {
			String id = (String) obsHolders.get(i);
			OBOClass t = (OBOClass) history.getObject(id);
			addDeleteMacro(item, t.getChildren());
			addDeleteMacro(item, t.getParents());
			item.addItem(new ObsoleteObjectHistoryItem(t));
		}

		for (int i = 0; i < dels.size(); i++) {
			String id = (String) dels.get(i);
			OBOClass t = (OBOClass) history.getObject(id);
			if (t.getChildren().size() > 0) {
				addDeleteMacro(item, t.getChildren());
				item.addItem(new ObsoleteObjectHistoryItem(t));
			}
			if (t.getParents().size() > 0) {
				addDeleteMacro(item, t.getParents());
				item.addItem(new ObsoleteObjectHistoryItem(t));
			} else {
				item.addItem(new DeleteLinkHistoryItem(history
						.getObjectFactory().createOBORestriction(t, null, null,
								false)));
			}
		}

		// import the relationship_types file?

		return item;
	}

	protected static void addDeleteMacro(TermMacroHistoryItem item, Collection parents) {
		Iterator it = parents.iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			item.addItem(new DeleteLinkHistoryItem(link));
		}
	}
}
