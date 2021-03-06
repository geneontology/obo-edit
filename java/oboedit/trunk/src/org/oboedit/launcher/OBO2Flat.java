package org.oboedit.launcher;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.apache.log4j.Logger;
import org.bbop.io.IOUtil;
import org.obo.dataadapter.GOFlatFileAdapter;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.ObsoletableObject;
import org.obo.datamodel.impl.DbxrefImpl;
import org.obo.datamodel.impl.DefaultOperationModel;
import org.obo.history.AddDbxrefHistoryItem;
import org.obo.history.CreateLinkHistoryItem;
import org.obo.history.CreateObjectHistoryItem;
import org.obo.history.DefinitionChangeHistoryItem;
import org.obo.history.DeleteLinkHistoryItem;
import org.obo.history.DestroyObjectHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.history.NameChangeHistoryItem;
import org.obo.history.ObsoleteObjectHistoryItem;
import org.obo.history.OperationWarning;
import org.obo.history.StringRelationship;
import org.obo.history.TermMacroHistoryItem;
import org.obo.util.TermUtil;

public class OBO2Flat {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBO2Flat.class);

	protected static String ROOT_ID = "GO:0003673";

	protected static String FUNCTION_ID = "GO:0003674";

	protected static String COMPONENT_ID = "GO:0005575";

	protected static String PROCESS_ID = "GO:0008150";

	protected static String FUNCTION_OBS_ID = "GO:0008369";

	protected static String COMPONENT_OBS_ID = "GO:0008370";

	protected static String PROCESS_OBS_ID = "GO:0008371";

	protected static double oldValue = 0;


	private OBO2Flat() {
	}

	public static void main(String[] args) throws Exception {
                IOUtil.setUpLogging();

		ConvertRecord record = getRecord(args);

		convert(record);
		System.exit(0);
	}

	private static ConvertRecord getRecord(String[] args) {
		if (args.length == 0) {
			System.err
					.println("Usage: obo2flat [options] inputfile1 inputfile2");
			logger.info("IO Switches:");
			logger.info("  -def <path to write definition file>");
			System.err
					.println("  -def4root <output root> <path to definition file>");
			logger.info("  -o <output root> <path to the ouput file>");
			logger.info("Graph rearrangement (required):");
			logger.info("  -cr <dummy root id> <dummy root name>");
			System.err
					.println("  -co <parent id> <obsolete holder id> <obsolete holder name>");
			logger.info("  -adddef <term id> <definition>");
			logger.info("  -addref <term id> <dbxref>");
			logger.info("  -mapobs <namespace> <obsolete holder id>");
			logger.info("  -defaultobs <default obsolete holder id>");
			logger.info("Optional switches:");
			logger.info("  -rootreltype <root id> <type>");
			logger.info("  -symbol <type id> <symbol>");
			logger.info("  -reducefilesize");
			logger.info("  -dangling");
			logger.info("Presets:");
			logger.info("  --gopresets <biological_process output file> <cellular_component output file> <molecular_function output file> <defs output file>");
			logger.info("Other switches:");
			logger.info("  -v\tVerbose mode");
			System.exit(1);
		}
		ConvertRecord record = new ConvertRecord();
		HashMap<String, Vector<String>> temp = new HashMap<String, Vector<String>>();
		for (int i = 0; i < args.length; i++) {
			if (args[i].equals("-v") || args[i].equals("-verbose")) {
				record.verbose = true;
			} else if (args[i].equals("-reducefilesize")) {
				record.reducefilesize = true;
			} else if (args[i].equals("-def")) {
				i++;
				if (i >= args.length) {
					logger.info("-def tag must specify a file name");
					System.exit(1);
				}
				record.defFile = args[i];
			} else if (args[i].equals("-def4root")) {
				i++;
				if (i >= args.length) {
					logger.info("-def4root tag must specify "
							+ "a root term");
					System.exit(1);
				}
				String id = args[i++];
				if (i >= args.length) {
					logger.info("-def4root tag must specify "
							+ "an output path");
					System.exit(1);
				}
				logger.info("id = " + id + ", path = " + args[i]);
				record.defFileHash.put(id, args[i]);
			} else if (args[i].equals("-o")) {
				i++;
				if (i >= args.length) {
					logger.info("-o tag must specify a root id");
					System.exit(1);
				}
				String id = args[i];
				i++;
				if (i >= args.length) {
					System.err
							.println("-o tag must specify an output file path");
					System.exit(1);
				}

				record.outHash.put(id, args[i]);
			} else if (args[i].equals("-cr")) {
				i++;
				if (i >= args.length) {
					logger.info("-cr tag must specify a root id");
					System.exit(1);
				}
				record.fakeRootID = args[i];
				i++;
				if (i >= args.length) {
					logger.info("-cr tag must specify a root name");
					System.exit(1);
				}
				record.fakeRootName = args[i];
			} else if (args[i].equals("-adddef")) {
				i++;
				if (i >= args.length) {
					logger.info("-adddef tag must specify a term id");
					System.exit(1);
				}
				String id = args[i];
				i++;
				if (i >= args.length) {
					logger.info("-adddef tag must specify "
							+ "a definition");
					System.exit(1);
				}
				String def = args[i];
				record.defFileHash.put(id, def);
			} else if (args[i].equals("-addref")) {
				i++;
				if (i >= args.length) {
					logger.info("-addref tag must specify a term id");
					System.exit(1);
				}
				String id = args[i];
				i++;
				if (i >= args.length) {
					logger.info("-addref tag must specify "
							+ "a dbxref to add");
					System.exit(1);
				}
				String dbxref = args[i];
				Vector<Dbxref> v = record.refHash.get(id);
				if (v == null) {
					v = new Vector<Dbxref>();
					record.refHash.put(id, v);
				}
				int index = dbxref.indexOf(':');
				String dbstr = dbxref.substring(0, index);
				String idstr = dbxref.substring(index + 1, dbxref.length());
				v.add(new DbxrefImpl(dbstr, idstr, Dbxref.DEFINITION));
			} else if (args[i].equals("-co")) {
				i++;
				if (i >= args.length) {
					logger.info("-co tag must specify a parent id");
					System.exit(1);
				}
				String id = args[i];
				i++;
				if (i >= args.length) {
					System.err
							.println("-co tag must specify an obsolete node id");
					System.exit(1);
				}
				String obs_id = args[i];
				i++;
				if (i >= args.length) {
					System.err
							.println("-co tag must specify an obsolete node name");
					System.exit(1);
				}
				ObsoleteRecord or = new ObsoleteRecord();
				or.id = obs_id;
				or.parent_id = id;
				or.name = args[i];
				record.obsoleteNodes.add(or);
			} else if (args[i].equals("-mapobs")) {
				i++;
				if (i >= args.length) {
					logger.info("-mapobs tag must specify a namespace");
					System.exit(1);
				}
				String ns = args[i];
				i++;
				if (i >= args.length) {
					System.err
							.println("-mapobs tag must specify an obsolete node id");
					System.exit(1);
				}
				String obs_id = args[i];
				Vector<String> v = temp.get(obs_id);
				if (v == null) {
					v = new Vector<String>();
					temp.put(obs_id, v);
				}
				v.add(ns);
			} else if (args[i].equals("-defaultobs")) {
				i++;
				if (i >= args.length) {
					System.err
							.println("-defaultobs tag must specify a namespace");
					System.exit(1);
				}
				record.defaultObsolete = args[i];
			} else if (args[i].equals("-rootreltype")) {
				i++;
				if (i >= args.length) {
					System.err
							.println("-rootreltype tag must specify a root id");
					System.exit(1);
				}
				RootRecord rr = new RootRecord();
				rr.id = args[i];
				i++;
				if (i >= args.length) {
					System.err
							.println("-rootreltype tag must specify a type id");
					System.exit(1);
				}
				rr.type_id = args[i];
				record.reroots.add(rr);
			} else if (args[i].equals("-symbol")) {
				i++;
				if (i >= args.length) {
					logger.info("-symbol tag must specify a type id");
					System.exit(1);
				}
				String typeid = args[i];
				i++;
				if (i >= args.length) {
					System.err
							.println("-symbol tag must specify a symbol character");
					System.exit(1);
				}
				logger.info("mapping " + typeid + " to " + args[i]);
				record.typeToChar.put(typeid, args[i]);
			} else if (args[i].equals("--gopresets")) {
				i++;
				if (i >= args.length) {
					System.err
							.println("--gopresets tag must specify an output path for the biological process ontology");
					System.exit(1);
				}
				String proc_out = args[i];
				i++;
				if (i >= args.length) {
					System.err
							.println("--gopresets tag must specify an output path for the cellular component ontology");
					System.exit(1);
				}
				String comp_out = args[i];
				i++;
				if (i >= args.length) {
					System.err
							.println("--gopresets tag must specify an output path for the molecular function ontology");
					System.exit(1);
				}
				String func_out = args[i];
				i++;
				if (i >= args.length) {
					System.err
							.println("--gopresets tag must specify an output path for the definitions file");
					System.exit(1);
				}
				String def_out = args[i];

				record.outHash.put(FUNCTION_ID, func_out);
				record.outHash.put(COMPONENT_ID, comp_out);
				record.outHash.put(PROCESS_ID, proc_out);

				record.defFile = def_out;

				record.typeToChar.put("OBO_REL:is_a", "%");
				record.typeToChar.put("part_of", "<");

				/*
				 * record.typeToChar.put("obo_rel:is_a", "%");
				 * record.typeToChar.put("is_a", "%");
				 * record.typeToChar.put("IS_A", "%");
				 * record.typeToChar.put("ISA", "%");
				 * 
				 * record.typeToChar.put("PART_OF", "<");
				 * record.typeToChar.put("PARTOF", "<");
				 * 
				 * record.typeToChar.put("DEVELOPS_FROM", "~");
				 * record.typeToChar.put("DEVELOPSFROM", "~");
				 * 
				 * record.typeToChar.put("inverse_of", "^");
				 * record.typeToChar.put("disjoint_from", "|");
				 * record.typeToChar.put("INVERSE_OF", "^");
				 * record.typeToChar.put("INVERSEOF", "^");
				 * 
				 * record.typeToChar.put("DISJOINT_FROM", "|");
				 * record.typeToChar.put("DISJOINTFROM", "|");
				 * 
				 * record.typeToChar.put("develops_from", "~");
				 */

				ObsoleteRecord or;
				or = new ObsoleteRecord();
				or.id = FUNCTION_OBS_ID;
				or.parent_id = FUNCTION_ID;
				or.name = "obsolete molecular function";
				or.namespaces.add("molecular_function");
				record.obsoleteNodes.add(or);

				or = new ObsoleteRecord();
				or.id = COMPONENT_OBS_ID;
				or.parent_id = COMPONENT_ID;
				or.name = "obsolete cellular component";
				or.namespaces.add("cellular_component");
				record.obsoleteNodes.add(or);

				or = new ObsoleteRecord();
				or.id = PROCESS_OBS_ID;
				or.parent_id = PROCESS_ID;
				or.name = "obsolete biological process";
				or.namespaces.add("biological_process");
				record.obsoleteNodes.add(or);

				record.defaultObsolete = FUNCTION_OBS_ID;
				record.fakeRootID = ROOT_ID;
				record.fakeRootName = "Gene_Ontology";

				Vector<Dbxref> v = new Vector<Dbxref>();
				v.add(new DbxrefImpl("go", "curators", Dbxref.DEFINITION));

				record.defHash
						.put(FUNCTION_OBS_ID,
								"These are terms that have been removed from the active function ontology.");
				record.refHash.put(FUNCTION_OBS_ID, v);
				record.defHash
						.put(COMPONENT_OBS_ID,
								"These are terms that have been removed from the active component ontology.");
				record.refHash.put(COMPONENT_OBS_ID, v);
				record.defHash
						.put(PROCESS_OBS_ID,
								"These are terms that have been removed from the active process ontology.");
				record.refHash.put(PROCESS_OBS_ID, v);

				RootRecord rr = new RootRecord();
				rr.id = FUNCTION_ID;
				rr.type_id = "part_of";
				record.reroots.add(rr);

				rr = new RootRecord();
				rr.id = COMPONENT_ID;
				rr.type_id = "part_of";
				record.reroots.add(rr);

				rr = new RootRecord();
				rr.id = PROCESS_ID;
				rr.type_id = "part_of";
				record.reroots.add(rr);
			} else {
				record.inputFiles.add(args[i]);
			}
		}
		for (int i = 0; i < record.obsoleteNodes.size(); i++) {
			ObsoleteRecord or = record.obsoleteNodes.get(i);
			Vector<String> v = temp.get(or.id);
			if (v != null) {
				or.namespaces.addAll(v);
			}
		}

		Iterator<String> it = record.defHash.keySet().iterator();
		while (it.hasNext()) {
			String id = it.next();
			boolean valid = false;

			for (int i = 0; i < record.obsoleteNodes.size(); i++) {
				ObsoleteRecord or = record.obsoleteNodes.get(i);
				if (or.id.equals(id)) {
					valid = true;
					break;
				}
			}
			if ((record.fakeRootID != null && id.equals(record.fakeRootID)))
				valid = true;
			if (!valid) {
				logger.info(id
						+ " is not allowed in an -adddef switch. "
						+ "-adddef switches may only specify "
						+ "terms created with -co or -cr");
				System.exit(1);
			}

			Vector<Dbxref> v = record.refHash.get(id);
			if (v == null || v.size() == 0) {
				logger.info("You must specify at least one dbxref if "
						+ "you want to do an -adddef on " + id);
				System.exit(1);
			}
		}
		it = record.refHash.keySet().iterator();
		while (it.hasNext()) {
			String id = it.next();
			String def = record.defHash.get(id);
			if (def == null) {
				logger.info("You must specify a definition if "
						+ "you want to do an -addref on " + id);
				System.exit(1);
			}
		}

		// all keys in defHash must correspond to a -cr or -co statement
		// all keys in refHash must correspond to a -cr or -co statement

		return record;
	}

	private static class CreateRecord {
		String id;

		String name;
	}

	private static class RootRecord {
		String id;

		String type_id;
	}

	private static class ObsoleteRecord extends CreateRecord {
		String parent_id;

		Vector<String> namespaces = new Vector<String>();
	}

	private static class ConvertRecord {
		boolean verbose = false;

		boolean reducefilesize = false;

		Vector<String> inputFiles = new Vector<String>();

		Map<String, String> outHash = new HashMap<String, String>();

		Map<String, String> defHash = new HashMap<String, String>();

		Map<String, Vector<Dbxref>> refHash = new HashMap<String, Vector<Dbxref>>();

		Map<String, String> defFileHash = new HashMap<String, String>();

		Vector<ObsoleteRecord> obsoleteNodes = new Vector<ObsoleteRecord>();

		Vector<RootRecord> reroots = new Vector<RootRecord>();

		String defaultObsolete;

		String defFile;

		String fakeRootID;

		String fakeRootName;

		HashMap<String, String> typeToChar = new HashMap<String, String>();

		boolean allowDangling = false;
	}

	public static void convert(ConvertRecord cr) throws Exception {

		OBOFileAdapter adapter = new OBOFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		config.setReadPaths(cr.inputFiles);
		config.setAllowDangling(cr.allowDangling);

		if (cr.verbose) {
			System.err.print("loading files...");
			System.err.flush();
		}

		OBOSession history = adapter.doOperation(OBOAdapter.READ_ONTOLOGY,
				config, null);
		if (cr.verbose) {
			logger.info("done");
			System.err.print("applying changes...");
			System.err.flush();
		}
		DefaultOperationModel opmodel = new DefaultOperationModel();
		opmodel.setSession(history);

		HashMap<String, ObsoleteRecord> obsHash = new HashMap<String, ObsoleteRecord>();
		HashMap<String, ObsoleteRecord> nsObsHash = new HashMap<String, ObsoleteRecord>();
		for (int i = 0; i < cr.obsoleteNodes.size(); i++) {
			ObsoleteRecord or = cr.obsoleteNodes.get(i);
			obsHash.put(or.parent_id, or);
			for (int j = 0; j < or.namespaces.size(); j++) {
				String ns = or.namespaces.get(j);
				nsObsHash.put(ns, or);
			}
		}

		HashMap<String, RootRecord> rootHash = new HashMap<String, RootRecord>();
		for (int i = 0; i < cr.reroots.size(); i++) {
			RootRecord rr = cr.reroots.get(i);
			rootHash.put(rr.id, rr);
		}

		HistoryItem item = getChangeItem(history, cr.fakeRootID,
				cr.fakeRootName, obsHash, rootHash, cr.defHash, cr.refHash);
		OperationWarning ow = opmodel.apply(item);
		logger.info("PO:obsolete = " + history.getObject("PO:obsolete"));
		if (ow != null) {
			logger.info("+++***+++ got operation warning "
					+ ow.getMessage());
		}

		item = getReverseItem(history, nsObsHash, cr.fakeRootID,
				cr.defaultObsolete);
		ow = opmodel.reverse(item);
		if (ow != null) {
			logger.info("+++***+++ got operation warning "
					+ ow.getMessage());
		}

		if (cr.verbose) {
			logger.info("done");
		}

		oldValue = 0;

		GOFlatFileAdapter fadapter = new GOFlatFileAdapter();
		GOFlatFileAdapter.GOFlatFileConfiguration fconfig = new GOFlatFileAdapter.GOFlatFileConfiguration();

		Iterator<String> it = cr.typeToChar.keySet().iterator();
		while (it.hasNext()) {
			String id = it.next();
			String chr = cr.typeToChar.get(id);
			GOFlatFileAdapter.CharTypeMapping mapping = new GOFlatFileAdapter.CharTypeMapping(
					chr, id, id);
			fconfig.getTypeMappings().add(mapping);
		}

		fconfig.setUseLegacyTypes(cr.typeToChar.size() > 0);
		fconfig.setAllowDangling(cr.allowDangling);
		fconfig.setSaveDefFilename(cr.defFile);
		fconfig.setReduceSize(cr.reducefilesize);

		Map<String, GOFlatFileAdapter.SaveRecord> saveRecords = new HashMap<String, GOFlatFileAdapter.SaveRecord>();

		it = cr.outHash.keySet().iterator();
		while (it.hasNext()) {
			String id = it.next();
			String path = cr.outHash.get(id);

			GOFlatFileAdapter.SaveRecord sr = new GOFlatFileAdapter.SaveRecord(
					id, path);
			fconfig.getSaveRecords().add(sr);
			saveRecords.put(id, sr);
		}
		it = cr.defFileHash.keySet().iterator();
		while (it.hasNext()) {
			String id = it.next();
			GOFlatFileAdapter.SaveRecord sr = saveRecords.get(id);
			if (sr == null) {
				sr = new GOFlatFileAdapter.SaveRecord(id, null);
				saveRecords.put(id, sr);
				fconfig.getSaveRecords().add(sr);
			}
			String defPath = cr.defFileHash.get(id);
			sr.setDefFilename(defPath);
		}

		logger.info("saveRecords = " + fconfig.getSaveRecords());
		fadapter.doOperation(OBOAdapter.WRITE_ONTOLOGY, fconfig, history);
	}

	public static TermMacroHistoryItem getChangeItem(OBOSession history,
			String falseRoot, String falseRootName, Map<String, ObsoleteRecord> obsHash, Map<String, RootRecord> rootHash,
			Map<String, String> defHash, Map<String, Vector<Dbxref>> refHash) {
		TermMacroHistoryItem item = new TermMacroHistoryItem(
				"OBO to flatfile operations");
		if (falseRoot != null) {
			OBOClass te = (OBOClass) history.getObject(falseRoot);
			if (te != null)
				item.addItem(new DestroyObjectHistoryItem(te));
			item.addItem(new CreateObjectHistoryItem(falseRoot,
					OBOClass.OBO_CLASS.getID()));
			item.addItem(new NameChangeHistoryItem(falseRootName, falseRoot,
					falseRoot));
		}

		List<IdentifiedObject> roots = new LinkedList<IdentifiedObject>();
		Iterator<IdentifiedObject> it = history.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiedObject io = it.next();
			if (io instanceof OBOClass
					&& ((OBOClass) io).getParents().size() == 0
					&& !TermUtil.isObsolete(io) && !io.isBuiltIn())
				roots.add(io);
		}

		if (falseRoot != null) {
			it = roots.iterator();
			while (it.hasNext()) {
				OBOClass t = (OBOClass) it.next();

				RootRecord rr = rootHash.get(t.getID());
				String type_id = OBOProperty.IS_A.getID();
				if (rr != null) {
//				    OBOClass type = (OBOClass) (history.getObject(rr.type_id));
				    // The line above was throwing a class cast exception.
				    // This is one of those "how did that ever work??" bugs.
				    OBOObject type = (OBOObject) (history.getObject(rr.type_id));
					if (type != null && TermUtil.isProperty(type)) {
						type_id = type.getID();
					}
				}
				// this relationship type may need to be configurable...

				CreateLinkHistoryItem mi = new CreateLinkHistoryItem(t
						.getID(), type_id, falseRoot);
				logger.info("copying " + t.getID() + " to " + falseRoot);
				item.addItem(mi);
			}
		}

		Iterator<String> its = defHash.keySet().iterator();
		while (its.hasNext()) {
			String id = its.next();
			String def = defHash.get(id);
			if (def != null) {
				Vector<Dbxref> temp = refHash.get(id);
				for (int i = 0; i < temp.size(); i++) {
					item.addItem(new AddDbxrefHistoryItem(id, temp.get(i), true, null));
				}
				item.addItem(new DefinitionChangeHistoryItem("", def, id));
			}
		}

		Iterator<ObsoleteRecord> ito = obsHash.values().iterator();
		while (ito.hasNext()) {
			ObsoleteRecord or = ito.next();

			// destroy all the obsolete holder nodes, if they exist already
			OBOClass destroyme = (OBOClass) history.getObject(or.id);
			if (destroyme != null) {
				Iterator<Link> it2 = destroyme.getParents().iterator();
				while (it2.hasNext()) {
					Link link = it2.next();
					item.addItem(new DeleteLinkHistoryItem(link));
				}
				item.addItem(new DestroyObjectHistoryItem(destroyme));
			}

			item.addItem(new CreateObjectHistoryItem(or.id, OBOClass.OBO_CLASS.getID()));
			item.addItem(new CreateLinkHistoryItem(or.id, OBOProperty.IS_A.getID(),
					or.parent_id));

			item.addItem(new NameChangeHistoryItem(or.name, or.id, or.id));
		}

		return item;
	}

	public static HistoryItem getReverseItem(OBOSession history,
			HashMap<String, ObsoleteRecord> nsObsHash, String falseRoot, String defaultObsNode) {

		TermMacroHistoryItem item = new TermMacroHistoryItem("");
		Iterator<ObsoletableObject> it = TermUtil.getObsoletes(history)
				.iterator();
		while (it.hasNext()) {
			ObsoletableObject oo = it.next();
			if (oo instanceof OBOClass) {
				OBOClass t = (OBOClass) oo;

				String ns = t.getNamespace().getID();
				ObsoleteRecord or = nsObsHash.get(ns);
				String obsNode = null;
				if (or != null)
					obsNode = or.id;
				if (obsNode == null)
					obsNode = defaultObsNode;

				if (obsNode == null || obsNode.equals(t.getID())
						|| (falseRoot != null && falseRoot.equals(t.getID())))
					continue;

				OBOClass a = (OBOClass) history.getObject(obsNode);
				OBOClass b = (OBOClass) history.getObject(t.getID());
				/*
				 * logger.info("adding reverse item with obsNode.id
				 * "+obsNode+ ": node = "+a+", child.id = "+t.getID()+": "+ b);
				 */
				StringRelationship sr = new StringRelationship(
						t.getID(), OBOProperty.IS_A.getID(), obsNode);
				item.addItem(new DeleteLinkHistoryItem(sr));
				item.addItem(new ObsoleteObjectHistoryItem(t.getID()));
			}
		}
		return item;
	}
}
