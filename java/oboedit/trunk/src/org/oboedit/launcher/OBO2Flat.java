package org.oboedit.launcher;

import java.util.*;

import org.bbop.dataadapter.*;
import org.bbop.util.*;
import org.obo.dataadapter.*;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.history.*;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

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
		ConvertRecord record = getRecord(args);

		convert(record);
		System.exit(0);
	}

	private static ConvertRecord getRecord(String[] args) {
		if (args.length == 0) {
			System.err
					.println("Usage: obo2flat [options] inputfile1 inputfile2");
			logger.error("IO Switches:");
			logger.error("  -def <path to write definition file>");
			System.err
					.println("  -def4root <output root> <path to definition file>");
			logger.error("  -o <output root> <path to the ouput file>");
			logger.error("Graph rearrangement (required):");
			logger.error("  -cr <dummy root id> <dummy root name>");
			System.err
					.println("  -co <parent id> <obsolete holder id> <obsolete holder name>");
			logger.error("  -adddef <term id> <definition>");
			logger.error("  -addref <term id> <dbxref>");
			logger.error("  -mapobs <namespace> <obsolete holder id>");
			logger.error("  -defaultobs <default obsolete holder id>");
			logger.error("Optional switches:");
			logger.error("  -rootreltype <root id> <type>");
			logger.error("  -symbol <type id> <symbol>");
			logger.error("  -reducefilesize");
			logger.error("  -dangling");
			logger.error("Presets:");
			System.err.println("  --gopresets <biological_process output file> <cellular_component output file> <molecular_function output file> <defs output file>");
			logger.error("Other switches:");
			logger.error("  -v\tVerbose mode");
			System.exit(1);
		}
		ConvertRecord record = new ConvertRecord();
		HashMap temp = new HashMap();
		for (int i = 0; i < args.length; i++) {
			if (args[i].equals("-v") || args[i].equals("-verbose")) {
				record.verbose = true;
			} else if (args[i].equals("-reducefilesize")) {
				record.reducefilesize = true;
			} else if (args[i].equals("-def")) {
				i++;
				if (i >= args.length) {
					logger.error("-def tag must specify a file name");
					System.exit(1);
				}
				record.defFile = args[i];
			} else if (args[i].equals("-def4root")) {
				i++;
				if (i >= args.length) {
					logger.error("-def4root tag must specify "
							+ "a root term");
					System.exit(1);
				}
				String id = args[i++];
				if (i >= args.length) {
					logger.error("-def4root tag must specify "
							+ "an output path");
					System.exit(1);
				}
				logger.error("id = " + id + ", path = " + args[i]);
				record.defFileHash.put(id, args[i]);
			} else if (args[i].equals("-o")) {
				i++;
				if (i >= args.length) {
					logger.error("-o tag must specify a root id");
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
					logger.error("-cr tag must specify a root id");
					System.exit(1);
				}
				record.fakeRootID = args[i];
				i++;
				if (i >= args.length) {
					logger.error("-cr tag must specify a root name");
					System.exit(1);
				}
				record.fakeRootName = args[i];
			} else if (args[i].equals("-adddef")) {
				i++;
				if (i >= args.length) {
					logger.error("-adddef tag must specify a term id");
					System.exit(1);
				}
				String id = args[i];
				i++;
				if (i >= args.length) {
					logger.error("-adddef tag must specify "
							+ "a definition");
					System.exit(1);
				}
				String def = args[i];
				record.defFileHash.put(id, def);
			} else if (args[i].equals("-addref")) {
				i++;
				if (i >= args.length) {
					logger.error("-addref tag must specify a term id");
					System.exit(1);
				}
				String id = args[i];
				i++;
				if (i >= args.length) {
					logger.error("-addref tag must specify "
							+ "a dbxref to add");
					System.exit(1);
				}
				String dbxref = args[i];
				Vector v = (Vector) record.refHash.get(id);
				if (v == null) {
					v = new Vector();
					record.refHash.put(id, v);
				}
				int index = dbxref.indexOf(':');
				String dbstr = dbxref.substring(0, index);
				String idstr = dbxref.substring(index + 1, dbxref.length());
				v.add(new DbxrefImpl(dbstr, idstr, Dbxref.DEFINITION));
			} else if (args[i].equals("-co")) {
				i++;
				if (i >= args.length) {
					logger.error("-co tag must specify a parent id");
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
					logger.error("-mapobs tag must specify a namespace");
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
				Vector v = (Vector) temp.get(obs_id);
				if (v == null) {
					v = new Vector();
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
					logger.error("-symbol tag must specify a type id");
					System.exit(1);
				}
				String typeid = args[i];
				i++;
				if (i >= args.length) {
					System.err
							.println("-symbol tag must specify a symbol character");
					System.exit(1);
				}
				logger.error("mapping " + typeid + " to " + args[i]);
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

				Vector v = new Vector();
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
			ObsoleteRecord or = (ObsoleteRecord) record.obsoleteNodes.get(i);
			Vector v = (Vector) temp.get(or.id);
			if (v != null) {
				or.namespaces.addAll(v);
			}
		}

		Iterator it = record.defHash.keySet().iterator();
		while (it.hasNext()) {
			String id = (String) it.next();
			boolean valid = false;

			for (int i = 0; i < record.obsoleteNodes.size(); i++) {
				ObsoleteRecord or = (ObsoleteRecord) record.obsoleteNodes
						.get(i);
				if (or.id.equals(id)) {
					valid = true;
					break;
				}
			}
			if ((record.fakeRootID != null && id.equals(record.fakeRootID)))
				valid = true;
			if (!valid) {
				logger.error(id
						+ " is not allowed in an -adddef switch. "
						+ "-adddef switches may only specify "
						+ "terms created with -co or -cr");
				System.exit(1);
			}

			Vector v = (Vector) record.refHash.get(id);
			if (v == null || v.size() == 0) {
				logger.error("You must specify at least one dbxref if "
						+ "you want to do an -adddef on " + id);
				System.exit(1);
			}
		}
		it = record.refHash.keySet().iterator();
		while (it.hasNext()) {
			String id = (String) it.next();
			String def = (String) record.defHash.get(id);
			if (def == null) {
				logger.error("You must specify a definition if "
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

		Vector namespaces = new Vector();
	}

	private static class ConvertRecord {
		boolean verbose = false;

		boolean reducefilesize = false;

		Vector inputFiles = new Vector();

		Map outHash = new HashMap();

		Map defHash = new HashMap();

		Map refHash = new HashMap();

		Map defFileHash = new HashMap();

		Vector obsoleteNodes = new Vector();

		Vector reroots = new Vector();

		String defaultObsolete;

		String defFile;

		String fakeRootID;

		String fakeRootName;

		HashMap typeToChar = new HashMap();

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

		OBOSession history = (OBOSession) adapter.doOperation(OBOAdapter.READ_ONTOLOGY,
				config, null);
		if (cr.verbose) {
			logger.error("done");
			System.err.print("applying changes...");
			System.err.flush();
		}
		DefaultOperationModel opmodel = new DefaultOperationModel();
		opmodel.setSession(history);

		HashMap obsHash = new HashMap();
		HashMap nsObsHash = new HashMap();
		for (int i = 0; i < cr.obsoleteNodes.size(); i++) {
			ObsoleteRecord or = (ObsoleteRecord) cr.obsoleteNodes.get(i);
			obsHash.put(or.parent_id, or);
			for (int j = 0; j < or.namespaces.size(); j++) {
				String ns = (String) or.namespaces.get(j);
				nsObsHash.put(ns, or);
			}
		}

		HashMap rootHash = new HashMap();
		for (int i = 0; i < cr.reroots.size(); i++) {
			RootRecord rr = (RootRecord) cr.reroots.get(i);
			rootHash.put(rr.id, rr);
		}

		HistoryItem item = getChangeItem(history, cr.fakeRootID,
				cr.fakeRootName, obsHash, rootHash, cr.defHash, cr.refHash);
		OperationWarning ow = opmodel.apply(item);
		logger.error("PO:obsolete = " + history.getObject("PO:obsolete"));
		if (ow != null) {
			logger.error("+++***+++ got operation warning "
					+ ow.getMessage());
		}

		item = getReverseItem(history, nsObsHash, cr.fakeRootID,
				cr.defaultObsolete);
		ow = opmodel.reverse(item);
		if (ow != null) {
			logger.error("+++***+++ got operation warning "
					+ ow.getMessage());
		}

		if (cr.verbose) {
			logger.error("done");
		}

		oldValue = 0;

		GOFlatFileAdapter fadapter = new GOFlatFileAdapter();
		GOFlatFileAdapter.GOFlatFileConfiguration fconfig = new GOFlatFileAdapter.GOFlatFileConfiguration();

		Iterator it = cr.typeToChar.keySet().iterator();
		Hashtable typeToChar = new Hashtable();
		while (it.hasNext()) {
			String id = (String) it.next();
			String chr = (String) cr.typeToChar.get(id);
			GOFlatFileAdapter.CharTypeMapping mapping = new GOFlatFileAdapter.CharTypeMapping(
					chr, id, id);
			fconfig.getTypeMappings().add(mapping);
		}

		fconfig.setUseLegacyTypes(cr.typeToChar.size() > 0);
		fconfig.setAllowDangling(cr.allowDangling);
		fconfig.setSaveDefFilename(cr.defFile);
		fconfig.setReduceSize(cr.reducefilesize);

		Map saveRecords = new HashMap();

		it = cr.outHash.keySet().iterator();
		while (it.hasNext()) {
			String id = (String) it.next();
			String path = (String) cr.outHash.get(id);

			GOFlatFileAdapter.SaveRecord sr = new GOFlatFileAdapter.SaveRecord(
					id, path);
			fconfig.getSaveRecords().add(sr);
			saveRecords.put(id, sr);
		}
		it = cr.defFileHash.keySet().iterator();
		while (it.hasNext()) {
			String id = (String) it.next();
			GOFlatFileAdapter.SaveRecord sr = (GOFlatFileAdapter.SaveRecord) saveRecords
					.get(id);
			if (sr == null) {
				sr = new GOFlatFileAdapter.SaveRecord(id, null);
				saveRecords.put(id, sr);
				fconfig.getSaveRecords().add(sr);
			}
			String defPath = (String) cr.defFileHash.get(id);
			sr.setDefFilename(defPath);
		}

		logger.error("saveRecords = " + fconfig.getSaveRecords());
		fadapter.doOperation(OBOAdapter.WRITE_ONTOLOGY, fconfig, history);
	}

	public static TermMacroHistoryItem getChangeItem(OBOSession history,
			String falseRoot, String falseRootName, Map obsHash, Map rootHash,
			Map defHash, Map refHash) {
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

		List roots = new LinkedList();
		Iterator it = history.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			if (io instanceof OBOClass
					&& ((OBOClass) io).getParents().size() == 0
					&& !TermUtil.isObsolete(io) && !io.isBuiltIn())
				roots.add(io);
		}

		if (falseRoot != null) {
			it = roots.iterator();
			while (it.hasNext()) {
				OBOClass t = (OBOClass) it.next();

				RootRecord rr = (RootRecord) rootHash.get(t.getID());
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
				logger.error("copying " + t.getID() + " to " + falseRoot);
				item.addItem(mi);
			}
		}

		it = defHash.keySet().iterator();
		while (it.hasNext()) {
			String id = (String) it.next();
			String def = (String) defHash.get(id);
			if (def != null) {
				Vector temp = (Vector) refHash.get(id);
				for (int i = 0; i < temp.size(); i++) {
					item.addItem(new AddDbxrefHistoryItem(id, (Dbxref) temp
							.get(i), true, null));
				}

				item.addItem(new DefinitionChangeHistoryItem("", def, id));
			}
		}

		it = obsHash.values().iterator();
		while (it.hasNext()) {
			ObsoleteRecord or = (ObsoleteRecord) it.next();

			// destroy all the obsolete holder nodes, if they exist already
			OBOClass destroyme = (OBOClass) history.getObject(or.id);
			if (destroyme != null) {
				Iterator it2 = destroyme.getParents().iterator();
				while (it2.hasNext()) {
					Link link = (Link) it2.next();
					item.addItem(new DeleteLinkHistoryItem(link));
				}
				item.addItem(new DestroyObjectHistoryItem(destroyme));
			}

			item.addItem(new CreateObjectHistoryItem(or.id, OBOClass.OBO_CLASS
					.getID()));
			item.addItem(new CreateLinkHistoryItem(or.id, OBOProperty.IS_A.getID(),
					or.parent_id));

			item
					.addItem(new NameChangeHistoryItem(or.name, or.id,
							or.id));
		}

		return item;
	}

	public static HistoryItem getReverseItem(OBOSession history,
			HashMap nsObsHash, String falseRoot, String defaultObsNode) {

		TermMacroHistoryItem item = new TermMacroHistoryItem("");
		Vector delList = new Vector();
		Iterator<ObsoletableObject> it = TermUtil.getObsoletes(history)
				.iterator();
		while (it.hasNext()) {
			ObsoletableObject oo = (ObsoletableObject) it.next();
			if (oo instanceof OBOClass) {
				OBOClass t = (OBOClass) oo;

				String ns = t.getNamespace().getID();
				ObsoleteRecord or = (ObsoleteRecord) nsObsHash.get(ns);
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
				 * logger.error("adding reverse item with obsNode.id
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
