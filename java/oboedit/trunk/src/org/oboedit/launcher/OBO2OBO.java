package org.oboedit.launcher;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;

import org.bbop.expression.ExpressionException;
import org.bbop.expression.ExpressionUtil;
import org.bbop.expression.JexlContext;
import org.bbop.io.IOUtil;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.dataadapter.OBOSerializationEngine;
import org.obo.dataadapter.OBOFileAdapter.OBOAdapterConfiguration;
import org.obo.datamodel.CommentedObject;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.DefinedObject;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.ObsoletableObject;
import org.obo.filters.Filter;
import org.obo.history.HistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.nlp.SemanticParser;
import org.obo.nlp.impl.RegulationTermParser;
import org.obo.reasoner.ReasonerFactory;
import org.obo.util.FilterUtil;
import org.obo.util.LexUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.ExpressionManager;
import org.oboedit.gui.Preferences;

public class OBO2OBO {

	protected static class DanglingWrapper {
		protected String id;

		protected String text;

		public DanglingWrapper(String id, String text) {
			this.id = id;
			this.text = text;
		}

		public String getID() {
			return id;
		}

		public String getName() {
			return text;
		}
	}

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

	public static OBOSession convertFiles(
			OBOFileAdapter.OBOAdapterConfiguration readConfig,
			OBOFileAdapter.OBOAdapterConfiguration writeConfig,
			boolean parseObsoleteComments, boolean writeObsoleteComments,
			boolean fixDbxrefs, List scripts) throws Exception {
		OBOFileAdapter adapter = new OBOFileAdapter();
		OBOSession session = (OBOSession) adapter.doOperation(OBOAdapter.READ_ONTOLOGY,
				readConfig, null);
		if (parseObsoleteComments) {
			parseComments(session);
		}
		if (writeObsoleteComments) {
			writeComments(session);
		}
		if (fixDbxrefs) {
			fixDbxrefs(session);
		}
		if (readConfig.getSemanticParser() != null) {
			SemanticParser semanticParser = readConfig.getSemanticParser();
			semanticParser.index(session);
			Collection<? extends HistoryItem> items = semanticParser.parseTerms();
			for (HistoryItem item : items) {
				//System.out.println(item);
			}
			semanticParser.apply(items);
			if (semanticParser.getNamer() != null) {
				semanticParser.apply(semanticParser.getNamer().generateSynonymChanges(session));
			}
				
			System.out.println("SEMANTIC PARSER REPORT:");
			for (String report : semanticParser.getReports()) {
				System.out.println(report);
			}
		}
		Iterator it = scripts.iterator();
		while (it.hasNext()) {
			ScriptWrapper wrapper = (ScriptWrapper) it.next();
			runScript(session, wrapper.getScript(), wrapper.getArgs());
		}
		System.err.println("About to write files... session object count = "
				+ session.getObjects().size());
		System.err.println("writePath = " + writeConfig.getWritePath());
		System.err.println("savePath = " + writeConfig.getSaveRecords());
		adapter.doOperation(OBOAdapter.WRITE_ONTOLOGY, writeConfig, session);
		return session;
	}

	public static void runScript(OBOSession session, String script, List args)
			throws ExpressionException {
		JexlContext context = ExpressionManager.getManager().getContext();
		context.setGlobalVariable("session", session, false);
		context.setLocalVariable("args", args, true);
		ExpressionUtil.exec(script, context);
	}

	protected static void fixDbxrefs(OBOSession session) {
		Iterator it = session.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			if (io instanceof DefinedObject) {
				DefinedObject dfo = (DefinedObject) io;
				Iterator it2 = dfo.getDefDbxrefs().iterator();
				Dbxref metacycRef = null;
				Dbxref brokenRef = null;
				Dbxref otherRef = null;
				int metacycCount = 0;
				int brokenCount = 0;
				while (it2.hasNext()) {
					Dbxref ref = (Dbxref) it2.next();
					if (ref.getDatabase().equalsIgnoreCase("metacyc")) {
						metacycCount++;
						metacycRef = ref;
					} else if (ref.getDatabase().length() == 0) {
						brokenCount++;
						brokenRef = ref;
					} else {
						otherRef = ref;
					}
				}
				if (brokenRef != null && metacycRef != null) {
					if (brokenCount > 1 || metacycCount > 1) {
						System.err
								.println("*!!!!! Probable broken ref at "
										+ io.getID()
										+ " cannot be automatically repaired. There are too many pieces.");
						continue;
					}
					dfo.removeDefDbxref(metacycRef);
					dfo.removeDefDbxref(brokenRef);
					System.err.println("* Repairing broken dbxref at "
							+ dfo.getID() + ", merging dbxrefs " + metacycRef
							+ " and " + brokenRef);
					metacycRef.setDatabaseID(metacycRef.getDatabaseID() + ","
							+ brokenRef.getDatabaseID());
					dfo.addDefDbxref(metacycRef);
				} else if (dfo.getDefDbxrefs().size() == 2 && brokenRef != null
						&& otherRef != null) {
					dfo.removeDefDbxref(otherRef);
					dfo.removeDefDbxref(brokenRef);
					System.err.println("* Repairing broken dbxref at "
							+ dfo.getID() + ", merging dbxrefs " + otherRef
							+ " and " + brokenRef);
					otherRef.setDatabaseID(otherRef.getDatabaseID() + "," + brokenRef.getDatabaseID());
					dfo.addDefDbxref(otherRef);
				} else if (brokenRef != null) {
					System.err.println("*!! Possible broken ref at "
							+ dfo.getID()
							+ " could not be automatically repaired.");
				}
			}
		}
	}

	protected static void groupTerms(List terms, Map mappable, List external) {
		long time = System.currentTimeMillis();
		Iterator it = terms.iterator();
		while (it.hasNext()) {
			Object o = it.next();
			if (o instanceof DanglingWrapper) {
				external.add(o);
			} else if (o instanceof IdentifiedObject) {
				LinkedObject root = TermUtil.getRoot((LinkedObject) o);
				List list = (List) mappable.get(root);
				if (list == null) {
					list = new ArrayList();
					mappable.put(root, list);
				}
				list.add(o);
			}
		}
		groupTime += System.currentTimeMillis() - time;
	}

	protected static class WordComparator implements Comparator {
		public int compare(Object a, Object b) {
			if (a instanceof DanglingWrapper && b instanceof DanglingWrapper) {
				DanglingWrapper dwa = (DanglingWrapper) a;
				DanglingWrapper dwb = (DanglingWrapper) b;
				return dwa.getName().compareToIgnoreCase(dwb.getName());
			} else if (a instanceof IdentifiedObject
					&& b instanceof IdentifiedObject) {
				IdentifiedObject ia = (IdentifiedObject) a;
				IdentifiedObject ib = (IdentifiedObject) b;
				return ia.getName().compareToIgnoreCase(ib.getName());
			} else
				return 0;
		}
	}

	protected static WordComparator wordComparator = new WordComparator();

	protected static String buildPhrase(List list, String ontologyDescriptor,
			boolean considerChildren) {
		if (list == null || list.size() == 0)
			return null;
		StringBuffer out = new StringBuffer(ontologyDescriptor + " term");
		if (list.size() > 1)
			out.append('s');
		out.append(' ');
		Collections.sort(list, wordComparator);
		for (int i = 0; i < list.size(); i++) {
			Object o = list.get(i);
			String name;
			String id;
			boolean addChildPhrase = false;
			if (o instanceof DanglingWrapper) {
				DanglingWrapper dw = (DanglingWrapper) o;
				name = dw.getName();
				id = dw.getID();
			} else {
				IdentifiedObject io = (IdentifiedObject) o;
				name = io.getName();
				id = io.getID();
				if (considerChildren && io instanceof LinkedObject) {
					addChildPhrase = ((LinkedObject) io).getChildren().size() > 0;
				}
			}
			if (list.size() > 1 && i == list.size() - 1) {
				if (list.size() > 2)
					out.append(",");
				out.append(" and ");
			} else if (i > 0 && list.size() > 2) {
				out.append(", ");
			}

			out.append("'" + name + " ; " + id + "'");
			if (addChildPhrase)
				out.append(" or its children");
		}
		return out.toString();
	}

	protected static String connectPhrases(List list) {
		if (list.size() == 0)
			return null;

		StringBuffer out = new StringBuffer();

		Iterator it = list.iterator();
		for (int i = 0; it.hasNext(); i++) {
			String phrase = (String) it.next();
			if (list.size() > 1 && i == list.size() - 1) {
				if (list.size() > 2)
					out.append(",");
				out.append(" and ");
			} else if (i > 0 && list.size() > 2) {
				out.append(", ");
			}
			out.append(phrase);
		}
		return out.toString();
	}

	private static long extendCommentTime = 0;

	private static long parseCommentTime = 0;

	private static long groupTime = 0;

	protected static void extendComment(OBOSession session,
			CommentedObject commented, List replacedBy, List consider) {

		long atime = System.currentTimeMillis();
		if (replacedBy.size() == 0 && consider.size() == 0)
			return;
		LinkedList replacementPhrases = new LinkedList();
		LinkedList considerPhrases = new LinkedList();

		Iterator it;
		List list = new ArrayList();
		Map mappableTerms = new LinkedHashMap();
		List externalTerms = new LinkedList();
		mappableTerms.clear();

		groupTerms(replacedBy, mappableTerms, externalTerms);

		list.addAll(mappableTerms.keySet());
		Collections.sort(list, wordComparator);
		it = list.iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			List phraseItems = (List) mappableTerms.get(io);
			String phrase = buildPhrase(phraseItems, io.getName(), false);
			if (phrase != null)
				replacementPhrases.add(phrase);
		}
		String phrase = buildPhrase(externalTerms, "external ontology", false);
		if (phrase != null)
			replacementPhrases.add(phrase);

		externalTerms.clear();
		mappableTerms.clear();
		groupTerms(consider, mappableTerms, externalTerms);
		list.addAll(mappableTerms.keySet());
		Collections.sort(list, wordComparator);
		it = list.iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			phrase = buildPhrase((List) mappableTerms.get(io), io.getName(),
					true);
			if (phrase != null)
				considerPhrases.add(phrase);
		}
		phrase = buildPhrase(externalTerms, "external ontology", true);
		if (phrase != null)
			considerPhrases.add(phrase);

		String replacedByStr = connectPhrases(replacementPhrases);
		String considerStr = connectPhrases(considerPhrases);
		StringBuffer out = new StringBuffer("To update annotations, ");
		if (replacedByStr != null) {
			out.append("use ");
			out.append(replacedByStr);
		}
		if (replacedByStr != null && considerStr != null) {
			out.append(" and ");
		}
		if (considerStr != null) {
			out.append("consider ");
			out.append(considerStr);
		}
		out.append(".");
		if (commented.getComment() == null
				&& commented.getComment().length() == 0) {
			commented.setComment(out.toString());
		} else {
			commented.setComment(commented.getComment() + " " + out.toString());
		}
		extendCommentTime += System.currentTimeMillis() - atime;
	}

	protected static void writeComments(OBOSession session) {
		List<ObsoletableObject> replacedBy = new LinkedList<ObsoletableObject>();
		List<ObsoletableObject> consider = new LinkedList<ObsoletableObject>();
		Iterator<ObsoletableObject> it = TermUtil.getObsoletes(session)
				.iterator();
		while (it.hasNext()) {
			ObsoletableObject io = it.next();
			if (io instanceof CommentedObject) {
				parseComments(session, (CommentedObject) io, replacedBy,
						consider, false);
				Iterator<ObsoletableObject> it2 = io.getReplacedBy().iterator();
				while (it2.hasNext()) {
					ObsoletableObject o = it2.next();
					if (!replacedBy.contains(o))
						replacedBy.add(o);
				}
				it2 = io.getConsiderReplacements().iterator();
				while (it2.hasNext()) {
					ObsoletableObject o = it2.next();
					if (!consider.contains(o))
						consider.add(o);
				}

				extendComment(session, (CommentedObject) io, replacedBy,
						consider);

				replacedBy.clear();
				consider.clear();
			}
		}
	}

	protected static void parseComments(OBOSession session) {
		List<ObsoletableObject> replacedBy = new LinkedList<ObsoletableObject>();
		List<ObsoletableObject> consider = new LinkedList<ObsoletableObject>();
		Iterator<ObsoletableObject> it = TermUtil.getObsoletes(session)
				.iterator();
		while (it.hasNext()) {
			ObsoletableObject io = it.next();
			if (io instanceof CommentedObject) {
				parseComments(session, (CommentedObject) io, replacedBy,
						consider, true);
				Iterator<ObsoletableObject> it2 = replacedBy.iterator();
				while (it2.hasNext()) {
					ObsoletableObject o = it2.next();
					io.addReplacedBy(o);
					it2.remove();
				}

				it2 = consider.iterator();
				while (it2.hasNext()) {
					ObsoletableObject o = it2.next();
					io.addConsiderReplacement(o);
					it2.remove();
				}

				extendComment(session, (CommentedObject) io, replacedBy,
						consider);

				replacedBy.clear();
				consider.clear();
			}
		}
	}

	protected static void parseComments(OBOSession session,
			CommentedObject commented, List replacedBy, List consider,
			boolean showWarnings) {
		long time = System.currentTimeMillis();
		int index = commented.getComment()
				.lastIndexOf("To update annotations,");
		if (index == -1) {
			if (showWarnings)
				System.err
						.println("Warning ("
								+ commented.getID()
								+ "): Found comment without appropriate obsolete info.");
			return;
		}
		String realComment = commented.getComment().substring(0, index).trim();
		String updateSection = commented.getComment().substring(index,
				commented.getComment().length());
		commented.setComment(realComment);
		boolean readConsider = false;
		boolean readReplaced = false;
		boolean inQuotes = false;
		List quoteList = new LinkedList();
		boolean magicWordComing = false;

		StringTokenizer tokenizer = new StringTokenizer(updateSection,
				" \t.;'\",!?", true);
		while (tokenizer.hasMoreTokens()) {
			String token = tokenizer.nextToken().trim();
			if (token.length() == 0)
				continue;
			if (token.equals("'") || token.equals("\"")) {
				if (inQuotes) {
					inQuotes = false;
				} else {
					inQuotes = true;
					quoteList.clear();
				}
			} else if (inQuotes) {
				if (magicWordComing) {
					magicWordComing = false;
					Object io = session.getObject(token);
					if (io == null) {
						StringBuffer text = new StringBuffer();
						Iterator it = quoteList.iterator();
						while (it.hasNext()) {
							if (text.length() > 0)
								text.append(" ");
							text.append(it.next().toString());
						}
						io = new DanglingWrapper(token, text.toString());
					} else if (!(io instanceof ObsoletableObject)) {
						System.err.println("Warning (" + commented.getID()
								+ "): " + "Parsed comment identifier " + token
								+ " refers to " + "a non-obsoletable object");
						continue;
					} else if (TermUtil.isObsolete((IdentifiedObject) io)) {
						System.err.println("Warning (" + commented.getID()
								+ "): " + "Parsed comment identifier " + token
								+ " refers to " + "an obsolete object");
						continue;
					}
					if (readConsider) {
						consider.add(io);
					} else if (readReplaced) {
						replacedBy.add(io);
					} else {
						System.err.println("Warning (" + commented.getID()
								+ "): " + "Found replacement identifier "
								+ token + " not preceded by 'use' or "
								+ "'consider'");
					}
				} else if (token.equals(";")) {
					magicWordComing = true;
				} else {
					quoteList.add(token);
				}
			} else if (token.equals("use")) {
				readReplaced = true;
				readConsider = false;
			} else if (token.equals("consider")) {
				readConsider = true;
				readReplaced = false;
			}
		}

		parseCommentTime += System.currentTimeMillis() - time;
	}
	
	public static void checkForSimilarDefs(OBOSession session) {
		 LexUtil lex = new LexUtil(session);
		 lex.findTermSetsWithSimilarDefinitions();
	}

	public static void main(String[] args) throws Exception {
		System.err.println("version = "+Preferences.getVersion());
		if (args.length == 0)
			printUsage(1);
		OBOFileAdapter.OBOAdapterConfiguration readConfig = new OBOFileAdapter.OBOAdapterConfiguration();
		readConfig.setBasicSave(false);
		OBOFileAdapter.OBOAdapterConfiguration writeConfig = new OBOFileAdapter.OBOAdapterConfiguration();
		writeConfig.setBasicSave(false);
		boolean parseObsoleteComments = false;
		boolean writeObsoleteComments = false;
		boolean fixDbxrefs = false;
		boolean checkForSimilarDefs = false;
		LinkedList scripts = new LinkedList();
		String formatVersion = "OBO_1_2";
		for (int i = 0; i < args.length; i++)
			System.err.println("args[" + i + "] = |" + args[i] + "|");

		for (int i = 0; i < args.length; i++) {
			if (args[i].equals("-formatversion")) {
				if (i >= args.length - 1)
					printUsage(1);
				i++;
				formatVersion = args[i];
				if (!(formatVersion.equals("OBO_1_2") || formatVersion
						.equals("OBO_1_0")))
					printUsage(1);
			} else if (args[i].equals("-parsecomments")) {
				parseObsoleteComments = true;
			} else if (args[i].equals("-allowdangling")) {
			    System.out.println("Please note: allowdangling is already set to true by default.");
				readConfig.setAllowDangling(true);
			} else if (args[i].equals("-fixdbxrefs")) {
				fixDbxrefs = true;
			} else if (args[i].equals("-writecomments")) {
				writeObsoleteComments = true;
			} else if (args[i].equals("-lexcheck")) {
				checkForSimilarDefs = true;
			} else if (args[i].equals("-semanticparse")) {
				// TODO: plugging in of specific rules
				// Each rule has an extra embedded -rule argument
				SemanticParser sp = new RegulationTermParser();
				if (args[i+1].equals("-addsynonyms")) {
					i++;
					sp.useDefaultNamer();
				}
				readConfig.setSemanticParser(sp);
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
					} else if (args[i].equals("-lf")) {
						if (i >= args.length - 1)
							printUsage(1);
						i++;
						String filterFile = args[i];
						Filter filter = FilterUtil.loadFilter(filterFile);

						path.setDoLinkFilter(filter != null);
						path.setLinkFilter(filter);
					} else if (args[i].equals("-reasonerfactory")) {
						if (i >= args.length - 1)
							printUsage(1);
						i++;
						path.setReasonerFactory((ReasonerFactory)Class.forName(args[i]).newInstance());
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
//				System.err
//						.println("Allowdangling = " + path.getAllowDangling());
				if (path.getPath() == null)
					printUsage(1);
				else
					writeConfig.getSaveRecords().add(path);
			} else if (args[i].equals("-?")) {
				printUsage(0);
			} else {
				readConfig.getReadPaths().add(args[i]);
			}
		}
		if (readConfig.getReadPaths().size() < 1) {
			System.err.println("You must specify at least one file to load.");
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
		OBOSession session =
			convertFiles(readConfig, writeConfig, parseObsoleteComments,
				writeObsoleteComments, fixDbxrefs, scripts);
		if (checkForSimilarDefs)
			checkForSimilarDefs(session);
	}

	protected static void printUsage(int exitCode) {
		System.err
				.println("obo2obo [-?] [-formatversion <versionid>] <filename 1> ... <filename N> \\\n"
						 + "             [-semanticparse [-addsynonyms]] \\\n"
						 + "             [-parsecomments] [-writecomments] \\\n"
					 + "             [-runscript <scriptname> [arg1 arg2 ... argN] \\;] \\\n"
					 + "             [-o [-reasonerfactory <class>] [-f <filterfile1.xml>] <outputfile1>] ... \\\n"
					 + "             [-o [-f <filterfileN.xml>] <outputfileN>]\n");
		System.err
				.println("  -?                         - Writes this page to stderr and exits.");
		System.err
		.println("  -semanticparse [-addsynonyms] - Does a semantic parse on term name/synonyms; optionally makes synonyms\n"
				+ "                               generates intersection_of definition (which can be reasoned over)\n"
				+ "                               so far only implemented for regulation terms.");
		System.err
		.println("  -reasonerfactory            - When saving implied links, calls a non-default reasoner\n"
				+ "                               Options: ");
		System.err
		.println("  -parsecomments             - Parses comments in obsolete terms looking for\n"
				+ "                               GO-style formatted comments containing parseable\n"
				+ "                               replacement and consider terms.");
		System.err
				.println("  -writecomments             - Writes replaced_by and consider tags into parseable\n"
						+ "                               GO-style formatted comments.");
		System.err
				.println("  -formatversion <versionid> - The version of OBO to write. Allowed values are\n"
						+ "                               OBO_1_0 and OBO_1_2. The default is OBO_1_2.\n"
						+ "                               Optional.");
		System.err
				.println("  -runscript <scriptname> <args> \\; - Runs an OSL script on the ontology. A script tag's\n"
					 + "                               arguments MUST be followed by \\; so that obo2obo knows\n"
					 + "                               where the script arguments stop and obo2obo arguments resume.");
		System.err
				.println("  <filenameN>                - An OBO file to load. Any number of OBO files may\n"
					 + "                               be loaded.");
		System.err
				.println("  -o [-f <objectfilterfile.xml>] [-lf <linkfilterfile.xml>] [-allowdangling] [-p <prefilter property id>] [-strictrootdetection] [-saveimpliedlinks|-saveallimpliedlinks] [-realizeimpliedlinks] <outputfile.obo>\n"
						+ "        An output file to write. The optional -f and -lf flags may be used to specify a\n"
						+ "        filter file or a link filter file to apply to the output file before writing.\n"
					        + "        -allowdangling is now the default and need not be specified.\n"
						+ "        The optional -p flag specifies the id of a property to use for \n"
						+ "        reasoner pre-filtering. The optional -strict-root-detection flag\n"
						+ "        applies filters using strict root detection.");
		System.exit(exitCode);
	}
}
