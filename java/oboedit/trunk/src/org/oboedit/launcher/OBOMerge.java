package org.oboedit.launcher;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;
import org.bbop.commandline.ArgumentSignature;
import org.bbop.commandline.CommandLineParser;
import org.bbop.commandline.EnumArgumentSignature;
import org.bbop.commandline.FlagSpec;
import org.bbop.commandline.OrderedArgumentSignature;
import org.bbop.commandline.Tag;
import org.bbop.commandline.TagSpec;
import org.bbop.commandline.UnorderedArgumentSignature;
import org.bbop.commandline.ValueSpec;
import org.bbop.dataadapter.DataAdapterException;
import org.bbop.util.VectorFilter;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.impl.AnnotatedObjectImpl;
import org.obo.datamodel.impl.DefaultOperationModel;
import org.obo.datamodel.impl.NestedValueImpl;
import org.obo.datamodel.impl.OBOSessionImpl;
import org.obo.history.CreateObjectHistoryItem;
import org.obo.history.HistoryGenerator;
import org.obo.history.HistoryItem;
import org.obo.history.HistoryList;
import org.obo.history.OperationModel;
import org.obo.history.AddConsiderHistoryItem; 
import org.obo.history.RemoveConsiderHistoryItem;
import org.obo.history.OperationWarning;
import org.obo.history.SessionHistoryList;
import org.obo.identifier.DefaultIDGenerator;
import org.obo.util.HistoryUtil;
import org.obo.util.IDUtil;

public class OBOMerge {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBOMerge.class);

	public static class IDClash {
		protected String id;
		protected String reassignedID;
		protected String message;
		protected int severity;
		protected String reason;

		public IDClash(String id) {
			this.id = id;
			if (id == null)
				throw new IllegalArgumentException();
		}

		public String getReassignedID() {
			return reassignedID;
		}

		public void setReassignedID(String reassignedID) {
			this.reassignedID = reassignedID;
			if (reassignedID == null)
				throw new IllegalArgumentException();
		}

		public String getID() {
			return id;
		}

		public void setID(String id) {
			this.id = id;
			if (id == null)
				throw new IllegalArgumentException();
		}

		public String getMessage() {
			return message;
		}

		public void setMessage(String message) {
			this.message = message;
		}

		public int getSeverity() {
			return severity;
		}

		public void setSeverity(int severity) {
			this.severity = severity;
		}

		public void setPossibleReasons(String string) {
			this.reason = string;
		}
	}

	public static final int FAIL_ON_ANY_CLASH = 0;
	public static final int FAIL_ON_LIKELY_CLASH = 1;
	public static final int REASSIGN_IDS = 2;

	public static final int NEVER_UPDATE = 0;
	public static final int UPDATE_LIKELY_CLASHES = 1;
	public static final int UPDATE_ALL_CLASHES = 2;

	public static final int POSSIBLE = 0;
	public static final int LIKELY = 1;


	protected static void applyChanges(OBOSession session, HistoryList changes) {
		//System.out.println("Here!!!!!");
		OperationModel model = new DefaultOperationModel();
		model.setSession(session);		
		Iterator it = changes.getHistoryItems();
		while (it.hasNext()) {
			HistoryItem item = (HistoryItem) it.next();
			OperationWarning warning = model.apply(item);
			if (warning != null)
				System.out.println("* warning: " + warning);
			logger.info("* warning: " + warning);

		}
	}

	protected static Collection findClashes(OBOSession parentSession,
			OBOSession liveSession, OBOSession branchSession, HistoryList historyParentToLive,
			HistoryList historyParentToBranch, Collection clashesToIgnore) {		


		VectorFilter creationFilter = new VectorFilter() {
			public boolean satisfies(Object o) {
				return o instanceof CreateObjectHistoryItem;
			}
		};

		Map termCreationMap = new HashMap();
		LinkedList clashes = new LinkedList();
		Collection termCreations = HistoryUtil.findMatchingItems(historyParentToLive,
				creationFilter);
		Iterator it = termCreations.iterator();
		while (it.hasNext()) {
			CreateObjectHistoryItem item = (CreateObjectHistoryItem) it.next();
			if (!clashesToIgnore.contains(item.getObjectID()))
				termCreationMap.put(item.getObjectID(), item);
		}

		
		termCreations = HistoryUtil.findMatchingItems(historyParentToBranch, creationFilter);
		it = termCreations.iterator();
		while (it.hasNext()) {
			CreateObjectHistoryItem item = (CreateObjectHistoryItem) it.next();
			if (termCreationMap.containsKey(item.getObjectID())) {
				IdentifiedObject obja = liveSession.getObject(item.getObjectID());
				IdentifiedObject objb = branchSession.getObject(item.getObjectID());

				IDClash clash = new IDClash(item.getObjectID());
				if (obja.getName().equals(objb.getName())) {
					clash.setSeverity(POSSIBLE);
					clash
					.setMessage("Possible ID clash found: Both versions of "
							+ "ontology created a the new term "
							+ obja.getName()
							+ " ("
							+ obja.getID()
							+ ").");
					clash
					.setPossibleReasons("This may be harmless. Usually it means "
							+ "that the \"original\" file specified is not the true "
							+ "point of divergence for these two terms, but is actually "
							+ "an older file (in this case, the merge will still work "
							+ "correctly).");
				} else {
					clash
					.setMessage("LIKELY ID CLASH FOUND: Both versions of the "
							+ "ontology created a term with ID "
							+ item.getObjectID()
							+ " but "
							+ "the term is named "
							+ obja.getName()
							+ " in one ontology and "
							+ objb.getName()
							+ "in the other.");
					clash
					.setPossibleReasons("This is probably a harmful ID clash. This "
							+ "can happen when 2 users have specified overlapping ID spaces"
							+ " and both create a new term. Two semantically distinct terms"
							+ " are then assigned the same id. There's a slight chance that "
							+ "this is a harmless error caused when an old "
							+ "original file is used AND the name of the term was changed "
							+ "in one revision but not the other.");
					clash.setSeverity(LIKELY);
				}
				clashes.add(clash);
			}
		}
		return clashes;
	}

	private static TreeSet getEditedTermIDs(HistoryList historyParentToEdited) {

		HistoryList aHistoryParentToEdited =  historyParentToEdited;
		TreeSet allEditedTerms = new TreeSet();

		for (Iterator historyParentToEditedIterator = aHistoryParentToEdited.getHistoryItems(); historyParentToEditedIterator.hasNext();) {
			HistoryItem historyItem = (HistoryItem) historyParentToEditedIterator.next();

			//System.out.println("OBOMerge: getWholeTermConflict: history item = " + historyItem.toString());
			//System.out.println("OBOMerge: getWholeTermConflict: history item = " + historyItem.getEditedTerms());		
			//System.out.println("OBOMerge: getWholeTermConflict: history item = " + historyItem.getClass());

			allEditedTerms.addAll(historyItem.getEditedTerms());
		}
		return allEditedTerms ;
	}

	public static ArgumentSignature getArgumentSignature() {
		EnumArgumentSignature updateIDsSig = new EnumArgumentSignature();
		updateIDsSig.addSignature(new FlagSpec("NEVER"), false);
		updateIDsSig.addSignature(new FlagSpec("IF_LIKELY"), false);
		updateIDsSig.addSignature(new FlagSpec("ALWAYS"), false);

		EnumArgumentSignature clashFailSig = new EnumArgumentSignature();
		clashFailSig.addSignature(new FlagSpec("ALWAYS"), false);
		clashFailSig.addSignature(new FlagSpec("IF_LIKELY"), false);
		clashFailSig.addSignature(new FlagSpec("NEVER"), false);

		EnumArgumentSignature versionSig = new EnumArgumentSignature();
		versionSig.addSignature(new FlagSpec("OBO_1_0"), false);
		versionSig.addSignature(new FlagSpec("OBO_1_2"), false);

		UnorderedArgumentSignature sig2 = new UnorderedArgumentSignature();
		//		sig2.addSignature(new TagSpec("-id-rule"), 0, 1);
		sig2.addSignature(new TagSpec("-ignore-clash-on-id"), 0,
				Integer.MAX_VALUE);

		//		sig2.addSignature(new TagSpec("-update-ids-when", updateIDsSig), 0, 1,
		sig2.addSignature(new TagSpec("-update-ids", updateIDsSig), 0, 1,
				false);
		sig2.addSignature(new TagSpec("-fail-on-clash", clashFailSig), 0, 1,
				false);

		sig2.addSignature(new TagSpec("-version", versionSig), 0, 1, true);

		//		EnumArgumentSignature idGenEnum = new EnumArgumentSignature();
		//		idGenEnum.addSignature(new FlagSpec("-auto"));
		//		idGenEnum.addSignature(new TagSpec("-rule"));
		//		TagSpec idGenSpec = new TagSpec("-update-clashing-ids", idGenEnum);
		//		sig2.addSignature(idGenSpec);

		sig2.addSignature(new TagSpec("-original"), 1, 1, true);
		sig2.addSignature(new TagSpec("-revision"), 2, 2, true);
		// sig.addSignature(new TagSpec("-rev2"), 1, 1, true);
		sig2.addSignature(new TagSpec("-o"), 1, 1, true);

		OrderedArgumentSignature sig1 = new OrderedArgumentSignature();
		sig1.addSignature(new ValueSpec());
		sig1.addSignature(new ValueSpec());
		sig1.addSignature(new ValueSpec());
		sig1.addSignature(new ValueSpec());

		EnumArgumentSignature sig = new EnumArgumentSignature();
		sig.addSignature(sig1);
		sig.addSignature(sig2);

		return sig2;
	}

	public static void main(String[] args) throws IOException,
	DataAdapterException {

		List argList = new ArrayList();
		for (int i = 0; i < args.length; i++)
			argList.add(args[i]);
		java.util.List argVals;
		try {
			argVals = CommandLineParser.parse(getArgumentSignature(), argList);
		} catch (Exception ex) {
			System.out.println(ex.getMessage());
			logger.info(ex.getMessage());
			printUsage();

			System.out.println("Operation finished.");
			logger.info("Operation finished.");

			return;
		}

		boolean forceMode = false;
		String parentFile = null;
		String liveFile = null;
		String branchFile = null;
		String writePath = null;

		int idUpdateBehavior = NEVER_UPDATE;
		int clashBehavior = FAIL_ON_ANY_CLASH;
		Collection clashesToIgnore = new HashSet();

		boolean autoUpdate = true;

		String oboVersion = "OBO_1_2";  // current default

		Iterator it = argVals.iterator();
		while (it.hasNext()) {
			Tag val = (Tag) it.next();
			if (val.getName().equals("-version")) {
				Tag t = (Tag) val.getValues().get(0);
				// User was forced to specify a version of OBO_1_0 or OBO_1_2 by the command line argument enforcer
				oboVersion = t.getName();
			}
			if (val.getName().equals("-ignore-clash-on-id"))
				clashesToIgnore.add(val.getStringValue());
			if (val.getName().equals("-revision")) {
				if (liveFile == null)
					liveFile = val.getStringValue();
				else
					branchFile = val.getStringValue();
			}
			if (val.getName().equals("-original"))
				parentFile = val.getStringValue();
			if (val.getName().equals("-o"))
				writePath = val.getStringValue();
			if (val.getName().equals("-update-ids")) {
				Tag t = (Tag) val.getValues().get(0);
				if (t.getName().equals("IF_LIKELY"))
					idUpdateBehavior = UPDATE_LIKELY_CLASHES;
				else if (t.getName().equals("ALWAYS"))
					idUpdateBehavior = UPDATE_ALL_CLASHES;
				else if (t.getName().equals("NEVER"))
					idUpdateBehavior = NEVER_UPDATE;
			}
			if (val.getName().equals("-fail-on-clash")) {
				Tag t = (Tag) val.getValues().get(0);
				if (t.getName().equals("IF_LIKELY"))
					clashBehavior = FAIL_ON_LIKELY_CLASH;
				else if (t.getName().equals("ALWAYS"))
					clashBehavior = FAIL_ON_ANY_CLASH;
				else if (t.getName().equals("NEVER"))
					clashBehavior = REASSIGN_IDS;
			}
		}

		OBOFileAdapter adapter = new OBOFileAdapter();
		
		//Read files into OBOSession objects
		OBOSession parentSession = getSession(parentFile, adapter);
		OBOSession liveSession = getSession(liveFile, adapter);
		OBOSession branchSession = getSession(branchFile, adapter);
		
		//Get HistoryList for the changes between the two pairs of files. 
		HistoryList historyParentToLive = HistoryGenerator.getHistory(parentSession, liveSession, null);
		HistoryList historyParentToBranch = HistoryGenerator.getHistory(parentSession, branchSession, null);

		Map mergeIDRemap = IDUtil.createIDRemapping(historyParentToLive);

		//Create TreeSets that will contain just the GO:ids of the terms 
		//that were found to be changed in the two pairs of files.
		TreeSet historyParentToBranchTreeSet = new TreeSet();
		historyParentToBranchTreeSet = getEditedTermIDs(historyParentToBranch);
		TreeSet historyParentToLiveTreeSet = new TreeSet();
		historyParentToLiveTreeSet = getEditedTermIDs(historyParentToLive);

		boolean unresolvedClashes = false;
		Collection clashes = null;
		//	do {
		if (!forceMode) {
			clashes = findClashes(parentSession, liveSession, branchSession, 
					historyParentToLive, historyParentToBranch,
					clashesToIgnore);

			boolean foundLikelyClashes = false;
			it = clashes.iterator();
			while (it.hasNext()) {
				IDClash clash = (IDClash) it.next();
				if (clash.getSeverity() == LIKELY) {
					System.err.print("!!!");
					logger.info("!!!");

					foundLikelyClashes = true;
				}
				System.out.println(clash.getMessage());
				logger.info(clash.getMessage());

			}
			if ((clashBehavior == FAIL_ON_ANY_CLASH && clashes.size() > 0)
					|| (clashBehavior == FAIL_ON_LIKELY_CLASH && foundLikelyClashes)) {
				System.out.println("ID Clashes detected, aborting.");
				logger.info("ID Clashes detected, aborting.");
				System.out.println("Operation finished.");
				logger.info("Operation finished.");

			}
			if (idUpdateBehavior == UPDATE_ALL_CLASHES
					|| (idUpdateBehavior == UPDATE_LIKELY_CLASHES && foundLikelyClashes))
				unresolvedClashes = true;
			if (unresolvedClashes) {
				if (autoUpdate) {
					Collection ids = DefaultIDGenerator.getIDs(liveSession);
					ids.addAll(DefaultIDGenerator.getIDs(branchSession));
					DefaultIDGenerator generator = new DefaultIDGenerator();

					it = clashes.iterator();
					while (it.hasNext()) {
						IDClash clash = (IDClash) it.next();
						if (!(idUpdateBehavior == UPDATE_ALL_CLASHES || (clash
								.getSeverity() == LIKELY && idUpdateBehavior == UPDATE_LIKELY_CLASHES)))
							continue;
						String currentRule = null;

						Pattern p = Pattern.compile("(\\w+):(\\d+)");
						Matcher m = p.matcher(clash.getID());
						if (m.matches()) {
							String prefix = m.group(1);
							String suffix = m.group(2);
							currentRule = prefix + ":$sequence("
							+ suffix.length() + ","
							+ Integer.parseInt(suffix) + ")$";
						} else {
							currentRule = clash.getID() + "_$sequence(10)$";
						}
						String newID = null;
						try {
							IdentifiedObject reassignMe = branchSession
							.getObject(clash.getID());
							newID = generator.generateID(currentRule, null,
									ids, false);
							((OBOSessionImpl) branchSession)
							.changeID(
									(AnnotatedObjectImpl) reassignMe,
									newID);
							clash.setReassignedID(newID);
							System.out.println("updating " + clash.getID()
									+ " to " + newID);
							logger.info("updating " + clash.getID()
									+ " to " + newID);

							ids.add(newID);
						} catch (Exception ex) {
							ex.printStackTrace();
							System.out.println("Could not reassign ID "
									+ clash.getID() + " to " + newID
									+ ". This is " + "probably a bug.");
							logger.info("Could not reassign ID "
									+ clash.getID() + " to " + newID
									+ ". This is " + "probably a bug.");

							System.out.println("Operation finished.");
							logger.info("Operation finished.");

						}

					}
				}
			}
		}
		//		} while (!unresolvedClashes);

		if (clashes != null) {
			it = clashes.iterator();
			while (it.hasNext()) {
				IDClash clash = (IDClash) it.next();
				IdentifiedObject reassignMe = branchSession.getObject(clash
						.getReassignedID());
				if (reassignMe == null)
					continue;
				if (reassignMe.getIDExtension() == null)
					reassignMe.setIDExtension(new NestedValueImpl());
				reassignMe.getIDExtension().setSuggestedComment(
						"id reassigned from " + clash.getID() + " by obomerge");
				System.out.println("set ID extension for "+reassignMe.getID()+", ext = "+reassignMe.getIDExtension());
				logger.info("set ID extension for "+reassignMe.getID()+", ext = "+reassignMe.getIDExtension());

			}
		}

		OBOSession writeMe;
		if (mergeIDRemap.size() > 0) {
			SessionHistoryList sessionHistoryListParentToBranch = HistoryGenerator.getHistory(parentSession, branchSession,
					null);
			it = mergeIDRemap.keySet().iterator();
			while(it.hasNext()) {
				String id = it.next().toString();
				Collection ids = (Collection) mergeIDRemap.get(id);
				System.out.println("** Warning: Mapping edits that refer to secondary "+id+" in file "+branchFile+" to the following primary ids "+ids);
				logger.info("** Warning: Mapping edits that refer to secondary "+id+" in file "+branchFile+" to the following primary ids "+ids);

				sessionHistoryListParentToBranch.forwardID(id, ids);
			}
			applyChanges(parentSession, historyParentToLive);
			applyChanges(parentSession, sessionHistoryListParentToBranch);
			writeMe = parentSession;
		} else {
			//System.out.println("OBOMerge: main about to call applyChanges in second else");
			applyChanges(branchSession, historyParentToLive);
			writeMe = branchSession;
		}

		historyParentToLive = null;
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		config.setWritePath(writePath);
		config.setSerializer(oboVersion);

		adapter.doOperation(OBOAdapter.WRITE_ONTOLOGY, config, writeMe);
		System.out.println("Saved merged ontologies to " + writePath + " (OBO version " + oboVersion + ")");
		logger.info("Saved merged ontologies to " + writePath + " (OBO version " + oboVersion + ")");

		getTermsEditedInLiveAndBranch(historyParentToBranchTreeSet, historyParentToLiveTreeSet);

	}

	private static void getTermsEditedInLiveAndBranch(
			TreeSet historyParentToBranchTreeSet,
			TreeSet historyParentToLiveTreeSet) {
		
		//System.out.println("OBOMerge: getTermsEditedInLiveAndBranch: historyParentToBranchTreeSet contains: " + historyParentToBranchTreeSet.toString());
		//System.out.println("OBOMerge: getTermsEditedInLiveAndBranch: historyParentToLiveTreeSet contains: " + historyParentToLiveTreeSet.toString());

		historyParentToLiveTreeSet.retainAll(historyParentToBranchTreeSet);
		
		//System.out.println("OBOMerge: getTermsEditedInLiveAndBranch Terms edited in both files are: " + historyParentToLiveTreeSet.toString());
	System.out.println("The following terms were edited in both of the derived files: " + historyParentToLiveTreeSet.toString());
	
	}

	public static OBOSession getSession(String path, OBOFileAdapter adapter)
	throws DataAdapterException {
		// OBOFileAdapter adapter = new OBOFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		config.getReadPaths().add(path);
		Object out = adapter.doOperation(OBOAdapter.READ_ONTOLOGY, config, null);
		return (OBOSession) out;
	}

	public static void printUsage() {
		System.out.println("Usage: obomerge "+getArgumentSignature().getShortDocumentation());
		logger.info("Usage: obomerge "+getArgumentSignature().getShortDocumentation());
		System.out.println("For example:  obomerge -fail-on-clash NEVER -original orig.obo -revision file1.obo -revision file2.obo -version OBO_1_2 -o file.merged.obo");
		logger.info("For example:  obomerge -fail-on-clash NEVER -original orig.obo -revision file1.obo -revision file2.obo -version OBO_1_2 -o file.merged.obo");
	}
}
