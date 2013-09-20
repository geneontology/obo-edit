package org.obo.test;

/**
 * Testing term merge
 * 
 * */
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.obo.datamodel.Dbxref;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.SynonymType;
import org.obo.datamodel.impl.DefaultOperationModel;
import org.obo.history.DefaultHistoryList;
import org.obo.history.HistoryItem;
import org.obo.history.HistoryList;
import org.obo.history.OperationModel;
import org.obo.history.TermMergeHistoryItem;
import org.obo.util.ReasonerUtil;

public class TermMergeTest extends AbstractOBOTest {
	protected OperationModel operationModel;
	HistoryList out = new DefaultHistoryList();

	public TermMergeTest(String name) {
		super(name);
	}

	@Override
	public Collection<String> getFilesToLoad() {
		String[] files = { "merge_xp_test.obo"};
		return Arrays.asList(files);
	}
	
	//merging two regular terms
	public void testMerge() throws IOException{
		//master= term to be merged to
		OBOClass master = (OBOClass) session.getObject("Test:00040004");
		//slave= intersection
		OBOClass slave = (OBOClass) session.getObject("Test:00040005");
		
		HistoryItem item = new TermMergeHistoryItem(master, slave);
		operationModel = new DefaultOperationModel();
		operationModel.setSession(session);
		operationModel.apply(item);
		
		out.addItem(item);
		assertNull(session.getObject("Test:00040005"));
		OBOClass mergedObj = (OBOClass) session.getObject("Test:00040004");
		assertEquals(2, mergedObj.getSubsets().size());
	}

	//merging intersection term with a regular term
	public void testMergeIntersectionTerm() throws IOException{
		//master= term to be merged to
		OBOClass master = (OBOClass) session.getObject("Test:00040001");
		//slave= intersection
		OBOClass slave = (OBOClass) session.getObject("Test:00040002");
		
		HistoryItem item = new TermMergeHistoryItem(master, slave);
		operationModel = new DefaultOperationModel();
		operationModel.setSession(session);
		operationModel.apply(item);
		
		OBOClass mergedObj = (OBOClass) session.getObject("Test:00040001");
		assertEquals(2, ReasonerUtil.getIntersectionLinks(mergedObj).size());
	}
	
	//merging two intersection terms
	public void testMerge2IntersectionTerms() throws IOException{
		//master= intersection2
		OBOClass master = (OBOClass) session.getObject("Test:00040006");
		//slave= intersection
		OBOClass slave = (OBOClass) session.getObject("Test:00040002");
		
		HistoryItem item = new TermMergeHistoryItem(master, slave);
		operationModel = new DefaultOperationModel();
		operationModel.setSession(session);
		operationModel.apply(item);
		
		OBOClass mergedObj = (OBOClass) session.getObject("Test:00040006");
		this.assertTrue(ReasonerUtil.getIntersectionLinks(mergedObj).size()==3);
	}
	
	//merging child term to parent term
	public void testMergeCycles1() throws IOException{
		//master = term to be used in genus
		OBOClass master = (OBOClass) session.getObject("Test:00040004");
		//slave= auto class
		OBOClass slave = (OBOClass) session.getObject("Test:00040005");
		
		HistoryItem item = new TermMergeHistoryItem(master, slave);
		operationModel = new DefaultOperationModel();
		operationModel.setSession(session);
		operationModel.apply(item);
		
		OBOClass mergedObj = (OBOClass) session.getObject("Test:00040004");
		this.assertTrue(session.getObject("Test:00040005")==null);
	}
	
	//merging parent term to child term 
	// merged term takes over all relations of parent term
	public void testMergeCycles2() throws IOException{
		//master = auto class 2
		OBOClass master = (OBOClass) session.getObject("Test:00040005");
		//slave= term to be used in genus
		OBOClass slave = (OBOClass) session.getObject("Test:00040004");
		
		HistoryItem item = new TermMergeHistoryItem(master, slave);
		operationModel = new DefaultOperationModel();
		operationModel.setSession(session);
		operationModel.apply(item);
		
		OBOClass mergedObj = (OBOClass) session.getObject("Test:00040005");
		this.assertTrue(session.getObject("Test:00040004")==null);
	}

	public void testMergeSynonymType() throws Exception {
		OBOClass master = (OBOClass) session.getObject("Test:00050000");
		OBOClass slave = (OBOClass) session.getObject("Test:00050001");
		
		HistoryItem item = new TermMergeHistoryItem(master, slave);
		operationModel = new DefaultOperationModel();
		operationModel.setSession(session);
		operationModel.apply(item);
		
		OBOClass mergedObj = (OBOClass) session.getObject("Test:00050000");
		Set<Synonym> synonyms = mergedObj.getSynonyms();
		// two synonyms, the merged term label and the merged synonyms with the type
		assertEquals(2, synonyms.size());
		boolean found = false;
		for (Synonym synonym : synonyms) {
			String text = synonym.getText();
			if ("foo bar".equals(text)) {
				SynonymType synonymType = synonym.getSynonymType();
				assertNotNull(synonymType);
				String id = synonymType.getID();
				found = "st1".equals(id);
				break;
			}
		}
		assertTrue(found);
	}
	
	public void testMergeXrefs() throws Exception {
		OBOClass master = (OBOClass) session.getObject("Test:00060000");
		OBOClass slave = (OBOClass) session.getObject("Test:00060001");
		
		HistoryItem item = new TermMergeHistoryItem(master, slave);
		operationModel = new DefaultOperationModel();
		operationModel.setSession(session);
		operationModel.apply(item);
		
		OBOClass mergedObj = (OBOClass) session.getObject("Test:00060000");
		Set<Dbxref> dbxrefs = mergedObj.getDbxrefs();
		assertEquals(2, dbxrefs.size());
		Set<String> expectedXrefs = new HashSet<String>(Arrays.asList("FOO:bar1", "FOO:bar2"));
		for(Dbxref dbxref : dbxrefs) {
			String id = dbxref.getDatabase() + ":" + dbxref.getDatabaseID();
			assertTrue(expectedXrefs.contains(id));
		}
	}
	
	public void testMergeSynonymDbXrefs() throws Exception {
		OBOClass master = (OBOClass) session.getObject("Test:00060002");
		OBOClass slave = (OBOClass) session.getObject("Test:00060003");
		
		HistoryItem item = new TermMergeHistoryItem(master, slave);
		operationModel = new DefaultOperationModel();
		operationModel.setSession(session);
		operationModel.apply(item);
		OBOClass mergedObj = (OBOClass) session.getObject("Test:00060002");
		Set<Synonym> synonyms = mergedObj.getSynonyms();
		// two synonyms, the merged term label and the merged synonyms with the type
		assertEquals(2, synonyms.size());
		boolean found = false;
		Set<String> expectedXrefs = new HashSet<String>(Arrays.asList("FOO:bar1", "FOO:bar2"));
		for (Synonym synonym : synonyms) {
			String text = synonym.getText();
			if ("foo bar".equals(text)) {
				Collection<Dbxref> xrefs = synonym.getXrefs();
				assertEquals(2, xrefs.size());
				for (Dbxref dbxref : xrefs) {
					String id = dbxref.getDatabase() + ":" + dbxref.getDatabaseID();
					assertTrue(expectedXrefs.contains(id));
				}
				found = true;
				break;
			}
		}
		assertTrue(found);
	}

	
	public void testMergeDefXrefs() throws Exception {
		OBOClass master = (OBOClass) session.getObject("Test:00060004");
		OBOClass slave = (OBOClass) session.getObject("Test:00060005");
		
		HistoryItem item = new TermMergeHistoryItem(master, slave);
		operationModel = new DefaultOperationModel();
		operationModel.setSession(session);
		operationModel.apply(item);
		
		OBOClass mergedObj = (OBOClass) session.getObject("Test:00060004");
		Set<Dbxref> defDbxrefs = mergedObj.getDefDbxrefs();
		assertEquals(2, defDbxrefs.size());
	}
}
