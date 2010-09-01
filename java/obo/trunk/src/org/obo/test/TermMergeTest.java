package org.obo.test;

/**
 * Testing term merge
 * 
 * */
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;

import org.obo.datamodel.OBOClass;
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
		this.assertTrue(session.getObject("Test:00040005")==null);
		OBOClass mergedObj = (OBOClass) session.getObject("Test:00040004");
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
		this.assertTrue(ReasonerUtil.getIntersectionLinks(mergedObj).size()==2);
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

	

}
