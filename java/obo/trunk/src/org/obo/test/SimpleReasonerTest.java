package org.obo.test;

import java.util.Arrays;
import java.util.Collection;

import org.apache.log4j.Logger;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.history.CreateLinkHistoryItem;
import org.obo.history.DeleteLinkHistoryItem;
import org.obo.history.HistoryItem;

public class SimpleReasonerTest extends AbstractReasonerTest {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SimpleReasonerTest.class);

	public SimpleReasonerTest(String name) {
		super(name);
	}

	public Collection<String> getFilesToLoad() {
		String[] files={"simple_ontology.obo"};
		return Arrays.asList(files);
	}
	
	

	public static Collection<String> getTests() {
		String[] tests={};
		return Arrays.asList(tests);
	}

	public void testReasonedLinks() throws Exception {
		String A = "A";
		String B = "B";
		String C = "C";
		String D = "D";
		String E = "E";

		testForIsA(B, A);
		testForIsA(C, A);
		testForIsA(D, B);
		testForIsA(D, C);
		testForIsA(D, A);
		
		printData(A);
		deleteLink(D, OBOProperty.IS_A, C);
		printData(A);
		deleteLink(D, OBOProperty.IS_A, B);
		printData(A);
	}

	public void deleteLink(String id1, OBOProperty prop, String id2)
	{
		Link link = reasonedDB.hasRelationship((LinkedObject)session.getObject(id1), prop, (LinkedObject)session.getObject(id2));
		HistoryItem item = new DeleteLinkHistoryItem(link);
		session.getOperationModel().apply(item);
		reasonedDB.removeLink(link);
	}
	
	public void printData(String id)
	{
		IdentifiedObject io = session.getObject(id);
		if (io instanceof LinkedObject) {
			for (Link l : reasonedDB.getParents((LinkedObject)io)) {
				System.out.printf("%s [%s]\n", l, l.isImplied() ? "implied" : "asserted");
			}
			System.out.println("========");
			for (Link l : reasonedDB.getChildren((LinkedObject)io)) {
				System.out.printf("%s [%s]\n", l, l.isImplied() ? "implied" : "asserted");
			}
			System.out.println("~~~~~~~~");
		}
	}

}
