package org.bbop.server;

import java.util.List;

import org.bbop.client.RefGenomeService;
import org.bbop.client.model.DateDTO;
import org.bbop.client.model.NodeDTO;
import org.bbop.client.model.StatementDTO;

import junit.framework.TestCase;

public class AssignTargetStatusTest extends TestCase {

	RefGenomeService refgservice;
	String userId = "system-test";
	
	static String testJdbcPath = "jdbc:postgresql://localhost:5432/obd_refg_test";

	
	public void setUp() {
		refgservice = new RefGenomeServiceImpl(testJdbcPath);		
	}
	
	public void testAssignTargetStatus() {
		DateDTO date = new DateDTO(1066,1,1);
		String geneId = "NCBI_Gene:55553";
		refgservice.assignEntityTargetStatus(userId, geneId, date);
		NodeDTO[] nodes = refgservice.fetchReferenceTargetNodesByName("SOX6");
		NodeDTO node = nodes[0];
		System.out.println(node);
		List vals = node.getTargetIds(RefGenomeService.HAS_STATUS);
		assertTrue(vals.contains(RefGenomeService.STATUS_TARGET));
	}
	
	public void testAssignHomologyLinkStatement() {
		DateDTO date = new DateDTO(1066,1,1);
		String geneId = "NCBI_Gene:6662"; // SOX9 (human)
		String targetId = "NCBI_Gene:60642";
		String[] methodIds = { "RefG:TEST" };
		refgservice.assignHomologyLinkStatement(userId,
				geneId, targetId, methodIds, "PMID:TEST", "this was assigned as part of a test");
		StatementDTO[] stmts = refgservice.fetchHomologyLinkStatementsByEntityId(geneId);
		System.out.println("homologs for: "+geneId);
		for (StatementDTO s : stmts) {
			System.out.println(s);
		}
		assertTrue(stmts.length > 0);
		System.out.println("homologs for: "+targetId);
		stmts = refgservice.fetchHomologyLinkStatementsByEntityId(targetId);
		for (StatementDTO s : stmts) {
			System.out.println(s);
		}
		assertTrue(stmts.length > 0);
	}
	
}
