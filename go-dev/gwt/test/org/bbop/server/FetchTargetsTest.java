package org.bbop.server;

import org.bbop.client.RefGenomeService;
import org.bbop.client.model.NodeDTO;

import junit.framework.TestCase;

public class FetchTargetsTest extends TestCase {

	RefGenomeService refgservice;
	
	public void testFetchTargetsByName() {
		refgservice = new RefGenomeServiceImpl();
		NodeDTO[] nodes = refgservice.fetchReferenceTargetNodesByName("TNNC1");
		System.out.println("results: "+nodes.length);
		for (NodeDTO n : nodes) {
			System.out.println(n);
		}
		assertTrue(nodes.length > 0);
		
	}
	
	public void testFetchNodeByNameAndTaxon() {
		refgservice = new RefGenomeServiceImpl();
		NodeDTO[] nodes = refgservice.fetchNodesByNameAndTaxon("TNNC1","NCBITaxon:9606");
		System.out.println("results: "+nodes.length);
		for (NodeDTO n : nodes) {
			System.out.println(n);
		}
		assertTrue(nodes.length > 0);
		
	}
}
