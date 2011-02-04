package org.geneontology.gold.rules.test;

import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;
import owltools.io.ParserWrapper;
import junit.framework.TestCase;

import org.geneontology.gold.rules.AnnotationTaxonRule;
import org.obolibrary.oboformat.model.FrameMergeException;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;

public class AnnotationTaxonCheckTest extends TestCase {
	

	AnnotationTaxonRule ac;
	
	@Override
	public void setUp() throws Exception {
		System.out.println("Setting up: " + this);
		ac = new AnnotationTaxonRule(
				"test_resources/cell_prolif_placenta.obo",
				"test_resources/taxon_go_triggers.obo",
				"test_resources/ncbi_taxon_slim.obo",
				"test_resources/taxon_union_terms.obo"
		);
		
	}


	public void testCheck() throws OWLOntologyCreationException, IOException, FrameMergeException {
		String IN_UTERO = "GO:0001701";
		String EPD = "GO:0001892"; // embryonic placenta development (child of in-utero dev)
		String THERIA = "NCBITaxon:32525"; // Theria is_a Mammalia
		String MAMMALIA = "NCBITaxon:40674";
		String MUS = "NCBITaxon:10088";
		String DMEL = "NCBITaxon:7227";
		String ARTHROPODA = "NCBITaxon:6656";
		String CHICKEN =  "NCBITaxon:9031";
		String AVES = "NCBITaxon:8782";
		String ODONTOGENESIS = "GO:0042476"; // odontogenesis
		String REG_TOOTH_MINERALIZATION = "GO:0070170";
		String ECDYSIS = "GO:0042394"; // ecdysis, protein-based cuticle
		String C_ELEGANS = "NCBITaxon:6239";

		
		// check the only_in theria links
		assertTrue(ac.check(IN_UTERO, MUS));
		assertFalse(ac.check(IN_UTERO, DMEL));
		assertTrue(ac.check(EPD, MUS));
		assertFalse(ac.check(EPD, DMEL));
		
		// check the never_in aves links
		assertFalse(ac.check(ODONTOGENESIS, CHICKEN));
		assertTrue(ac.check(ODONTOGENESIS, MUS));
		assertFalse(ac.check(REG_TOOTH_MINERALIZATION, CHICKEN));
		assertTrue(ac.check(REG_TOOTH_MINERALIZATION, MUS));
		
		// check union
		assertTrue(ac.check(ECDYSIS, C_ELEGANS));
		assertFalse(ac.check(ECDYSIS, MUS));
	}

}
