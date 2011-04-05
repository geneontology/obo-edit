package org.geneontology.gold.rules;

import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import org.geneontology.gaf.hibernate.GeneAnnotation;
import org.obolibrary.oboformat.model.FrameMergeException;
import org.semanticweb.owlapi.model.OWLNamedObject;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;

import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;
import owltools.graph.OWLQuantifiedProperty;
import owltools.io.ParserWrapper;

public class AnnotationTaxonRule extends AbstractAnnotatioRule implements AnnotationRule {
	OWLGraphWrapper graphWrapper;
	OWLGraphWrapper taxGraphWrapper;
	List<String> files;
	List<String> taxfiles;
	
	public String DEFAULT_ONTOL_LOC = "http://www.geneontology.org/ontology/obo_format_1_2/gene_ontology_ext.obo";
	public String DEFAULT_ONTOL_TAXON_BRIDGE_LOC = "http://www.geneontology.org/quality_control/annotation_checks/taxon_checks/taxon_go_triggers.obo";
	public String DEFAULT_TAXON_SLIM_LOC = "http://www.geneontology.org/quality_control/annotation_checks/taxon_checks/ncbi_taxon_slim.obo";
	public String DEFAULT_TAXON_UNION_LOC = "http://www.geneontology.org/quality_control/annotation_checks/taxon_checks/taxon_union_terms.obo";
	
	public AnnotationTaxonRule() throws OWLOntologyCreationException, IOException, FrameMergeException {
		super();
		files = new Vector<String>();
		files.add(DEFAULT_ONTOL_LOC);
		files.add(DEFAULT_ONTOL_TAXON_BRIDGE_LOC);
		taxfiles = new Vector<String>();
		taxfiles.add(DEFAULT_TAXON_SLIM_LOC);
		taxfiles.add(DEFAULT_TAXON_UNION_LOC);
		init();
	}
	
	public AnnotationTaxonRule(String ontolLoc, String ontolTaxonBridgeLoc,
			String taxonSlimLoc, String taxonUnionLoc) throws OWLOntologyCreationException, IOException, FrameMergeException {
		super();
		files = new Vector<String>();
		files.add(ontolLoc);
		files.add(ontolTaxonBridgeLoc);
		taxfiles = new Vector<String>();
		taxfiles.add(taxonSlimLoc);
		taxfiles.add(taxonUnionLoc);
		init();
	}
	
		
	private void init() throws OWLOntologyCreationException, IOException, FrameMergeException {
		ParserWrapper pw = new ParserWrapper();
		System.out.println("ATC PARSING:"+files);
		OWLOntology ont = pw.parseOBOFiles(files);
		ParserWrapper tpw = new ParserWrapper();
		OWLOntology taxOnt = tpw.parseOBOFiles(taxfiles);

		graphWrapper = new OWLGraphWrapper(ont);
		taxGraphWrapper = new OWLGraphWrapper(taxOnt);
	}

	

	public OWLGraphWrapper getTaxGraphWrapper() {
		return taxGraphWrapper;
	}

	public void setTaxGraphWrapper(OWLGraphWrapper taxGraphWrapper) {
		this.taxGraphWrapper = taxGraphWrapper;
	}

	public Set<AnnotationRuleViolation> getRuleViolations(GeneAnnotation a) {
		return getRuleViolations(a.getCls(), a.getBioentityObject().getNcbiTaxonId());
	}
	/*public Set<AnnotationRuleViolation> getRuleViolations(String annotationCls, String ncbiTaxonId) {
		return getRuleViolations(annotationCls, cbiTaxonId);
	}*/

	public boolean check(String annotationCls, String taxonCls) {
		return getRuleViolations(annotationCls, taxonCls).size() == 0;
	
	}
	
	public Set<AnnotationRuleViolation> getRuleViolations(String annotationCls, String taxonCls) {
		Set<AnnotationRuleViolation> violations = new HashSet<AnnotationRuleViolation>();
		OWLObject cls = graphWrapper.getOWLObjectByIdentifier(annotationCls);
		OWLObject tax = taxGraphWrapper.getOWLObjectByIdentifier(taxonCls);
		
		OWLObject rNever = graphWrapper.getOWLObjectPropertyByIdentifier("never_in_taxon");
		OWLObject rOnly = graphWrapper.getOWLObjectPropertyByIdentifier("only_in_taxon");

		Set<OWLGraphEdge> edges = graphWrapper.getOutgoingEdgesClosure(cls);

		// TODO - requires correct obo2owl translation for negation and only constructs
		// ALTERNATIVE HACK - load taxonomy into separate ontology
		boolean isValid = true;
		for (OWLGraphEdge ge : edges) {
			OWLObject tgt = ge.getTarget();
			if (!(tgt instanceof OWLNamedObject))
				continue;
			OWLObject p = taxGraphWrapper.getOWLClass(tgt);
			//System.out.println("edge: "+ge+" prop:"+ge.getLastQuantifiedProperty().getProperty());
			OWLQuantifiedProperty qp = ge.getLastQuantifiedProperty();
			if (qp.isQuantified() &&
					qp.getProperty().equals(rOnly)) {
				//System.out.println("   ONLY: "+rOnly+" p:"+p);
				// ONLY
				if (!taxGraphWrapper.getAncestorsReflexive(tax).contains(p)) {
					//System.out.println("   ANCESTORS OF "+tax+" DOES NOT CONTAIN "+p);
					violations.add(new AnnotationRuleViolation("ANCESTORS OF "+tax+" DOES NOT CONTAIN "+p));
					isValid = false;
				}
			}
			else if (qp.isQuantified() &&
					qp.getProperty().equals(rNever)) {
				//System.out.println("   NEVER: "+rOnly+" p:"+p);
				// NEVER
				if (taxGraphWrapper.getAncestorsReflexive(tax).contains(p)) {
					violations.add(new AnnotationRuleViolation("ANCESTORS OF "+tax+" CONTAINS "+p));
					isValid = false;
				}
			}
		}
		return violations;	
	}
}

