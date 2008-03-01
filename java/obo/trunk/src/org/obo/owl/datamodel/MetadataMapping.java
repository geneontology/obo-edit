package org.obo.owl.datamodel;

import java.util.Set;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOSession;
import org.obo.owl.dataadapter.OWLAdapter;
import org.semanticweb.owl.model.OWLAnnotationAxiom;
import org.semanticweb.owl.model.OWLAxiom;
import org.semanticweb.owl.model.OWLEntity;

public interface MetadataMapping {


	public boolean isOboToOWLLossy();
	public String getName();
	public String getDesc();
	Set<OWLAxiom> getOWLAxioms(OWLAdapter adapter, OWLEntity owlEntity, IdentifiedObject io);

	/**
	 * Given an axiom in OWL, convert it to the appropriate obo tag
	 * @param axiom
	 * @param lo
	 * @param adapter
	 * @return
	 */
	public boolean translateOWLAxiom(OWLAnnotationAxiom axiom, IdentifiedObject lo, OWLAdapter adapter);

	
	/**
	 * translate everything not handled at the individual OWL Axiom level.
	 * For example, make everything under ObsoleteClass obsolete
	 * @param session
	 */
	public void translateGraph(OBOSession session);

}
