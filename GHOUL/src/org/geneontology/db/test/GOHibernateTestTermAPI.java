package org.geneontology.db.test;

import java.util.Set;

import org.geneontology.db.factory.GOobjectFactory;
import org.geneontology.db.model.GOModel;
import org.geneontology.db.model.MetaRelation;
import org.geneontology.db.model.Relation;
import org.geneontology.db.model.Term;
import org.geneontology.db.model.TermDBXref;
import org.geneontology.db.model.TermSynonym;

public class GOHibernateTestTermAPI extends AbstractGOHibernateAPITest{
	
	// These probably will need to be adjusted as the database changes. 
	private final String test_name = "protein tetramerization activity";
	
	public GOHibernateTestTermAPI () {
		super();
	}
	 
	public void testTermQuery(){
		GOobjectFactory goFactory = initSessionFactory();
		
		Term term = goFactory.getTermByName(test_name);
		prettyPrint (term);
	}
	
	public void prettyPrint(GOModel model) {
		Term term = (Term) model;
		System.out.println(term.getCv() + " - " + term.getAcc() + ": " + term.getName());
		System.out.println("def: " + term.getDefinition());
		System.out.println("obsolete=" + term.getIs_obsolete() + ", is_root=" + term.getIs_root());
		
		prettyPrintDBXrefs(term);
		prettyPrintSyns(term);
		
		prettyPrintParents (term.getParents());
		prettyPrintChildren (term.getChildren());
		prettyPrintSubsets (term.getSubsets());
		prettyPrintConsiderations (term.getConsiderations());
	}
	
	protected void prettyPrintParents(Set<Relation> rels) {
		for (Relation r : rels) {
			System.out.println("PARENTS:\t" + r.getType().getName() + "\t" + r.getObject().getName());
		}
	}
	
	protected void prettyPrintChildren(Set<Relation> rels) {
		for (Relation r : rels) {
			System.out.println("CHILDREN:\t" + r.getType().getName() + "\t" + r.getSubject().getName());
		}
	}

	protected void prettyPrintSubsets(Set<Term> slims) {
		for (Term t : slims) {
			System.out.println("SUBSET:\t" + t.getName());
		}
	}

	protected void prettyPrintDBXrefs(Term term) {
		for (TermDBXref tdbx : term.getTermDBXrefs()){
			System.out.println("ID:\t" + tdbx.getDbxref().getDb().getName() + ":" + tdbx.getDbxref().getAccession() + "\t" + tdbx.getDbxref().getDescription() + 
					"\t" + tdbx.getDbxref().getKeytype() + "\t" + tdbx.getIs_for_definition());		
		}
	}
	
	protected void prettyPrintSyns(Term term) {
		for (TermSynonym tsyn : term.getSynonyms()){
			System.out.println("SYN:\t" + tsyn.getSynonym() + "\t" + tsyn.getSynonymCategory() + "\t" + 
					tsyn.getSynonymType().getName() + "\t" + tsyn.getAlternateID());
			
		}
	}

	protected void prettyPrintConsiderations(Set<MetaRelation> metas) {
		for (MetaRelation r : metas) {
			System.out.println("META:\t" + r.getType().getAcc() + "\t" + r.getObject().getName());
		}
	}


}