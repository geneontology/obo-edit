package org.geneontology.db.test;

import java.util.Set;

import org.geneontology.db.factory.GOobjectFactory;
import org.geneontology.db.model.GOModel;
import org.geneontology.db.model.Term;
import org.geneontology.db.model.TermDBXref;
import org.geneontology.db.model.TermSynonym;

public class GOHibernateTestTermAPI extends AbstractGOHibernateAPITest{
	
	// These probably will need to be adjusted as the database changes. 
	private final String test_name = "cyclohexanone dehydrogenase activity";
	
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
		/*
		prettyPrintTerm (term.getParents(), "PARENTS");
		prettyPrintTerm (term.getChildren(), "CHILDREN");
		*/
		prettyPrintDBXrefs(term);
		prettyPrintSyns(term);
	}
	
	protected void prettyPrintTerm(Set<Term> term_list, String label) {
		for (Term t : term_list) {
			System.out.println(label + ":\t" + t.getName());
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

}