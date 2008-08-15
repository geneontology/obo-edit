package org.geneontology.db.test;

import java.util.Iterator;
import org.geneontology.db.factory.GOobjectFactory;
import org.geneontology.db.model.DBXref;
import org.geneontology.db.model.GOModel;
import org.geneontology.db.model.Term;
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
		
		logResult(term);
	}
	
	public void prettyPrint(GOModel model) {
		Term term = (Term) model;
		System.out.println(term.getCv() + " - " + term.getAcc() + ": " + term.getName());
		System.out.println("def: " + term.getDefinition());
		System.out.println("obsolete=" + term.getIs_obsolete() + ", is_root=" + term.getIs_root());
		Iterator<DBXref> dbxrefs = term.getDbxrefs().iterator();
		while (dbxrefs.hasNext()) {
			DBXref dbxref = (DBXref) dbxrefs.next();
			prettyPrintDBXref(dbxref);
		}
		Iterator<TermSynonym> synonyms = term.getSynonyms().iterator();
		while (synonyms.hasNext()) {
			TermSynonym synonym = (TermSynonym) synonyms.next();
			System.out.println(synonym.getSynonym() + " category=" + synonym.getSynonymCategory() + 
					" type=" + synonym.getSynonymType().getName() + " alt=" + synonym.getAlternateID());

		}
	}
	
	protected void prettyPrintDBXref(DBXref dbxref) {
		System.out.println(dbxref.getDb().getName() + ":" + dbxref.getAccession() + 
			" keytype=" + dbxref.getKeytype() + " desc=" + dbxref.getDescription());
	}
}