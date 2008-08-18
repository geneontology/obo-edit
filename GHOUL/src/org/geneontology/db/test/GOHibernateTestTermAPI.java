package org.geneontology.db.test;

import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.geneontology.db.factory.GOobjectFactory;
import org.geneontology.db.model.DBXref;
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
		for (TermDBXref tdbx : term.getTermDBXrefs()){
			System.out.println("ID:\t" + tdbx.getTerm().getName() + "\t" + tdbx.getDbxref().getDbxref_id());
			
		}
		
		for (TermDBXref tdbx: (List<TermDBXref>)this.getSessionFactory().getCurrentSession().createQuery("from TermDBXref where term=?").setEntity(0, term).list()){
			System.out.println("ID2:\t" + tdbx.getTerm().getName() + "\t" + tdbx.getDbxref().getDbxref_id());
		}
		
	}
	
	public void prettyPrint(GOModel model) {
		Term term = (Term) model;
		System.out.println(term.getCv() + " - " + term.getAcc() + ": " + term.getName());
		System.out.println("def: " + term.getDefinition());
		System.out.println("obsolete=" + term.getIs_obsolete() + ", is_root=" + term.getIs_root());
		if (term.getTermDBXrefs() != null) {
			Iterator<TermDBXref> dbxrefs = term.getTermDBXrefs().iterator();
			while (dbxrefs.hasNext()) {
				DBXref dbxref = ((TermDBXref) dbxrefs.next()).getDbxref();
				prettyPrintDBXref(dbxref);
			}
		}
		if (term.getSynonyms() != null) {
			Iterator<TermSynonym> synonyms = term.getSynonyms().iterator();
			while (synonyms.hasNext()) {
				TermSynonym synonym = (TermSynonym) synonyms.next();
				System.out.println(synonym.getSynonym() + " category=" + synonym.getSynonymCategory() + 
						" type=" + synonym.getSynonymType().getName() + " alt=" + synonym.getAlternateID());

			}
		}
	}
	
	protected void prettyPrintDBXref(DBXref dbxref) {
		System.out.println(dbxref.getDb().getName() + ":" + dbxref.getAccession() + 
			" keytype=" + dbxref.getKeytype() + " desc=" + dbxref.getDescription());
	}
}