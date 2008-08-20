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
	
}