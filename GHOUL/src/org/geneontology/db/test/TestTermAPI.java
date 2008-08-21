package org.geneontology.db.test;

import org.geneontology.db.factory.GOobjectFactory;
import org.geneontology.db.model.Term;

public class TestTermAPI extends AbstractGOHibernateAPITest{
	
	// These probably will need to be adjusted as the database changes. 
	private final String test_name = "protein tetramerization activity";
	
	public TestTermAPI () {
		super();
	}
	 
	public void testTermQuery(){
		GOobjectFactory goFactory = initSessionFactory();
		
//		Term term = goFactory.getTermByName(test_name);
		this.getSessionFactory().getCurrentSession().beginTransaction();
		Term term = (Term) this.getSessionFactory().getCurrentSession().get(Term.class, 96);
		prettyPrint (term);
	}
	
}