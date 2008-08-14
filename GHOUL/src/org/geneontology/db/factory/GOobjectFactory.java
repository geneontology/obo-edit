package org.geneontology.db.factory;

import org.geneontology.db.model.Term;
import org.hibernate.Session;
import org.hibernate.SessionFactory;

public class GOobjectFactory {
		/** The local {@link SessionFactory} object used to retrieve data. */
	private SessionFactory sf;
	
	/**
	 * Creates a new ChadoAdaptor that will retrieve data from the database configured in the supplied {@link SessionFactory} object.
	 * @param sf a {@link SessionFactory} object with an active transaction. 
	 */
	public GOobjectFactory(SessionFactory sf){
		this.sf = sf;
	}
	
		
	/**
	 * Fetches a {@link Transcript} from the database with the specified feature_id
	 * @param feature_id the feature_id of the {@link Transcript} object to be retrieved from the database.
	 * @return a {@link Transcript} object with the specified feature_id, if it exists. 
	 */
	public Term getTermByName(String name){
		Session session = sf.getCurrentSession();
		return (Term)session.createQuery("from GOTerm where name = ?").setString(0, name).uniqueResult();
	}
}

