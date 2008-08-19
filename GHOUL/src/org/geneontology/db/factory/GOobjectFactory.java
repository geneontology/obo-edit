package org.geneontology.db.factory;

import org.geneontology.db.model.DBXref;
import org.geneontology.db.model.GeneProduct;
import org.geneontology.db.model.Species;
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
		return (Term)session.createQuery("from Term where name = ?").setString(0, name).uniqueResult();
	}
	
	public GeneProduct getGPByDBXref(int dbxref_id){
		Session session = sf.getCurrentSession();
		return (GeneProduct)session.createQuery("from GeneProduct where dbxref_id = ?").setInteger(0, dbxref_id).uniqueResult();
	}
	
	public Species getSpeciesByTaxa(int taxa){
		Session session = sf.getCurrentSession();
		return (Species)session.createQuery("from Species where ncbi_taxa_id = ?").setInteger(0, taxa).uniqueResult();
	}
	
	public DBXref getDBXrefByID (int id) {
		Session session = sf.getCurrentSession();
		return (DBXref)session.createQuery("from DBXref where id = ?").setInteger(0, id).uniqueResult();
	}
}

