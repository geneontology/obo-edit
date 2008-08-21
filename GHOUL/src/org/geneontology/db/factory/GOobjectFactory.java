package org.geneontology.db.factory;

import org.geneontology.db.model.DB;
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
	
		
	/** Graph factories for term class
	 */
	
	/**
	 * getTermByName: Fetches a Term from the database with the specified name
	 */
	public Term getTermByName(String name){
		Session session = sf.getCurrentSession();
		return (Term)session.createQuery("from Term where name = ?").setString(0, name).uniqueResult();
	}
	
	/** General utility factories for DBXref and DB classes
	 */
	
	/**
	 * getDBXrefByID
	 * @param int id
	 * @return DBXref
	 */
	public DBXref getDBXrefByID (int id) {
		Session session = sf.getCurrentSession();
		return (DBXref)session.createQuery("from DBXref where id = ?").setInteger(0, id).uniqueResult();
	}

	/**
	 * getDBByName
	 * @param String name
	 * @return DB
	 */
	public DB getDBByName (String name) {
		Session session = sf.getCurrentSession();
		return (DB)session.createQuery("from DB where name = ?").setString(0, name).uniqueResult();
	}

	/** Association factories for these classes:
    * association (includes tables association_qualifier, association_species_qualifier, assoc_rel
    * evidence (includes table evidence_dbxref)
    * gene_product (includes table gene_product_synonym)
    * species
	 */
	
	/** 
	 * getGPByDBXref: Fetches a gene product using the unique identifier as the bait
	 * @param int dbxref_id
	 * @return GeneProduct
	 */
	public GeneProduct getGPByID(int dbxref_id){
		Session session = sf.getCurrentSession();
		return (GeneProduct)session.createQuery("from GeneProduct where dbxref_id = ?").setInteger(0, dbxref_id).uniqueResult();
	}

	/**
	 * getSpeciesByTaxa
	 * @param taxa
	 * @return Species
	 */
	public Species getSpeciesByTaxa(int taxa){
		Session session = sf.getCurrentSession();
		return (Species)session.createQuery("from Species where ncbi_taxa_id = ?").setInteger(0, taxa).uniqueResult();
	}
	

}

