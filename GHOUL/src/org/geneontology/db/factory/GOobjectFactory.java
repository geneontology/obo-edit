package org.geneontology.db.factory;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLWarning;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import org.geneontology.db.model.Association;
import org.geneontology.db.model.DB;
import org.geneontology.db.model.DBXref;
import org.geneontology.db.model.GeneProduct;
import org.geneontology.db.model.GraphPath;
import org.geneontology.db.model.Species;
import org.geneontology.db.model.Term;
import org.geneontology.db.model.TermSynonym;
import org.geneontology.db.util.HibernateUtil;
import org.hibernate.HibernateException;
import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.SessionFactory;


public class GOobjectFactory {
	/** The local {@link SessionFactory} object used to retrieve data. */
	private static SessionFactory sessions;

	/**
	 * Creates a new ChadoAdaptor that will retrieve data from the database configured in the supplied {@link SessionFactory} object.
	 * @param sessions a {@link SessionFactory} object without an active transaction. 
	 */
	public GOobjectFactory(String config){
		try {
			sessions  = HibernateUtil.buildSessionFactory(config);
		} catch (Exception e) {
			e.printStackTrace();
			if (e instanceof SQLException)
				printSQLException((SQLException) e);
		}
	}

	/**
	 * Creates a new ChadoAdaptor that will retrieve data from the database configured in the supplied {@link SessionFactory} object.
	 * @param sessions a {@link SessionFactory} object without an active transaction. 
	 */
	public GOobjectFactory(SessionFactory sessionFactory){
		sessions  = sessionFactory;
	}

	public synchronized Session startSession() {
		Session session = sessions.getCurrentSession();
		
//		Session session = sessions.getCurrentSession();
		session.beginTransaction();
		return session;
	}

	/** Graph factories for term class
	 */

	/**
	 * @param acc
	 * @return
	 */
	public synchronized Term getTermByAcc(String acc, Session session) {
		if (acc == null || session == null)
			return null;
		Term term =  (Term)session.createQuery("from Term where acc = ?").setString(0, acc).uniqueResult();
		return term;
	}

	public synchronized Term getTermByAlternateAcc(String acc, Session session) {
		String query = "from TermSynonym where alternateID = ?";
		TermSynonym ts = (TermSynonym)session.createQuery(query).setString(0, acc).uniqueResult();
		if (ts == null) {
			return null;
		}
		return ts.getTerm();
	}

	/**
	 * getTermByName: Fetches a Term from the database with the specified name
	 */
	public synchronized Term getTermByName(String name, Session session){
		Iterator<Term> results = session.createQuery("from Term where name = ?").setString(0, name).iterate();
		Term term = null;
		int cnt = 0;
		while (results.hasNext()) {
			term = results.next();
			cnt++;
		}
		if (cnt > 1)
			System.out.println("Retrieved " + cnt + " terms with name " + name);
		return term;
	}

	/** General utility factories for DBXref and DB classes
	 */

	/**
	 * getDBXrefByID
	 * @param int id
	 * @return DBXref
	 */
	public synchronized DBXref getDBXrefByID (int id, Session session) {
		return (DBXref)session.createQuery("from DBXref where id = ?").setInteger(0, id).uniqueResult();
	}

	/**
	 * getDBXrefByDBAcc
	 * @param String db - the name of the database
	 * @param String acc - the accession number
	 * @return DBXref
	 */
	public synchronized DBXref getDBXrefByDBAcc (String db, String acc, Session session) {
		Query q = session.createQuery("from DBXref where db = ? and acc = ?");
		q.setString(0, db);
		q.setString(1, acc);
		return (DBXref)q.uniqueResult();
	}

	/**
	 * getDBByName
	 * @param String name
	 * @return DB
	 */
	public synchronized DB getDBByName (String name, Session session) {
		return (DB)session.createQuery("from DB where name = ?").setString(0, name).uniqueResult();
	}

	/**
	 * Fetches a GeneProduct of {@link GeneProduct} with a {@link org.geneontology.db.model.GeneProduct} having the specified db_name and key.  
	 * @param db_name the {@link org.geneontology.db.model.DBXref} db_name to fetch {@link GeneProduct} objects by. 
	 * @param db_key the {@link org..geneontology.db.model.DBXref} db_key to fetch {@link GeneProduct} objects by.
	 * @return the unique {@link GeneProduct} that have DBXref of with the specified name and key.
	 */
	public synchronized Iterator<GeneProduct> getGPListByDBXref(Vector<String []> xrefs, Session session) {
		String query_str = "select g from GeneProduct as g inner join g.dbxref as xref where ";
		String prefix = "";
		for (Iterator<String []> xref_it = xrefs.iterator(); xref_it.hasNext();) {
			xref_it.next(); // be sure to move forward through the list
			query_str += prefix + "(xref.db_name = ? and xref.accession = ?)";
			prefix = " or ";
		}
		Query query = session.createQuery(query_str);
		int param_count = 0;
		for (Iterator<String []> xref_it = xrefs.iterator(); xref_it.hasNext();) {
			String [] xref = xref_it.next();
			query.setString(param_count++, xref[0]);
			query.setString(param_count++, xref[1]);
		}
		Iterator<GeneProduct> results = query.iterate();
		return results;
	}

	/**
	 * Fetches a GeneProduct of {@link GeneProduct} with a {@link org.geneontology.db.model.GeneProduct} having the specified db_name and key.  
	 * @param db_name the {@link org.geneontology.db.model.DBXref} db_name to fetch {@link GeneProduct} objects by. 
	 * @param db_key the {@link org..geneontology.db.model.DBXref} db_key to fetch {@link GeneProduct} objects by.
	 * @return the unique {@link GeneProduct} that have DBXref of with the specified name and key.
	 */
	public synchronized GeneProduct getGPByDBXref(String db_name, String db_key, Session session) {
		GeneProduct gp = (GeneProduct) session.createQuery("select g from GeneProduct as g inner join g.dbxref as xref " +
				" where xref.db_name = ? and" +
		" xref.accession = ?")
		.setString(0, db_name)
		.setString(1, db_key).uniqueResult();
		return gp;
	}

	/**
	 * As getGPByDBXref, but accepts a dbxref as a string (e.g. "FlyBase:FBgn00000001")
	 * @param xrefStr
	 * @return
	 */
	public synchronized GeneProduct getGPByDBXrefStr(String xrefStr, Session session) {
		String[] parts = xrefStr.split(":", 2);
		return getGPByDBXref(parts[0], parts[1], session);
	}

	/**
	 * Fetches a GeneProduct of {@link GeneProduct} with a {@link org.geneontology.db.model.GeneProduct} having the specified db_name and key.  
	 * @param seq_acc the {@link org..geneontology.db.model.DBXref} db_key to fetch {@link GeneProduct} objects by.
	 * @return the unique {@link GeneProduct} that have DBXref of with the specified name and key.
	 */
	public synchronized GeneProduct getGPByAcc(String seq_acc, Session session) {
		GeneProduct	gp = (GeneProduct) session.createQuery("select g from GeneProduct as g inner join g.dbxref as xref " +
		" where xref.accession = ?")
		.setString(0, seq_acc).uniqueResult();
		return gp;
	}

	/** 
	 * getGPByDBXref: Fetches a gene product using the MOD unique identifier as the bait
	 * @param int dbxref_id
	 * @return GeneProduct
	 */
	public synchronized GeneProduct getGPByDBXref_ID(int dbxref_id, Session session){
		return (GeneProduct)session.createQuery("from GeneProduct where dbxref_id = ?").setInteger(0, dbxref_id).uniqueResult();
	}

	/** 
	 * getGPByDBXref: Fetches a gene product using the MOD unique identifier as the bait
	 * @param int dbxref_id
	 * @return GeneProduct
	 */
	public synchronized GeneProduct getGPByID(int gene_id, Session session){
		return (GeneProduct)session.createQuery("from GeneProduct where id = ?").setInteger(0, gene_id).uniqueResult();
	}

	/**
	 * getGPByName
	 * @param String name
	 * @return GeneProduct
	 */
	public synchronized Iterator<GeneProduct> getGPByName (String name, Session session) {
		return (Iterator<GeneProduct>)session.createQuery("from GeneProduct where symbol = ?").setString(0, name).iterate();
	}

	/*
	 * TODO
	 * Use this when I get around to adding a progress bar
	 * how to get a count
	 * Integer count = (Integer) session.createQuery("select count(*) from ....").uniqueResult();
	 */

	/**
	 * Fetches a GeneProduct of {@link GeneProduct} with a {@link org.geneontology.db.model.GeneProduct} having the specified db_name and key.  
	 * @param db_name the {@link org.geneontology.db.model.DBXref} db_name to fetch {@link GeneProduct} objects by. 
	 * @param db_key the {@link org.geneontology.db.model.DBXref} db_key to fetch {@link GeneProduct} objects by.
	 * @return the unique {@link GeneProduct} that have DBXref of with the specified name and key.
	 */
	public synchronized Iterator<GeneProduct>  getGPBySeq(String db_key, Session session) {
		return (Iterator<GeneProduct>) session.createQuery("select g from GeneProduct as g inner join g.seqs as seq_link " +
				"inner join seq_link.seq as seq " +
				"inner join seq.dbxrefs as xref " +
		"where xref.accession = ?")
		.setString(0, db_key).iterate();
	}

	/**
	 * getSpeciesByTaxa
	 * @param taxa
	 * @return Species
	 */
	public synchronized Species getSpeciesByTaxa(int taxa, Session session){
		return (Species)session.createQuery("from Species where ncbi_taxa_id = ?").setInteger(0, taxa).uniqueResult();
	}

	public synchronized GraphPath getPath(int id1, int id2, Session session) {
		Query q = session.createQuery("from GraphPath p where p.object = ? and p.subject = ?");
		q.setInteger(0, id1);
		q.setInteger(1, id2);
		GraphPath path = null;
		List<GraphPath> path_list = null;
		try {
			path_list = (List<GraphPath>) q.list();
		} catch (Exception e) {
			if (HibernateException.class.isInstance( e ) ) {
				System.out.println ("HibernateException: " + e.getMessage());
			} else if (SQLException.class.isInstance( e ) ) {
				System.out.println ("SQLException: " + e.getMessage());
			} else {
				System.out.println (e.getClass().toString() + " Exception: " + e.getMessage() + " on " + q.getQueryString());
			}
			session.cancelQuery();
//			session.flush();
		}

		if (path_list == null || path_list.size() == 0) {
			/* try again reversing the arguments */
			q = session.createQuery("from GraphPath p where p.object = ? and p.subject = ?");
			q.setInteger(0, id2);
			q.setInteger(1, id1);
			try {
				path_list = (List<GraphPath>) q.list();	
			} catch (Exception e) {
				if (HibernateException.class.isInstance( e ) ) {
					System.out.println ("HibernateException: " + e.getMessage());
				} else if (SQLException.class.isInstance( e ) ) {
					System.out.println ("SQLException: " + e.getMessage());
				} else {
					System.out.println (e.getClass().toString() + " Exception: " + e.getMessage() + " on " + q.getQueryString());
				}
				session.cancelQuery();
			}
		}
		if (path_list != null && path_list.size() > 0) {
			path = path_list.get(0);
			if (path_list.size() > 1) {
				for (int i = 1; i < path_list.size(); i++) {
					GraphPath alt = path_list.get(i);
					if (alt.getDistance() > path.getDistance())
						path = alt;
				}
			}
			//			System.out.println(path.getDistance() + " jumps from " + path.getSubject().getName() + " up to " + path.getObject().getName() + " across " + path_list.size() + " paths");
		}
		return path;
	}

	public synchronized GraphPath getPath(Term term1, Term term2, Session session) {
		GraphPath path = null;
		if (term1 != null && term2 != null) {
			path = getPath(term1.getTerm_id(), term2.getTerm_id(), session);
		}
		return path;
	}

	/**
	 * getSpeciesByName
	 * @param species name
	 * @return Species
	 */
	public synchronized List<Species> getSpeciesByName(String genus, String species, Session session){
		Query q;
		List<Species> specie_list = new ArrayList<Species>();
		try {
			if (species != null && !species.equals("")) {
				q = session.createQuery("from Species where genus = ? and species = ?");
				q.setString(0, genus);
				q.setString(1, species);
				Species result = (Species)q.uniqueResult();
				specie_list.add(result);
				return specie_list;			
			} else {
				q = session.createQuery("from Species where genus = ?");
				q.setString(0, genus);
				q.toString();
				specie_list = (List<Species>) q.list();
			}
		} catch (Exception e) {
			if (HibernateException.class.isInstance( e ) ) {
				System.out.println ("HibernateException: " + e.getMessage());
			} else if (SQLException.class.isInstance( e ) ) {
				System.out.println ("SQLException: " + e.getMessage());
			} else {
				e.printStackTrace();
			}
			session.close();
			// TODO Auto-generated catch block
		}
		return specie_list;
	}

	public synchronized Iterator<Association> getAssociationsIteratorByGP(GeneProduct gp, Session session) {
		Iterator<Association> it = session.createQuery("from Association where gene_product = ?").setEntity(0, gp).iterate();
		return it;
	}

	/**
	 * 
	 */
	public synchronized Vector<Term> getTermIntersection(HashMap<Term, Vector<Association>> annots, Session session) {
		Vector<String> gp_ids = new Vector<String> (annots.size());
		for (Iterator<Vector<Association>> it = annots.values().iterator(); it.hasNext();) {
			Vector<Association> assocs = it.next();
			for (Iterator<Association> vit = assocs.iterator(); vit.hasNext();) {
				Association annot = vit.next();
				DBXref gp_xref = annot.getGene_product().getDbxref();
				String gp_id = gp_xref.getDb_name() + ":" + gp_xref.getAccession();
				if (!gp_ids.contains(gp_id)) {
					gp_ids.add(gp_id);
				}
			}
		}
		return getTermIntersectionByGP(gp_ids, session);

	}

	public synchronized Vector<Term> getTermIntersectionByGP(Collection<String> gp_ids, Session session) {
		/* 
		 * For (working) example
		 * select term.acc, term.name, COUNT(DISTINCT gene_product.id) 
		 * from association a, gene_product gene_product, dbxref dbxref, graph_path graph_path, term term 
		 * where a.is_not = 0
		 *  and gene_product.id = a.gene_product_id
		 *  and dbxref.id = gene_product.dbxref_id 
		 *  and graph_path.term2_id = a.term_id 
		 *  and term.id = graph_path.term1_id 
		 *  and term.is_obsolete = 0 
		 *  and dbxref.xref_key in ('MGI:98358' , 'ZDB-GENE-011207-1', '3735' ) 
		 * group by term.acc, term.name;
		 */
		Query query = session.createQuery(
				"select term.term_id, COUNT(DISTINCT gene_product.gp_id) " +
				"FROM " +
				"Association as association, " +
				"GeneProduct as gene_product, " +
				"DBXref as xref, " +
				"GraphPath as graph_path, " +
				"Term as term " +
				"WHERE " +
				"association.is_not = 0 AND " +
				"gene_product.gp_id = association.gene_product AND " +
				"xref.dbxref_id = gene_product.dbxref AND " +
				"graph_path.subject = association.term AND " +
				"term.term_id = graph_path.object AND " +
				"term.is_obsolete = 0 AND " +
				"xref.accession IN (:gp_list) " +
		"GROUP BY term.term_id"); //having COUNT(DISTINCT gene_product.gp_id) = :gp_count");
		query.setParameterList("gp_list", gp_ids);
		//		query.setInteger("gp_count", gp_ids.size());
		Iterator<Object> it = query.list().iterator();
		Vector<Term> terms = new Vector<Term> ();
		while ( it.hasNext() ) {
			Object[] row = (Object[]) it.next();
			Integer term_id = (Integer) row[0];
			Long gp_count = (Long) row[1];
			if (gp_count.intValue() == gp_ids.size()) {
				Term term = (Term) session.createQuery("from Term where id = ?").setLong(0, term_id.longValue()).uniqueResult();
				terms.add(term);
			}
		}
		return terms;
	}

	public static void printSQLException(SQLException ex) {
		for (Throwable e : ex) {
			if (e instanceof SQLException) {
				e.printStackTrace(System.err);
				System.err.println("SQLState: " +
						((SQLException)e).getSQLState());
				System.err.println("Error Code: " +
						((SQLException)e).getErrorCode());
				System.err.println("Message: " +
						e.getMessage());
				Throwable t = ex.getCause();
				while(t != null) {
					System.out.println(
							"Cause: " + t);
					t = t.getCause();
				}
			}

		}
	}

	public static void getWarningsFromResultSet(ResultSet rs) throws SQLException {
		printWarnings(rs.getWarnings());
	}

	public static void getWarningsFromStatement(Statement stmt) throws SQLException {
		printWarnings(stmt.getWarnings());
	}

	public static void printWarnings(SQLWarning warning) throws SQLException {
		if (warning != null) {
			System.out.println("\n---Warning---\n");
			while (warning != null) {
				System.out.println("Message: " +
						warning.getMessage());
				System.out.println("SQLState: " +
						warning.getSQLState());
				System.out.print("Vendor error code: ");
				System.out.println(
						warning.getErrorCode());
				System.out.println("");
				warning = warning.getNextWarning();
			}
		}
	}
}

