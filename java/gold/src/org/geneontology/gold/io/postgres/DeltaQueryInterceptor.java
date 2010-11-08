package org.geneontology.gold.io.postgres;

import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gold.hibernate.factory.GoldDeltaFactory;
import org.hibernate.EmptyInterceptor;

/**
 * This object changes the table name in a sql statement to temporary deltatable names 
 * (see property geneontology.gold.deltatableprefix). The instance of the object of this class
 * is passed to SessionFactory.openSession() while creating a session. 
 * @see {@link PostgresDialect}, {@link GoldDeltaFactory}, 
 * @author Shahid Manzoor
 *
 */
public class DeltaQueryInterceptor extends EmptyInterceptor {

	@Override
	public String onPrepareStatement(String sql) {
		sql = sql.replaceAll("public\\.", "public."+GeneOntologyManager.getInstance().getGoldDetlaTablePrefix());
		System.out.println("sql is : "+ sql);
		return super.onPrepareStatement(sql);
	}

	
	
}
