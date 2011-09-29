package org.geneontology.gold.io.postgres;

import org.geneontology.conf.GoConfigManager;
import org.geneontology.gold.hibernate.model.GoldDeltaFactory;
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
		
		if(sql.contains("public."+GoConfigManager.getInstance().getGoldDetlaTablePrefix()))
			return super.onPrepareStatement(sql);

		sql = sql.replaceAll("public\\.", "public."+GoConfigManager.getInstance().getGoldDetlaTablePrefix());

		return super.onPrepareStatement(sql);
	}

	
	
}
