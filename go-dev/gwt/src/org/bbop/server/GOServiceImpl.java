package org.bbop.server;

import java.util.LinkedList;

import org.bbop.client.AmiGOLookupService;
import org.bbop.client.GOService;
import org.bbop.client.model.GOGeneProduct;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;

public class GOServiceImpl
	extends RemoteServiceServlet implements GOService {
	
	private static GOConnect godb;
	private static final long serialVersionUID = 1L;

	//
	public GOServiceImpl() {

		super();

		godb = new GOConnect();
	}
	
	//
    public GOGeneProduct[] getGPsBySearch( String symbol ) {

    	// SQL.
        String sql = "SELECT gene_product.id, gene_product.symbol, gene_product.full_name, dbxref.xref_dbname, dbxref.xref_key FROM gene_product INNER JOIN dbxref ON gene_product.dbxref_id = dbxref.id WHERE gene_product.symbol LIKE '%" + symbol + "%'";

    	String[][] info = godb.makeQuery(sql);
    	
    	LinkedList<GOGeneProduct> gp_list = new LinkedList<GOGeneProduct>();
    	for( int i = 0; i < info.length; i++ ){
    		
    		String[] row = info[i];
    		String id = row[3] + ':' + row[4];
    		
    		GOGeneProduct gp = new GOGeneProduct(id);
        	gp.setFullName(row[2]);
        	gp.setSymbol(row[1]);
        	gp_list.add(gp);
    	}
    	
    	return (GOGeneProduct[]) gp_list.toArray(new GOGeneProduct[0]);
    }
    
}
