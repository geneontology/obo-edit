package org.bbop.client;


import org.bbop.client.model.GO.GeneProduct;

import com.google.gwt.user.client.rpc.RemoteService;

public interface GOService extends RemoteService {

	//
    public GeneProduct[] getGPsBySearch( String symbol );
}
