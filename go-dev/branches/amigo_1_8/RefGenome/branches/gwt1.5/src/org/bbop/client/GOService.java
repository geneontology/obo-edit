package org.bbop.client;


import org.bbop.client.model.GOGeneProduct;

import com.google.gwt.user.client.rpc.RemoteService;

public interface GOService extends RemoteService {

	//
    public GOGeneProduct[] getGPsBySearch( String symbol );
}
