package org.bbop.client;


import com.google.gwt.user.client.rpc.AsyncCallback;

public interface GOServiceAsync {
	void getGPsBySearch(String symbol, AsyncCallback async);
}
