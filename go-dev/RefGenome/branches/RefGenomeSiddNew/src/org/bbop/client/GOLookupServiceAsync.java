package org.bbop.client;


import com.google.gwt.user.client.rpc.AsyncCallback;

public interface GOLookupServiceAsync {
	void isTerm(String identifier, AsyncCallback async);
	void getGPCount(String identifier, AsyncCallback async);
	void getGPs(String identifier, int limit, int offset, AsyncCallback async);
	void getGPInfo(String id, AsyncCallback async);
	void getTermInfo(String acc, AsyncCallback async);
}
