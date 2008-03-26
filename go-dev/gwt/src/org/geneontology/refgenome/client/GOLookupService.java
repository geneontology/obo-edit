package org.bbop.client;


import com.google.gwt.user.client.rpc.RemoteService;

public interface GOLookupService extends RemoteService {

	// Term data.
	public String[] getGPs(String identifier, int limit, int offset);
	public String[] getGPInfo(String id);
	public String[] getTermInfo(String acc);
	public Integer getGPCount(String identifier);
	public Boolean isTerm(String identifier);
	public String getSniff();
}
