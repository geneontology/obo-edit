package org.bbop.client;

import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.View.RefGenomeView;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.rpc.ServiceDefTarget;

public class RefGenomeServiceClientImpl implements RefGenomeViewListenerI {
	
	private RefGenomeView refgview ;
	private RefGenomeServiceAsync refgservice;
	
	public RefGenomeServiceClientImpl (String url){
		refgview = new RefGenomeView(this);
		refgview.initView();
		
		refgservice = (RefGenomeServiceAsync) GWT.create(RefGenomeService.class);
		ServiceDefTarget endpoint = (ServiceDefTarget) refgservice;
		endpoint.setServiceEntryPoint(url);
		
	}

	public RefGenomeView getView(){
		return refgview;
	}
	
	public void doLogin(String user, String pass) {
		// TODO Auto-generated method stub
		refgservice.checkUserPassword(user, pass, new LoginCallback());
		
	}
	
	private class LoginCallback implements AsyncCallback {
		public void onFailure(Throwable throwable){ GWT.log("error sign in",throwable); }

		public void onSuccess(Object result) {
			// TODO Auto-generated method stub
			Boolean response = (Boolean) result;
			boolean val = response.booleanValue();
			
			if (val) {
				refgview.getLoginPanel().enableLogin();
				refgview.getNavPanel().addCurationBar();
			}
			else {
				refgview.getLoginPanel().denyLogin();
			}
		}
	}

}
