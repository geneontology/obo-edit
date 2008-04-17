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
	
	public void fetchTargetIds() {
		refgservice.fetchReferenceTaxonNodes(new TargetIdsCallback());
	}
	
	public void fetchByName(String name) {
		// TODO Auto-generated method stub
		refgservice.fetchIdsByName(name, new SearchByNameCallback());
		
	}
	
	public void fetchTaxonNodes() {
		refgservice.fetchReferenceTaxonNodes(new TaxonIdsCallback());
	}
	
	private class SearchByNameCallback implements AsyncCallback {

		public void onFailure(Throwable caught) {
			// TODO Auto-generated method stub
			GWT.log("error in search",caught);
		}

		public void onSuccess(Object result) {
			// TODO Auto-generated method stub
			refgview.getNavPanel().getSearchPanelView().displayNameSearchResult(result);
		}
		
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
	
	private class TargetIdsCallback implements AsyncCallback {

		public void onFailure(Throwable throwable) {
			// TODO Auto-generated method stub
			GWT.log("error getting target ids",throwable);
		}

		public void onSuccess(Object result) {
			// TODO Auto-generated method stub
			
		}
		
	}
	
	private class TaxonIdsCallback implements AsyncCallback {

		public void onFailure(Throwable caught) {
			// TODO Auto-generated method stub
			GWT.log("Error fetching taxonids", caught);	
		}

		public void onSuccess(Object result) {
			// TODO Auto-generated method stub
			refgview.getSearchPanel().fillTaxonNodes(result);
			
		}
		
	}

	

}
