package org.bbop.client;

import java.util.HashMap;


import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.View.RefGenomeView;
import org.bbop.client.model.DateDTO;
import org.bbop.client.model.NodeDTO;

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
		refgservice.fetchNodesByName(name, new SearchByNameCallback());
		
	}
	
	public void fetchTaxonNodes() {
		refgservice.fetchReferenceTaxonNodes(new TaxonIdsCallback());
	}
	
	public void uploadFile(String userId, String filePath, String fileType) {
		refgservice.uploadFile(userId, filePath, fileType, new UploadFileCallback());
		
	}

	public void fetchTargets() {
		FetchTargetNodesCallback callBack = new FetchTargetNodesCallback();
		refgservice.fetchReferenceTargetNodes(callBack);
		
	}

	public void fetchByNameAndTaxon(String name, String taxonId) {
		refgservice.fetchNodesByNameAndTaxon(name, taxonId, new SearchByNameCallback());
		
	}

	public void fetchTargetNodesByName(String name) {
		refgservice.fetchReferenceTargetNodesByName(name, new FetchTargetNodesDetailCallback());
		
	}

	public void assignEntityTargetStatus(String userId, String id, DateDTO date) {
		refgservice.assignEntityTargetStatus(userId, id, date, new ChangeEntityTargetStatusCallback());
	}

	public void retractEntityTargetStatus(String userId, String id) {
		System.err.println("retracting");
		refgservice.retractEntityTargetStatus(userId, id, new ChangeEntityTargetStatusCallback());
	}
	
	public void cancelFetch() {
		FetchTargetNodesCallback callBack = new FetchTargetNodesCallback();
		callBack.doCancel();
		
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
	
	private class FetchTargetNodesCallback implements AsyncCallback {
		
		private HashMap callStack = new HashMap();
		
		public FetchTargetNodesCallback() {
			callStack.put("target", this);
		}
				
		public void doCancel() {
			if (callStack.containsKey("target")) {
				callStack.remove("target");
			}
			refgview.getNavPanel().getBrowseView().displayCancelMsg();
		}

		public void onFailure(Throwable caught) {
			// TODO Auto-generated method stub
			callStack.remove("target");
			GWT.log("error in search",caught);
		}

		public void onSuccess(Object result) {
			// TODO Auto-generated method stub
			System.err.println("showing results...");
			callStack.remove("target");
			refgview.getNavPanel().getBrowseView().displayTargets((NodeDTO[])result);
		}
		
	}
	
	// TODO - merge with the above
	private class FetchTargetNodesDetailCallback implements AsyncCallback {

		public void onFailure(Throwable caught) {
			// TODO Auto-generated method stub
			GWT.log("error in search",caught);
		}

		public void onSuccess(Object result) {
			// TODO Auto-generated method stub
			System.err.println("showing results...");
			//refgview.getNavPanel().getBrowseView().displayTargets((NodeDTO[])result);

			refgview.getSearchPanel().displaySearchTargets((NodeDTO[])result);
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
	
	private class UploadFileCallback implements AsyncCallback {

		public void onFailure(Throwable caught) {
			// TODO Auto-generated method stub
			GWT.log("Error uploading file", caught);	
		}

		public void onSuccess(Object result) {
			// TODO Auto-generated method stub

			// TODO - signal to user somehow
		}
		
	}

	

	private class ChangeEntityTargetStatusCallback implements AsyncCallback {

		public void onFailure(Throwable caught) {
			// TODO Auto-generated method stub
			GWT.log("Error changing status", caught);	
		}

		public void onSuccess(Object result) {
			// TODO Auto-generated method stub

			// TODO - signal to user somehow
		}
		
	}

	

}
