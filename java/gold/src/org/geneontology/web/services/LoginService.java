package org.geneontology.web.services;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.NameValuePair;
import org.apache.commons.httpclient.methods.PostMethod;

import com.google.gson.Gson;

public class LoginService extends ServiceHandlerAbstract {

	private static final Gson gson = new Gson();	
	
	public static final String SERVICE_NAME="login-service";
	
	private String viewPath;
	
	private boolean isLoggedIn;
	
	@Override
	public void handleService(HttpServletRequest request,
			HttpServletResponse response) throws IOException, ServletException {
	
		
		this.viewPath = "/servicesui/login.jsp";

		HttpSession session = request.getSession(true);

		Object userId = session.getAttribute("userid");
		String assertion = request.getParameter("assertion");
		//user is not logged-in uet
		if(userId == null && assertion != null) {
			BrowserIdVerificationResponse details=	authenticateUser(assertion, request.getServerName() + ":" + request.getLocalPort() );
			if(details != null){
				this.isLoggedIn = true;
				session.setAttribute("userid", details.email);
			}
		}else if(userId != null){
			isLoggedIn = true;
		}
		
	}

	private BrowserIdVerificationResponse authenticateUser(String assertion, String audienceValue) throws HttpException, IOException{
		PostMethod post = new PostMethod("https://browserid.org/verify");
		List<NameValuePair> pairs = new ArrayList<NameValuePair>(2);
		
		post.addParameter("assertion", assertion);
		post.addParameter("audience", audienceValue);
		
		HttpClient client = new HttpClient();
		
		client.executeMethod(post);

		String response = post.getResponseBodyAsString();
		BrowserIdVerificationResponse details= gson.fromJson(response, BrowserIdVerificationResponse.class);

		
        if (details.status != null) {
            if ("okay".equals(details.status)) {
                    if (details.email != null) {
                    	return details;
                    }
            }
        }
		
		return null;
	}

	public boolean isLoggedIn(){
		return this.isLoggedIn;
	}
	
	
	@Override
	public String getServiceName() {
		return SERVICE_NAME;
	}

	@Override
	public String getViewPath() {
		return this.viewPath;
	}
	
	
	private static class BrowserIdVerificationResponse {

        String status;
        String reason;
        String email;
        String audience;
        Long validUntil;
        String issuer;
}

	

}
