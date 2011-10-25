package org.geneontology.web.services;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import org.apache.commons.configuration.PropertiesConfiguration;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.NameValuePair;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.log4j.Logger;
import org.geneontology.jetty.JettyStarter;

import com.google.gson.Gson;

public class LoginService extends ServiceHandlerAbstract {

	private static final Gson gson = new Gson();	
	
	public static final String SERVICE_NAME="login-service";
	
	private String viewPath;
	
	private static Logger LOG = Logger.getLogger(LoginService.class);
	
	private static boolean DEBUG = LOG.isDebugEnabled();
	
	private boolean isLoggedIn;
	
	private static Hashtable<String, List<String>> servicesWithSecurity;
	
	static{

		try{
			File f = new File("conf/security.properties");

			servicesWithSecurity = new Hashtable<String, List<String>>();
			
		//	PropertyListConfiguration root = new PropertyListConfiguration(f);
			
		//	Configuration config = root.subset("geneontology.gold.security.service");
			PropertiesConfiguration config = new PropertiesConfiguration(f);
			
			String prefix = "geneontology.gold.security.service";
			HashSet<String> servicesIndexes = new HashSet<String>();
			Iterator itr = config.getKeys(prefix);
			while(itr.hasNext()){
				String key = (String)itr.next();
				int index = key.indexOf(".", prefix.length()+1);
				String servicePrefix = key.substring(prefix.length()+1, index);
				
				if(DEBUG)
					LOG.debug(servicePrefix);
				
				servicesIndexes.add(servicePrefix);
			}
			
			for(String index: servicesIndexes){
				String key = prefix + "." + index + ".name";
				String serviceName = config.getString(key);
				
				if(serviceName != null){
					List<String> list = new ArrayList<String>();
					servicesWithSecurity.put(serviceName.trim(), list);
					for(Object restriction: config.getList(prefix + "." + index + ".restriction")){
						list.add(((String)restriction).trim());
					}
				}
			}
		
			if(DEBUG){
				LOG.debug(servicesWithSecurity.keySet());
				LOG.debug(servicesWithSecurity.values());
			}
			
			
		}catch(Exception ex){
			LOG.error("Error in reading conf/security.xml file", ex);
		}
		
		
	}
	
	public LoginService(){
	}
	
	
	@Override
	public void handleService(HttpServletRequest request,
			HttpServletResponse response) throws IOException, ServletException {
	
	
		HttpSession session = request.getSession(true);
		
		this.viewPath = "/servicesui/login.jsp";

		//If the server is running in the deveopment environment then 
		//skip the security.
		if(JettyStarter.getInstance().isDevelopmentInstanceRunning()){
			this.isLoggedIn = true;
			return;
		}
		
		String servicName = request.getParameter("servicename");

		if(servicName == null){
			this.isLoggedIn = true;
			return;
		}
		
		List<String> commands= servicesWithSecurity.get(servicName);
		
		if(commands == null){
			this.isLoggedIn = true;
			return;
		}
		
		String command = request.getParameter("command");
		
		if(!commands.contains("*") && (command == null || !commands.contains(command))){
			this.isLoggedIn = true;
			return;
		}
		
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
