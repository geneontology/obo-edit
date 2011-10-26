package org.geneontology.web.services;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import org.apache.commons.configuration.PropertiesConfiguration;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.NameValuePair;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.log4j.Logger;
import org.geneontology.jetty.JettyStarter;
import com.google.gson.Gson;

public class AuthenticationFilter implements Filter {

	private static Logger LOG = Logger.getLogger(AuthenticationFilter.class);
	private static boolean DEBUG = LOG.isDebugEnabled();
	
	private FilterConfig filterConfig;
	
	private Hashtable<String, List<String>> servicesWithSecurity;
	
	@Override
	public void destroy() {
		// TODO Auto-generated method stub

	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response,
			FilterChain chain) throws IOException, ServletException {

		HttpServletRequest httpRequest = (HttpServletRequest) request;
		
		HttpSession session = httpRequest.getSession(true);

		boolean autherized = false;
		
		//String pathInfo = httpRequest.getPathInfo();
		
	//	this.viewPath = "/servicesui/login.jsp";
		Object userId = session.getAttribute("userid");
		
		/*if(pathInfo != null && pathInfo.endsWith("/login.jsp")){
			autherized = true;
		}else */if(userId != null){
			autherized = true;
		//If the server is running in the deveopment environment then 
		//skip the security.
		}else if(JettyStarter.getInstance().isDevelopmentInstanceRunning()){
			autherized = true;
		}else{
		
			String servicName = request.getParameter("servicename");
	
			if(servicName == null){
				autherized = true;
			}else{

				List<String> commands= servicesWithSecurity.get(servicName);
				String command = request.getParameter("command");
				
				if(commands == null){
					autherized = true;
				}else if(!commands.contains("*") && (command == null || !commands.contains(command))){
					autherized = true;
				}
			}
		}
		
		
		if(!autherized){
			//chain.doFilter(request, response);
		//}else{
		
			String assertion = request.getParameter("assertion");
			//user is not logged-in uet
			if(userId == null && assertion != null) {
				BrowserIdVerificationResponse details=	authenticateUser(assertion, request.getServerName() + ":" + request.getLocalPort() );
				if(details != null){
					autherized = true;
					session.setAttribute("userid", details.email);
				}
			}
			
		//	if(filterConfig != null){
				//ServletContext context = filterConfig.getServletContext();
			//request.getRequestDispatcher("/servicesui/login.jsp").forward(request, response);
		//	}
			
			//forward to login.jsp
		}
		
		if(autherized){
			request.setAttribute("isAuthenticated", new Boolean(true));
		}
		
		chain.doFilter(request, response);
		
	}

	private BrowserIdVerificationResponse authenticateUser(String assertion, String audienceValue) throws HttpException, IOException{
		PostMethod post = new PostMethod("https://browserid.org/verify");
		List<NameValuePair> pairs = new ArrayList<NameValuePair>(2);
		
		post.addParameter("assertion", assertion);
		post.addParameter("audience", audienceValue);
		
		HttpClient client = new HttpClient();
		
		client.executeMethod(post);

		String response = post.getResponseBodyAsString();
		Gson gson = new Gson();
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
	
	
	@Override
	public void init(FilterConfig filterConfig) throws ServletException {

		if(DEBUG)
			LOG.debug("--");
	
		this.filterConfig = filterConfig;
			
		System.out.println("Init method is called");
			
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

	
	private static class BrowserIdVerificationResponse {

        String status;
        String reason;
        String email;
        String audience;
        Long validUntil;
        String issuer;
	}
	
	
}
