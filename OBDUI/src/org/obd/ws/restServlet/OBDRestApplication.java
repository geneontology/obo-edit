package org.obd.ws.restServlet;

import java.util.Map;
import org.obd.query.Shard;
import org.obd.query.impl.MultiShard;
import org.restlet.Application;
import org.restlet.Context;
import org.restlet.Directory;
import org.restlet.Restlet;
import org.restlet.Router;
import org.restlet.resource.Resource;

public class OBDRestApplication extends Application {

	protected Shard shard;	
	private ServletConfiguration configuration;
	public Map<String, MultiShard> resourceMap; 
	private OBDRestServerServlet serverServlet;
	
	
	private final String servletConfigFile = "obdRestServletConfig.xml";
	
    public OBDRestApplication() {  	
        super();
    }
   	   
    public OBDRestApplication(Context context) throws Exception {
    	super(context);
    	this.setConfiguration(new ServletConfiguration());
    }

    @Override
    public Restlet createRoot() {
    	String contextRootPath = this.getServerServlet().getRealPath("/");
    	String servletConfigPath = contextRootPath + this.servletConfigFile;
    	this.getConfiguration().validateConfig(servletConfigPath);
    	
    	try {
			this.getConfiguration().configureFromFile(servletConfigPath,this.getContext(),contextRootPath);
		} catch (Exception e) {
			System.err.println("Unable to configure servlet: " + e.getMessage());
			e.printStackTrace();
			return null;
		}
    	
        this.resourceMap = this.getConfiguration().getDataSources();
    	System.err.println("Available data sources:");
    	for (String resourcePath : this.resourceMap.keySet()){	
    		System.out.println("\tDATA SOURCE:" + resourcePath);
    	}
    	
    	Router router = new Router(getContext());
    	//router.attach("/", HomeResource.class).getTemplate().setMatchingMode(org.restlet.util.Template.MODE_EQUALS);

    	for (String path : this.getConfiguration().getPathResourceMap().keySet()){
    		Class<?extends Resource> c = this.configuration.getPathResourceMap().get(path);
    		System.out.println("Attaching resource " + c.getSimpleName() + " to path " + path);
    		router.attach(path,c);
    	}
    	
    	for (String path : this.configuration.getPathDirectoryMap().keySet()){
    		Directory d = this.configuration.getPathDirectoryMap().get(path);
    		System.out.println("Attaching directory " + d.getRootRef().getPath() + " to path " + path);
    		router.attach(path,d);
    	}
    	return router;
    }


	public Shard getShard(String resourceName){
		return this.resourceMap.get(resourceName);
	}

	public OBDRestServerServlet getServerServlet() {
		return serverServlet;
	}

	public void setServerServlet(OBDRestServerServlet serverServlet) {
		this.serverServlet = serverServlet;
	}

	public void setConfiguration(ServletConfiguration configuration) {
		this.configuration = configuration;
	}

	public ServletConfiguration getConfiguration() {
		return configuration;
	}

	public Map<String, MultiShard> getResourceMap() {
		return resourceMap;
	}

	public void setResourceMap(Map<String, MultiShard> resourceMap) {
		this.resourceMap = resourceMap;
	}

}