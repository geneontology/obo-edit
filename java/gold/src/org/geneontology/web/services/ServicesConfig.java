package org.geneontology.web.services;

import java.util.Collection;
import java.util.Hashtable;

public class ServicesConfig {

	private static Hashtable<String, ServiceHandler> services = buildServices();
	
	
	private static Hashtable<String, ServiceHandler> buildServices(){

		Hashtable<String, ServiceHandler> table = new Hashtable<String, ServiceHandler>();
		
	//	ServiceHandler service = new DbOperationsService();
		
		ServiceHandler service = new GoldDbOperationsService();
		table.put(service.getServiceName(), service);

		service = new GafDbOperationsService();
		table.put(service.getServiceName(), service);
		
		service = new ReasoningService();
		table.put(service.getServiceName(), service);
		
		table.put(service.getServiceName(), service);
		
		
		return table;
	}
	
	public static ServiceHandler getService(String name){
		
		if(name == null)
			return null;
		
		ServiceHandler service = services.get(name);
		
		return service;
	}
	
	public static Collection<ServiceHandler> getServices(){
		return services.values();
	}

	
	
}
