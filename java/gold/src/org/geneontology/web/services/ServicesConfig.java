package org.geneontology.web.services;

import java.util.Collection;
import java.util.Hashtable;
import java.util.Set;
import java.util.UUID;

import org.apache.commons.configuration.PropertiesConfiguration;

public class ServicesConfig {

	private static Hashtable<String, ServiceHandler> services = buildServices();
	
	
	private static void addService(Hashtable<String, ServiceHandler> table, PropertiesConfiguration props, ServiceHandler service){
		table.put(service.getServiceName(), service);
		
	}
	
	private static Hashtable<String, ServiceHandler> buildServices(){

		Hashtable<String, ServiceHandler> table = new Hashtable<String, ServiceHandler>();
		
		try{
			

			//add Service
			
		}catch(Exception ex){
			ex.printStackTrace();
		}
		
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
