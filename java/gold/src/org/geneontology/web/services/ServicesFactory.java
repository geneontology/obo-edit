package org.geneontology.web.services;

import java.util.Hashtable;

/**
 * This is a singlton class. This class maintains the collections of 
 * services (implements the {@link ServiceHandler} interface). The services method handleService
 * is called by servlet.
 * @author Shahid Manzoor
 *
 */

public class ServicesFactory {

	//implement cache
	
	private static ServicesFactory factory;
	
//	private static Hashtable<String, ServiceHandler> services;
	
	private static InitializationService initService = new InitializationService();
	
	private static ReasoningService reasoningService = new ReasoningService();
	
	private ServicesFactory(){
	//	services = new Hashtable<String, ServiceHandler>();
	}
	
	/*private static Hashtable<String, ServiceHandler> services = buildServices();
	
	private static Hashtable<String, ServiceHandler> buildServices(){

		Hashtable<String, ServiceHandler> table = new Hashtable<String, ServiceHandler>();
		
		ServiceHandler service = new GoldDbOperationsService();
		table.put(service.getServiceName(), service);

		service = new GafDbOperationsService();
		table.put(service.getServiceName(), service);
		
		service = new ReasoningService();
		table.put(service.getServiceName(), service);
		
		service = new InitializationService();
		table.put(service.getServiceName(), service);
		
		service = new OntSolrjLoaderService();
		table.put(service.getServiceName(), service);
		
		return table;
	}
	
	public static ServicesConfig getServicesConfig(){
		if(servicesConfig == null){
			servicesConfig = new ServicesConfig();
		}
		
		return servicesConfig;
	}
	
	public static ServiceHandler getService(String name){
		
		if(name == null)
			return null;
		
		ServiceHandler service = services.get(name);
		
		return service;
	}
	
	public static Collection<ServiceHandler> getServices(){
		return services.values();
	}*/

	/*
	public ServiceHandler getServiceHandler(String id){
		if(id == null)
			return null;
		
		 ServiceHandler service = services.get(id);
		
		return service;
	}*/
	

	public ServiceHandler createServiceHandler(String name){
		if(name == null)
			return null;
		
		if(InitializationService.SERVICE_NAME.equals(name)){
			return initService;
		}else if (GafDbOperationsService.SERVICE_NAME.equals(name)){
			return new GafDbOperationsService();
		}else if (GoldDbOperationsService.SERVICE_NAME.equals(name)){
			return new GoldDbOperationsService();
		}else if(ReasoningService.SERVICE_NAME.equals(name)){
			return reasoningService;
		}
		
		return null;
	}
	
	
	public static ServicesFactory getInstance(){
		if(factory == null){
			factory = new ServicesFactory();
		}
		
		return factory;
	}
	
}
