package org.geneontology.web.services;

/**
 * This factory object builds services (instanceof {@link ServiceHandler}) objects.
 * @author Shahid Manzoor
 *
 */

public class ServicesFactory {

	private static ServicesFactory factory;
	
	private static InitializationService initService = new InitializationService();
	
	private static ReasoningService reasoningService = new ReasoningService();
	
	private ServicesFactory(){
	//	services = new Hashtable<String, ServiceHandler>();
	}
	
	/**
	 * This method return a service object 
	 * @param name
	 * @return
	 */
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
		}else if(PhyloTreeLoaderService.SERVICE_NAME.equals(name)){
			return new PhyloTreeLoaderService();
		}else if(LoginService.SERVICE_NAME.equals(name)){
			return new LoginService();
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
