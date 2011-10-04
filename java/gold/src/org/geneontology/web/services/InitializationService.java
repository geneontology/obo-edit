package org.geneontology.web.services;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;
import org.geneontology.gold.io.postgres.SchemaManager;

/**
 * This service initializes the database which includes creating database and creating schema.
 * @author Shahid Manzoor
 *
 */
public class InitializationService extends ServiceHandlerAbstract {

	private static Logger LOG = Logger.getLogger(InitializationService.class);
	
	public static final String SERVICE_NAME="initialization";
	
	private boolean isInitialized;
	
	private String viewPath;
	
	private Task runner;
	
//	@Override
	public void handleService(HttpServletRequest request,
			HttpServletResponse response) throws IOException, ServletException {

		this.viewPath = "/index.html";
		
		if(this.isInitialized){
			return;
		}
		
		SchemaManager sm = new SchemaManager();
		
		
		try{
			this.isInitialized = sm.isDatabaseInitialed();
		}catch(Exception ex){
			LOG.error(ex.getMessage(), ex);
		}
		if(!isInitialized){
			this.viewPath = "/servicesui/initialize.jsp";
			
			
			if(runner == null){
				if(runner == null){
					runner = new InitializationTask();
					runner.start();
				}
			}
			
			request.setAttribute("isTaskRunning", runner == null ? false: runner.isRunning());
			
			if(runner != null && !runner.isRunning()){
				runner = null;
			}

		}
		
	}

	public boolean isInitialized(){
		return this.isInitialized;
	}
	
//	@Override
	public String getServiceName() {
		return SERVICE_NAME;
	}

//	@Override
	public String getViewPath(){
		return this.viewPath;
	}
	
	/**
	 * Thread runs in background to load schema into the database
	 * @author Shahid Manzoor
	 *
	 */
	class InitializationTask extends Task{

		@Override
		public void execute() {
			try{
				SchemaManager sm = new SchemaManager();
				sm.loadSchemaSQL();
				
			}catch(Exception ex){
				//exception = ex;
				this.exceptions.add(ex);
			}
		}
		
		
		@Override
		public Object getData(){
			return null;
		}
		
	}
	

}
