package org.geneontology.web.services;

import java.io.File;
import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.log4j.Logger;
import org.geneontology.gaf.io.GAFDbOperations;
import org.geneontology.gold.io.DbOperationsListener;
import org.geneontology.gold.io.PhyloTreeLoader;
import org.geneontology.web.Task;

public class PhyloTreeLoaderService extends ServiceHandlerAbstract {

	private static Logger LOG = Logger.getLogger(PhyloTreeLoaderService.class);

	public static final String SERVICE_NAME = "phylo-tree-loader";


	/**
	 * The GAF files paths
	 */
	private Object fileLocations;

	/**
	 * The thread which runs the bulkload and update operations in background.
	 */
	private DbTaskExecution runner;

	/**
	 * It holds the value of the 'force' request parameter. The parameter is
	 * used in bulkload. If the value is true then delete gold database tables
	 * and create from scratch.
	 */

	private String viewPath;

	private static boolean updateInProgress;
	

	/**
	 * Each GAF operation runs a thread in background. The web browser calls the
	 * handle service method in intervals to check whether the operation is
	 * completed or not. On the last call all the variables are reset. But there
	 * are situations when the output is other than the html and in those
	 * situation the service for a specific operation will be called once so in
	 * those cases after the end of operation variables reset.
	 */
	// private boolean noReloadMode;

	public PhyloTreeLoaderService() {
		runner = null;
	}

	public void handleService(HttpServletRequest request,
			HttpServletResponse response) throws IOException, ServletException {

		try {

//			String command = request.getParameter("command");
//			String remoteLocation = request.getParameter("remote-file");
			String fileLocation = request.getParameter("filelocation");

			// set the default view
			viewPath = "/servicesui/phylotreeloader.jsp";

			/**
			 * The below condition sets phylotree documents location
			 */
			if (runner == null) {

				this.fileLocations = fileLocation;
				if (!(fileLocation != null && (fileLocation.startsWith("http:") || fileLocation.startsWith("ftp:") || fileLocation.startsWith("file:"))) ){
					File f = new File(fileLocation);
					fileLocations = f.toURI().toString();
				}	
				
				if(!(updateInProgress)){
					runner = new DbTaskExecution();
					runner.start();
				}
				
				updateInProgress = true;

			}

		} finally {

			// store information in the request object. The request object is
			// available in the
			// jsp file. The jsp use the objects data in print html.

			if (runner != null) {
				request.setAttribute("task", runner);
				request.setAttribute("exception", runner.getException());
			}
			request.setAttribute("isTaskRunning", runner == null ? false
					: runner.isRunning());

			// if the task has completed its operation then set it to null
			if (runner != null && !runner.isRunning()) {
				runner = null;
			}

		}

	}

	public static boolean isDbUpdateInProgress(){
		return updateInProgress;
	}
	
	public String getServiceName() {
		return SERVICE_NAME;
	}

	public String getViewPath() {
		return this.viewPath;
	}

	/**
	 * The execute method is called by the {@link Task} class. This class
	 * executes the update and bulkload methods of {@link GAFDbOperations}. The
	 * implemented listener methods of the {@link DbOperationsListener}
	 * interface are called {@link GAFDbOperations}. The listener methods keep
	 * stores start and end time of sub task of the the bulkload and update
	 * operation. The subtasks completion time is printed by the jsp associated
	 * with the GafDbOperationsService class.
	 * 
	 * @author Shahid Manzoor
	 * 
	 */
	class DbTaskExecution extends Task{


		public DbTaskExecution() {
			this.data = this;
		}

		
		@Override
		public void execute() {


			try {
				
				reportStartTime("Loading");
				PhyloTreeLoader loader = new PhyloTreeLoader();
				loader.setSource((String)fileLocations );
				loader.loadThrow();

				reportEndTime("Loading");


			} catch (Throwable ex) {
				this.exception = ex;
				LOG.error(ex, ex);

			}finally{
				updateInProgress = false;
			}
		}


		protected void reportStartTime(String name) {
			this.addInProgress(name);
		}

		protected void reportEndTime(String name) {
			this.addCompleted(name);
		}



	}

}
