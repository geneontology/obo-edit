package org.geneontology.web;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

/*import net.sf.ehcache.Cache;
import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Element;
import net.sf.ehcache.config.CacheConfiguration;
import net.sf.ehcache.store.MemoryStoreEvictionPolicy;
*/
import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.NullArgumentException;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.web.services.GafDbOperationsService;
import org.geneontology.web.services.GoldDbOperationsService;
import org.geneontology.web.services.InitializationService;
import org.geneontology.web.services.ReasoningService;
import org.geneontology.web.services.ServiceHandler;
import org.geneontology.web.services.ServicesFactory;


/**
 * This servlet is the main entry point of the web interface of the 
 * Gold project.
 * All the different operations are implemented as Services
 * (see {@link ServiceHandler}. 
 * This serlvet expects the 'service-name'  required
 * request parameter. It finds the service object via service name in the
 *  {@link ServicesConfig} class object. It calls the handle service method of the service object.
 *  The service object is supposed to do 
 * computations and store the results in the request object via 
 *  setAttribute method.
 * The service also sets the view name (a jsp file) which renders the results.
 *  In the last step the servlet forwards the request to the jsp file.
 * 
 * See also {@link GoldDbOperationsService}, {@link ReasoningService} and {@link GafDbOperationsService}
 * @author Shahid Manzoor
 *
 */
public class AdminServlet extends HttpServlet {

	
/*	private static Cache servicesCache = createServicesCache();
	
	private static Cache createServicesCache(){
		
		CacheManager manager = new CacheManager();
		
		Cache cache = new Cache(
			     new CacheConfiguration("servicesCache", 1000)
			       .memoryStoreEvictionPolicy(MemoryStoreEvictionPolicy.LFU)
			       .overflowToDisk(false)
			       .eternal(false)
			       .timeToLiveSeconds(0)
			       .timeToIdleSeconds(300)
			       .diskPersistent(false)
			       .diskExpiryThreadIntervalSeconds(0));		
		
		manager.addCache(cache);
		return cache;
	}*/
	
	private ServicesCache servicesCache;
	
	public AdminServlet(){
		servicesCache = new ServicesCache(300);
	}
	
	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		
		if(ServletFileUpload.isMultipartContent(request)){
			response.getWriter().println("File uload");
			
			try{
				ServletFileUpload servletFileUpload = new ServletFileUpload(new DiskFileItemFactory());
				List fileItemsList = servletFileUpload.parseRequest(request);		
				
				Iterator it = fileItemsList.iterator();
				
				FileItem uploadFile = null;
				Hashtable<String, String> parameters = new Hashtable<String, String>();
				String query = "";
				  while (it.hasNext()){
				    FileItem fileItemTemp = (FileItem)it.next();
				    if (fileItemTemp.isFormField()){
				    	response.getWriter().println(fileItemTemp.getFieldName());
				    	//query += fileItemTemp.getFieldName() + "=" + URLEncoder.encode(fileItemTemp.getString(), "UTF-8") + "&";
				    	parameters.put(fileItemTemp.getFieldName(), fileItemTemp.getString());
				    }else{
				    	uploadFile = fileItemTemp;
				    }
				  }
				  
				  if(uploadFile != null){
					  String fileName = uploadFile.getName();
					  fileName = FilenameUtils.getName(fileName);
					  
					  String dirName = GeneOntologyManager.getInstance().getGafUploadDir();
					  
					  File file = new File(dirName, fileName);
					  
					//  query += "filepath=" + URLEncoder.encode(file.getAbsolutePath(), "UTF-8");
					  parameters.put("filelocation", file.getCanonicalPath());
					  
					  
					  uploadFile.write(file);
					  
					  
				  }
				  
				  request.setAttribute("parameters", parameters);
				
					ServletContext context = getServletContext().getContext("/");
					RequestDispatcher dispatcher = context.getRequestDispatcher("/servicesui/uploadfile.jsp");
					dispatcher.forward(request, response);
				  
				  
			}catch(Exception ex){
				throw new ServletException(ex);
			}
			
			
		}else{
			doGet(request, response);
		}
		
	}

	
	
	@Override
	protected void doGet(HttpServletRequest request,
			HttpServletResponse response)
			throws ServletException, IOException {

		HttpSession session = request.getSession(true);
		//servicesCache.flush();
	//	System.out.println("cache..........................."+servicesCache.getKeys());
		
		InitializationService initHandler =(InitializationService) ServicesFactory.getInstance().createServiceHandler(InitializationService.SERVICE_NAME);
		initHandler.handleService(request, response);
		String view = initHandler.getViewPath();
		String id = request.getParameter("id");
		ServiceHandler handler = null;
		if(initHandler.isInitialized()){
			//get the service name from the parameter
			String servicename= request.getParameter("servicename");
			
			String error = null;
			
			if(servicename == null){
				error = "servicename parameter is missing in the parameter";
			}else if(id != null && servicesCache.get(id) == null){
					error = "The service session id '" + id + "' is expired. Please call this service wihout id parameter to run a new task.";
			}else if(id != null){
				handler =  servicesCache.get(id);
			}else{
				handler = ServicesFactory.getInstance().createServiceHandler(servicename);
				if(handler == null){
					error = "The service '"+ servicename + "' is not supported by the server";
				}else{
					servicesCache.put(session.getId(), handler);
				}
			}
			
			//find the service object
			if(handler != null){
				handler.handleService(request, response);
				view = handler.getViewPath();
			}
			
			if(error != null){
				view = "/servicesui/error.jsp";
				request.setAttribute("error", error);
			}
		}
		
			
		//forwarding the request to a jsp file reffered in the 'view' variable
		if(view != null){
			ServletContext context = getServletContext().getContext("/");
			RequestDispatcher dispatcher = context.getRequestDispatcher(view);
			dispatcher.forward(request, response);
		}
		
	
	}
	
	private class ServicesCache extends TimerTask{

		private Map<String, Element> cache;
		
		/**
		 * Time in seconds. 
		 */
		private int timeToIdleInSeconds;
		
		private Timer timer;
		
		public ServicesCache(int timeToIdleInSeconds){
			cache = new Hashtable<String, Element>();
			cache = Collections.synchronizedMap(cache);
			this.timeToIdleInSeconds = timeToIdleInSeconds;
			
			timer = new Timer();
			timer.scheduleAtFixedRate(this, Calendar.getInstance().getTime(), 1000*60*1);
		}
		
		@Override
		public void run() {

			
			List<String> toRemove = new ArrayList<String>();
			
			for(String key: cache.keySet()){
				Element e = cache.get(key);
				long l = (System.currentTimeMillis() - e.time)/1000;
				if(l>=timeToIdleInSeconds)
					toRemove.add(key);
			}
			
			
			for(String key: toRemove){
				cache.remove(key);
			}
			
		}
		
		public void put(String id, ServiceHandler handler){
			if(handler == null){
				throw new NullArgumentException("Service handler cann't be null");
			}
			Element e = new Element(handler, System.currentTimeMillis());
			cache.put(id, e);
		}
		
		public ServiceHandler get(String id){
			Element e = cache.get(id);
			if(e != null){
				long l = (System.currentTimeMillis() - e.time)/1000;
				if(l<timeToIdleInSeconds){
					e.time = System.currentTimeMillis();
					return e.handler;
				}
			}
			
			return null;
				
		}
		
		private class Element{
			
			private Element(ServiceHandler handler, long time){
				this.handler = handler;
				this.time = time;
			}
			
			ServiceHandler handler;
			long time;
		}
	}
	
}
