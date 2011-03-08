package org.geneontology.web;

import java.io.IOException;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.geneontology.web.services.GafDbOperationsService;
import org.geneontology.web.services.GoldDbOperationsService;
import org.geneontology.web.services.ReasoningService;
import org.geneontology.web.services.ServiceHandler;
import org.geneontology.web.services.ServicesConfig;

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

	@Override
	protected void service(HttpServletRequest request,
			HttpServletResponse response)
			throws ServletException, IOException {

		//get the service name from the parameter
		String servicename= request.getParameter("servicename");
		
		String error = null;
		
		if(servicename == null){
			error = "servicename parameter is missing in the parameter";
		}
		
		//find the service object
		ServiceHandler handler= ServicesConfig.getService(servicename);	
		String view = null;
		if(handler == null){
			error = "The service '"+ servicename + "' is not supproted by the server";
		}else{
			
			handler.handleService(request, response);
			view = handler.getViewPath();
		}
		
		if(error != null){
			view = "/servicesui/error.jsp";
			request.setAttribute("error", error);
		}
		
		//forwarding the request to a jsp file reffered in the 'view' variable
		if(view != null){
			ServletContext context = getServletContext().getContext("/");
			RequestDispatcher dispatcher = context.getRequestDispatcher(view);
			dispatcher.forward(request, response);
		}
	
	}


}
