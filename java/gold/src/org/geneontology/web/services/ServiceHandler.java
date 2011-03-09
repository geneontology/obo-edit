package org.geneontology.web.services;

import java.io.IOException;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.geneontology.web.AdminServlet;

/**
 * The implementation of this interface is called by {@link AdminServlet}.
 * @author Shahid Manzoor
 *
 */
public interface ServiceHandler {

	/**
	 * This service performs the computations and stores the computations results via request.setAttribute method.
	 * This request object attributes values are visible in the jsp file associated with this service. 
	 * The jsp file prints the values during its rendering process.   
	 */
	public void handleService(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException;
	public String getServiceName();
	
	/**
	 * 
	 * @return It returns the path of the jsp file which is executed after the execution of the handleService method
	 */
	public String getViewPath();
	
}
