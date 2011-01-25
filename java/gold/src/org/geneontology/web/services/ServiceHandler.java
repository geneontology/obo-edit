package org.geneontology.web.services;

import java.io.IOException;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


public interface ServiceHandler {

	public void handleService(HttpServletRequest request, HttpServletResponse response) throws IOException;
	public String getServiceName();
	
}
