package org.geneontology.web;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.Writer;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Servlet implementation class AdminServlet
 */
public class AdminServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public AdminServlet() {
        super();
        // TODO Auto-generated constructor stub
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		

		Writer writer = response.getWriter();

		writer.write("<h1>Hello World</h1>");
		
		String command = request.getParameter("command");

		if(command != null){
		
			if("bulk-load".equals(command)){
				writer.write("<h1>Bulk load is command is provided. This ia temporary message. This message will be replaced as soon as this service is implemented.</h1>");
				return;
			}
		}
		
			
			writer.write("<h1>No valid parameters are provided. Please call this page with valid parameters</h1>");
			
			
		
		

	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
	}

}
