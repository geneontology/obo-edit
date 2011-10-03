/**
 * The web interface of GOLD is implemented along the lines of Model View Controller (MVC) architecture. The package contains classes of which perform
 * the controller functionality. On each user request with the URL ending /gold the {@link org.geneontology.web.services.AdminServlet} servlet is invoked. 
 * The servlet get the service {@link org.geneontology.web.services.ServiceHandlerAbstract} whose name is mentioned in the request parameters.
 * A service runs a user request (sent from a web client) asynchronously (as a Thread). When the client sends a request 
 * for the first time to run a process, after running the service a session is sent to the client which is associated with the service.
 * At fix internals the web browsers keeps sending request along with the session id to check the status of the process running in the service until
 * the process is not finished. The service running status information and the service computation results are 
 * set in a request object (refers as model in this architecture). Finally the request response is sent to browser through a view (jsp file).
 * 
 * To create a service a class need to extend {@link org.geneontology.web.services.ServiceHandlerAbstract} class. See {@link org.geneontology.web.services.InitializationService}
 * , {@link org.geneontology.web.services.ReasoningService} and {@link org.geneontology.web.services.GafDbOperationsService} for more detail to learn about
 * how to implement a service.
 * 
  * @author Shahid Manzoor
 */

package org.geneontology.web.services;

