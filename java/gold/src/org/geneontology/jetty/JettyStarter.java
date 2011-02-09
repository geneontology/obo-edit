package org.geneontology.jetty;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.geneontology.web.AdminServlet;
import org.geneontology.web.services.ServicesConfig;
import org.mortbay.jetty.Handler;
import org.mortbay.jetty.Server;
import org.mortbay.jetty.handler.ContextHandlerCollection;
import org.mortbay.jetty.handler.DefaultHandler;
import org.mortbay.jetty.handler.HandlerCollection;
import org.mortbay.jetty.servlet.Context;
import org.mortbay.jetty.servlet.ServletHolder;
import org.mortbay.jetty.webapp.WebAppContext;

import org.geneontology.conf.GeneOntologyManager;

public class JettyStarter {

	private static Logger LOG = Logger.getLogger(JettyStarter.class);

	private static Server server;

	private JettyStarter() {

	}

	public void start() throws Exception {
		
		GeneOntologyManager manager = GeneOntologyManager.getInstance();
		int jetty_port = manager.getJettyPort();
		
		server = new Server(jetty_port);
		WebAppContext files = new WebAppContext("webcontents", "/");
		
		ContextHandlerCollection contexts = new ContextHandlerCollection();
	
	//	server.setHandler(contexts);

		
		
		Context root = new Context(contexts, "/gold", Context.SESSIONS);
		root.addServlet(new ServletHolder(new AdminServlet()), "/*");

	      HandlerCollection handlers = new HandlerCollection();
	      handlers.setHandlers(new Handler[]{contexts,files, new DefaultHandler()});		
		
		server.setHandler(handlers);
		ShutdownMonitor monitor = new ShutdownMonitor();
		
		
		monitor.start();
		
		server.start();

		LOG.info("Jetty Server is started");
		LOG.info("Please visit the web application at the url : http://localhost:" + jetty_port + "/");
		
		LOG.info("Initializing Services which include loading ontologies in memory. This step may take several mintues. ");
		LOG.info("WAIT.............");
		ServicesConfig.getServices();
		
		LOG.info("Services are initialized. The server is ready for performing services");
		
		server.join();
	}

	public void stop() throws Exception {

		Socket s = new Socket(InetAddress.getByName("127.0.0.1"), 8079);
		OutputStream out = s.getOutputStream();
		LOG.info("sending jetty stop request");
		out.write(("\r\n").getBytes());
		out.flush();
		s.close();

	}

	private static void exit(String message){
		System.err.println(message);
		usage();
		System.exit(0);
		
	}
	
	
	private static void usage(){
		System.out.println("Descripton:");
			System.out.println("\tJetty start or stop utility");
		
		System.out.println("Syntax:");
			System.out.println("\tstart");
			System.out.println("\tstop");
	}
	
	public static void main(String args[]) throws Exception {

		System.out.println("**************Jetty Utility**************");
		
		Logger.getRootLogger().setLevel(Level.INFO);
		
		if(args.length ==0){
			exit("Invalid Arguments");
		}
		
		String command = args[0];
		
		if("start".equals(command)){
			new JettyStarter().start();
		}else if ("stop".equals(command)){
			new JettyStarter().stop();
		}else{
			exit("Invalid options");
		}
		
	}

	private class ShutdownMonitor extends Thread {

		private ServerSocket socket;

		public ShutdownMonitor() {
			setDaemon(true);
			setName("JettyShutdownMonitor");

			try {
				socket = new ServerSocket(8079, 1,
						InetAddress.getByName("127.0.0.1"));
			} catch (Exception e) {
				throw new RuntimeException(e);
			}

		}

		@Override
		public void run() {
			LOG.info("running jetty 'stop' thread");
			Socket accept;
			try {
				accept = socket.accept();
				BufferedReader reader = new BufferedReader(
						new InputStreamReader(accept.getInputStream()));
				reader.readLine();
				LOG.info("stopping jetty embedded server");
				server.stop();
				accept.close();
				socket.close();
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}
	}

}
