package org.geneontology.jetty;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.eclipse.jetty.deploy.DeploymentManager;
import org.eclipse.jetty.deploy.providers.WebAppProvider;
import org.eclipse.jetty.server.Connector;
import org.eclipse.jetty.server.Handler;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.handler.ContextHandlerCollection;
import org.eclipse.jetty.server.handler.DefaultHandler;
import org.eclipse.jetty.server.handler.HandlerCollection;
import org.eclipse.jetty.server.handler.RequestLogHandler;
import org.eclipse.jetty.server.handler.StatisticsHandler;
import org.eclipse.jetty.server.nio.SelectChannelConnector;
import org.eclipse.jetty.util.thread.QueuedThreadPool;
import org.eclipse.jetty.webapp.WebAppContext;
import org.geneontology.web.services.GoldDbOperationsService;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gold.io.FileMonitor;
import org.geneontology.gold.io.postgres.SchemaManager;

/**
 * This class responsible for start and stop of the jetty server.
 * @author Shahid Manzoor
 *
 */

public class JettyStarter {

	private static Logger LOG = Logger.getLogger(JettyStarter.class);

	private static Server server;

	private JettyStarter() {

	}

	public void start() throws Exception {
		
		GeneOntologyManager manager = GeneOntologyManager.getInstance();
		int jetty_port = manager.getJettyPort();
		
		
        String jetty_home = System.getProperty("jetty.home",".");
        System.setProperty("jetty.home",jetty_home);

        server = new Server(jetty_port);
//        server.setDumpAfterStart(true);
 //       server.setDumpBeforeStop(true);
    
        /*
        // Setup JMX
        MBeanContainer mbContainer=new MBeanContainer(ManagementFactory.getPlatformMBeanServer());
        server.getContainer().addEventListener(mbContainer);
        server.addBean(mbContainer);
        mbContainer.addBean(Log.getLog());
		*/
        
        // Setup Threadpool
        QueuedThreadPool threadPool = new QueuedThreadPool();
        threadPool.setMaxThreads(100);
        server.setThreadPool(threadPool);

        // Setup Connectors
        SelectChannelConnector connector = new SelectChannelConnector();
        connector.setPort(jetty_port);
        connector.setMaxIdleTime(30000);
        connector.setConfidentialPort(8443);
        connector.setStatsOn(true);
        server.setGracefulShutdown(1000);
        
        server.setConnectors(new Connector[]
        { connector });

        /*SslSelectChannelConnector ssl_connector = new SslSelectChannelConnector();
        ssl_connector.setPort(8443);
        SslContextFactory cf = ssl_connector.getSslContextFactory();
        cf.setKeyStore(jetty_home + "/etc/keystore");
        cf.setKeyStorePassword("OBF:1vny1zlo1x8e1vnw1vn61x8g1zlu1vn4");
        cf.setKeyManagerPassword("OBF:1u2u1wml1z7s1z7a1wnl1u2g");
        cf.setTrustStore(jetty_home + "/etc/keystore");
        cf.setTrustStorePassword("OBF:1vny1zlo1x8e1vnw1vn61x8g1zlu1vn4");
        cf.setExcludeCipherSuites(
                new String[] {
                    "SSL_RSA_WITH_DES_CBC_SHA",
                    "SSL_DHE_RSA_WITH_DES_CBC_SHA",
                    "SSL_DHE_DSS_WITH_DES_CBC_SHA",
                    "SSL_RSA_EXPORT_WITH_RC4_40_MD5",
                    "SSL_RSA_EXPORT_WITH_DES40_CBC_SHA",
                    "SSL_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA",
                    "SSL_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA"
                });
        ssl_connector.setStatsOn(true);
        server.addConnector(ssl_connector);*/

      /*  Ajp13SocketConnector ajp = new Ajp13SocketConnector();
        ajp.setPort(8009);
        server.addConnector(ajp);
        */
        
        
        HandlerCollection handlers = new HandlerCollection();
        ContextHandlerCollection contexts = new ContextHandlerCollection();
        RequestLogHandler requestLogHandler = new RequestLogHandler();
        WebAppContext files = new WebAppContext("webcontents", "/");
        contexts.setHandlers(new Handler[]{files});
        handlers.setHandlers(new Handler[]
        { contexts, new DefaultHandler(), requestLogHandler, files });
        
        StatisticsHandler stats = new StatisticsHandler();
        stats.setHandler(handlers);
        
        server.setHandler(stats);

        // Setup deployers
        DeploymentManager deployer = new DeploymentManager();
        deployer.setContexts(contexts);
        server.addBean(deployer);   
        
        /*ContextProvider context_provider = new ContextProvider();
        context_provider.setMonitoredDirName(jetty_home + "/contexts");
        context_provider.setScanInterval(2);
        deployer.addAppProvider(context_provider);*/

        WebAppProvider webapp_provider = new WebAppProvider();
        webapp_provider.setMonitoredDirName(jetty_home + "/webcontents");
        webapp_provider.setParentLoaderPriority(false);
        webapp_provider.setExtractWars(true);
        webapp_provider.setScanInterval(2);
        webapp_provider.setDefaultsDescriptor(jetty_home + "/etc/webdefault.xml");
        webapp_provider.setContextXmlDir(jetty_home + "/contexts");
        deployer.addAppProvider(webapp_provider);
        
     /*   HashLoginService login = new HashLoginService();
        login.setName("Test Realm");
        login.setConfig(jetty_home + "/etc/realm.properties");
        server.addBean(login);
*/
       /*
        NCSARequestLog requestLog = new NCSARequestLog(jetty_home + "/logs/jetty-yyyy_mm_dd.log");
        requestLog.setExtended(false);
        requestLogHandler.setRequestLog(requestLog);
		*/
        server.setStopAtShutdown(true);
        server.setSendServerVersion(true);
        
		
		server.start();

		LOG.info("Jetty Server is started");
		LOG.info("Please visit the web application at the url : http://localhost:" + jetty_port + "/");
		
		
		
		new Thread(new Runnable() {
			
			public void run() {

				LOG.info("Please wait until services are initialized.............");

				new GoldDbOperationsService();
				
				LOG.info("Services are initialized. The server is ready for performing services");

				
				ShutdownMonitor monitor = new ShutdownMonitor();
				
				int delay = GeneOntologyManager.getInstance().getFileMonitorDelay();
				FileMonitor goMonitor = new FileMonitor(GeneOntologyManager.getInstance().getDefaultOntologyLocations(), delay*60*1000);
				goMonitor.addFileMonitorListener(GoldDbOperationsService.getFileMonitorListener());
				goMonitor.startMonitoring();
				LOG.info("Ontologies files monitor is started");
				
				
				monitor.start();
				
			}
		}).start();
		
		
		/*LOG.info("Initializing gold database schema.");
		LOG.info("WAIT.............");
		
		SchemaManager schema = new SchemaManager();
		schema.loadSchemaSQL();

		LOG.info("SQL Schema is created successfully");
		*/
		server.join();		
		
		
		/*server = new Server();
		
		System.setProperty("jetty.port", jetty_port + "");
		
		//reading the jetty configurations including the configurtions for jsp
		String[] configFiles = {"etc/jetty.xml"};
		for(String configFile : configFiles) {
		    XmlConfiguration configuration = new XmlConfiguration(new File(configFile).toURI().toURL());
		    configuration.configure(server);
		}
		
		
		//setting up the root context
		WebAppContext files = new WebAppContext("webcontents", "/");
		
		ContextHandlerCollection contexts = new ContextHandlerCollection();
	
		//setting up the gold servlet
		Context root = new Context(contexts, "/gold", Context.SESSIONS);
		root.addServlet(new ServletHolder(new AdminServlet()), "/*");

		
	//	HandlerList handlers = new HandlerList();
		ContextHandlerCollection handlers = new ContextHandlerCollection();
		handlers.setHandlers(new Handler[]{ files, root, new DefaultHandler()});		
		server.setHandler(handlers);
		ShutdownMonitor monitor = new ShutdownMonitor();
		
		int delay = GeneOntologyManager.getInstance().getFileMonitorDelay();
		FileMonitor goMonitor = new FileMonitor(GeneOntologyManager.getInstance().getDefaultOntologyLocations(), delay*60*1000);
		goMonitor.addFileMonitorListener((GoldDbOperationsService)ServicesConfig.getService("gold-db-operations"));
		goMonitor.startMonitoring();
		LOG.info("Ontologies files monitor is started");
		
		
		monitor.start();
		
		server.start();

		LOG.info("Jetty Server is started");
		LOG.info("Please visit the web application at the url : http://localhost:" + jetty_port + "/");
		
		
		LOG.info("Initializing Services which include loading ontologies in memory. This step may take several mintues. ");
		LOG.info("WAIT.............");
		ServicesConfig.getServices();
		
		LOG.info("Services are initialized. The server is ready for performing services");
		
		
		server.join();*/
	}

	/*
	 * From calling this method even from a different jvm, it stops
	 * the jetty server.
	 * 
	 */
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

	/**
	 * This monitor runs in background. It waits for the user requests over the 8097 socket port 
	 * to stop the jetty. The request can be initiated from a different jvm of the localhost.
	 * 
	 * @author Shahid Manzoor
	 *
	 */
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
