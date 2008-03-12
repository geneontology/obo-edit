package org.obd.ws.restServlet;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import java.util.Vector;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;
import org.apache.xerces.parsers.DOMParser;
import org.obd.query.impl.MultiShard;
import org.obd.query.impl.OBDSQLShard;
import org.restlet.Context;
import org.restlet.Directory;
import org.restlet.data.LocalReference;
import org.restlet.resource.Resource;


public class ServletConfiguration{
	
	
	protected Map<String,MultiShard> dataSources;
	protected Map<String,Class<? extends Resource>> pathResourceMap;
	protected Map<String,Directory> pathDirectoryMap;
	protected Map<String, Vector<String>> sourceMessages;
	protected Vector<String> pathMappingMessages;
	protected String fremarkerTemplateDirectory;
	
	
	public ServletConfiguration() {
		this.dataSources = new HashMap<String,MultiShard>();
		this.pathResourceMap = new HashMap<String, Class<?extends Resource>>();
		this.pathDirectoryMap = new HashMap<String,Directory>();
		this.sourceMessages = new HashMap<String,Vector<String>>();
		this.pathMappingMessages = new Vector<String>();
	}

	private static class ServerConfigValidatorErrorHandler extends DefaultHandler {
	      public void warning(SAXParseException e) throws SAXException {
	         System.out.println("Warning: "); 
	         printInfo(e);
	      }
	      public void error(SAXParseException e) throws SAXException {
	         System.out.println("Error: "); 
	         printInfo(e);
	      }
	      public void fatalError(SAXParseException e) throws SAXException {
	         System.out.println("Fatal error: "); 
	         printInfo(e);
	      }
	      private void printInfo(SAXParseException e) {
	      	 System.out.println("   Public ID: "+e.getPublicId());
	      	 System.out.println("   System ID: "+e.getSystemId());
	      	 System.out.println("   Line number: "+e.getLineNumber());
	      	 System.out.println("   Column number: "+e.getColumnNumber());
	      	 System.out.println("   Message: "+e.getMessage());
	      }
	}
	    
	public void validateConfig(String filePath){
		System.out.println("Validating " + filePath);
    	try {
			File x = new File(filePath);
			SAXParserFactory f = SAXParserFactory.newInstance();
			f.setValidating(true); // Default is false         
			SAXParser p = f.newSAXParser();
			DefaultHandler h = new ServerConfigValidatorErrorHandler();
			p.parse(x,h);
    	} catch (ParserConfigurationException e) {
    		System.out.println(e.toString()); 
    	} catch (SAXException e) {
    		System.out.println(e.toString()); 
    	} catch (IOException e) {
    		System.out.println(e.toString()); 
    	}
    }
	
	@SuppressWarnings("unchecked")
	public void configureFromFile(String configurationFile, Context context, String contextPath) throws Exception{
        DOMParser parser = new DOMParser();
        
        
        try {
        	System.out.println("Parsing Server configuration");
        	parser.parse(configurationFile); 
        	Document d = parser.getDocument();
        	NodeList list = d.getElementsByTagName("obdRESTServletConfig");
        	if (list.getLength() != 1){
        		throw new Exception("Unexpected number of obdRESTServletConfig params. Expecting 1, found " + list.getLength());
        	}
        	Node orscNode = list.item(0);
        	
        	for (Node ftlDirNode : this.getChildNodes(orscNode, "ftlTemplateDirectory")){
        		this.fremarkerTemplateDirectory = this.getNodeValue(ftlDirNode);
        	}
        	
        	for (Node dataSourceNode : this.getChildNodes(orscNode, "dataSource")){
        		System.out.println("Configuring new DataSource.");
        		String resourcePath = this.getChildNode(dataSourceNode,"resourcePath").getChildNodes().item(0).getNodeValue();
        		System.out.println("\tResource Path: " + resourcePath);
        		
        		MultiShard m = new MultiShard();
        		this.dataSources.put(resourcePath, m);
        		
        		// For each shard / adaptor you want, you'll need to write some code to configure the shard.
        		for (Node obdSQLConfigNode : this.getChildNodes(dataSourceNode, "OBDSQLAdaptor")){
        			String connectionPath = this.getConnectionPath(obdSQLConfigNode);
        			try {
        				OBDSQLShard s = this.generateOBDSQLShard(obdSQLConfigNode);
        				this.dataSources.get(resourcePath).addShard(s);
        				this.addDataSourceMessage(resourcePath, "Available OBDSQLShard: " + connectionPath);
        			} catch (Exception e){
        				this.addDataSourceMessage(resourcePath, "Failed connection to " + connectionPath + " : " + e.getMessage());
        			}
        			
        		}
        		
        	}
        	
        	for (Node resourcePathMapNode : this.getChildNodes(orscNode, "resourcePathMap")){
        		
            	String className = this.getParam(resourcePathMapNode, "resourceClass");
            	if (className != null){
	        		try {
	        			System.out.println("Mapping paths to class " + className);
	        			Class c = Class.forName(className);
	        		
	        			for (Node pathNode : this.getChildNodes(resourcePathMapNode, "path")){
	            			String path = this.getNodeValue(pathNode);
	            			if (path == null){
	            				path = "";
	            			}
	            			System.out.println("\tpath: " + path);
            				this.pathResourceMap.put(path, c);
	            		}
	        			
	        		} catch (ClassNotFoundException e) {
	        			System.err.println("Unable to find class " + className + ". Is it in the class path or the Servlet WEB-INF/lib directory?");
	        			System.err.println(e.getMessage());
	        			e.printStackTrace();
	        			for (Node pathNode : this.getChildNodes(resourcePathMapNode, "path")){
	            			String path = this.getNodeValue(pathNode);
	            			this.pathMappingMessages.add(("Couldn't attach resource " + className + " to path " + path + ". Class not found."));
	            		}
	        		}
            	}
            	
            	String dirName = this.getParam(resourcePathMapNode, "directoryPath");
            	if (dirName != null){
            		String dirPath = contextPath + dirName;
            		File f = new File(dirPath);
            		if (f.exists()){
            			System.out.println("Mapping paths to directory " + dirPath);
            			for (Node pathNode : this.getChildNodes(resourcePathMapNode, "path")){
            				String path = this.getNodeValue(pathNode);
            				LocalReference l = LocalReference.createFileReference(f);
            				Directory directory = new Directory(context, l);
            				this.pathDirectoryMap.put(path, directory);
            				System.out.println("\tpath: " + path);
            			}
            		} else {
            			for (Node pathNode : this.getChildNodes(resourcePathMapNode, "path")){
            				String path = this.getNodeValue(pathNode);
            				this.pathMappingMessages.add("Can't map directory " + dirPath + " to path " + path + ". Directory does not exist.");
            				System.err.println("Can't map directory " + dirPath + " to path " + path + ". Directory does not exist.");
            			
            			}
            		}
        		
            	}
        	}
        } catch (SAXException e) {
        	System.out.println("Document is not well-formed.");
        	e.printStackTrace();
        } catch (IOException e) { 
        	System.out.println("Due to an IOException, the parser could not finish" );
        	e.printStackTrace();
        }
	}
	
	
	
	public Map<String, MultiShard> getDataSources() {
		return dataSources;
	}

	public void setDataSources(Map<String, MultiShard> dataSources) {
		this.dataSources = dataSources;
	}

	public Map<String, Class<? extends Resource>> getPathResourceMap() {
		return pathResourceMap;
	}

	public void setPathResourceMap(
			Map<String, Class<? extends Resource>> pathResourceMap) {
		this.pathResourceMap = pathResourceMap;
	}

	private Node getChildNode(Node n, String name){
		NodeList childNodes = n.getChildNodes();
		for (int i=0;i<childNodes.getLength();i++){
			if (childNodes.item(i).getNodeName().equals(name)){
				return childNodes.item(i);
			}
		}
		return null;
	}
	
	private Vector<Node> getChildNodes(Node n, String name){
		Vector<Node> returnChildNodes = new Vector<Node>();
		NodeList childNodes = n.getChildNodes();
		for (int i=0;i<childNodes.getLength();i++){
			if (childNodes.item(i).getNodeName().equals(name)){
				returnChildNodes.add(childNodes.item(i));
			}
		}
		
		return returnChildNodes;
	}
	
	private OBDSQLShard generateOBDSQLShard(Node n) throws Exception{
		
		
		String jdbcPath = this.getConnectionPath(n);
		
			try {
				OBDSQLShard obd = new OBDSQLShard();
				obd.connect(jdbcPath, this.getParam(n, "username"), this.getParam(n, "password"));
				System.out.println("\tCreating new OBDSQLShard: " + jdbcPath);
				return obd;
			} catch (SQLException e) {
				System.err.println("ERROR: Could not create OBDSQL shard for " + jdbcPath + ". SQLException: " + e.getMessage());
				e.printStackTrace();
				throw e;
			} catch (ClassNotFoundException e) {
				System.err.println("ERROR: Could not create OBDSQL shard for " + jdbcPath + ". ClassNotFoundException: " + e.getMessage());
				e.printStackTrace();
				throw e;
			}
	}
	
	private String getConnectionPath(Node n){
		return ("jdbc:postgresql://" + this.getParam(n, "hostname") + ":" + this.getParam(n, "port") + "/" + this.getParam(n, "databaseName"));
	}
	
	private String getNodeValue(Node n){
		NodeList nl = n.getChildNodes();
		if (nl.getLength() == 1){
			return nl.item(0).getNodeValue();
		} else {
			return null;
		}
		
	}
	
	private String getParam(Node n, String paramName){
		Node childNode = this.getChildNode(n, paramName);
		if (childNode==null){
			return null;
		} else {
			return this.getNodeValue(childNode);
		}
	}
	
	private void addDataSourceMessage(String dataSource, String message){
		if (!this.sourceMessages.containsKey(dataSource)){
			this.sourceMessages.put(dataSource, (new Vector<String>()));
		}
		this.sourceMessages.get(dataSource).add(message);
	}

	public Map<String, Vector<String>> getSourceMessages() {
		return sourceMessages;
	}

	public Map<String, Directory> getPathDirectoryMap() {
		return pathDirectoryMap;
	}

	public void setPathDirectoryMap(Map<String, Directory> pathDirectoryMap) {
		this.pathDirectoryMap = pathDirectoryMap;
	}

	public Vector<String> getPathMappingMessages() {
		return pathMappingMessages;
	}

	public void setPathMappingMessages(Vector<String> pathMappingMessages) {
		this.pathMappingMessages = pathMappingMessages;
	}

	public void setSourceMessages(Map<String, Vector<String>> sourceMessages) {
		this.sourceMessages = sourceMessages;
	}

	public String getFremarkerTemplateDirectory() {
		return fremarkerTemplateDirectory;
	}

	public void setFremarkerTemplateDirectory(String fremarkerTemplateDirectory) {
		this.fremarkerTemplateDirectory = fremarkerTemplateDirectory;
	}

}