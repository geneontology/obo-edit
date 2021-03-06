package org.geneontology.cli;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.net.URI;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.httpclient.DefaultHttpMethodRetryHandler;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.methods.multipart.FilePart;
import org.apache.commons.httpclient.methods.multipart.MultipartRequestEntity;
import org.apache.commons.httpclient.methods.multipart.Part;
import org.apache.commons.httpclient.methods.multipart.StringPart;
import org.apache.commons.httpclient.params.HttpMethodParams;
import org.apache.log4j.Logger;
import org.geneontology.conf.GoConfigManager;

/**
 * This command line utility executes commands to process gaf file.
 * @author Shahid Manzoor
 *
 */

public class GafCommandLine {

	private static Logger LOG = Logger.getLogger(GafCommandLine.class);
	
	private static void usage(){
		System.out.println();
		System.out.println();

		System.out.println("*********Gene Association File (GAF) tool**********");
		System.out.println("The tool submit a GAF file to the admin servlet, the  \n" +
				"servlet parse the GAF file and runs QC checks on the file. The output is returned as a \n" +
				"json which can be saved in a file or printed on the standard output."
				+ "\nThe tool can be run remotely, and a local gaf file is uploaded to the server for parsing and annotation checks." );
		System.out.println("gaf-tools [-serverurl url -o outfilepath  gafile-path/url] ");
		System.out.println();
		System.out.println("\tAll arguments are optional. The default server url is http://localhost:"+GoConfigManager.getInstance().getJettyPort()+"/gold/. The" +
				"\n\tThe default output is printed on stdout. " +
				"\n\tThe default gaf file read through the geneontology.gold.ontologylocation property" +
				"\n\tin the conf/gold.properties file. The gafile-path/url can refer to a in the local system and http or ftp url.");
		System.out.println();
		System.out.println("\tExamples:");
		System.out.println("\t\tbin/gaf-runner http://www.geneontology.org/gene-associations/gene_association.GeneDB_Spombe.gz");
		System.out.println("\t\tbin/gaf-runner -serverurl http://localhost:8080/gold/ -o out.json  http://www.geneontology.org/gene-associations/gene_association.GeneDB_Spombe.gz");
		System.out.println("\t\tbin/gaf-runner -o out.json");
		
		System.exit(0);
	}
	
	public static void main(String args[]){
		
		String serverURL = "http://localhost:"+GoConfigManager.getInstance().getJettyPort()+"/gold/";
		String annotationFilePath = GoConfigManager.getInstance().getDefaultGafFileLocations().get(0).toString();
		String outFile = null;
		for(int i=0;i<args.length;i++){
			String arg = args[i];
		
			if(arg.equals("-h") || arg.equals("--help")){
				usage();
			}else if(arg.equals("-serverurl")){
				i++;
				serverURL = args[i];
			}else if(arg.equals("-o")){
				i++;
				outFile = args[i];
			}else{
				annotationFilePath = arg;
			}
				
		}
		
		if(!(annotationFilePath.startsWith("http://") || annotationFilePath.startsWith("file:/") 
				|| annotationFilePath.startsWith("ftp://"))){
			File f = new File(annotationFilePath);
			annotationFilePath = f.toURI().toString();
		}
		
		
		checkAnnotations(serverURL, annotationFilePath, outFile);
		
	}
	
	
	/**
	 * This method sends request to run annotation checks at the GOLD server via using Jakarata Commans Http Client library. 
	 * The annotation checks run asynchronously at server. When a request is sent first time the server sends session id.
	 * This client sends requests in a infinite loop to the sever along with session id to get annotation violations. The loop
	 * breaks only when it gets NO_DATA in result (response text).
	 * 
	 * @param adminServletURL
	 * @param annotationFilePath
	 * @param outFile
	 */
	private static void checkAnnotations(String adminServletURL, String annotationFilePath, String outFile){
	
		
		
		HttpMethod method = null;
        PrintStream ps = System.out;
		
	    try {

	    	if(outFile != null){
	          	 ps = new PrintStream(new File(outFile));
	           }

	    	boolean postSent = false;
	        Pattern pattern = Pattern.compile("name\\s*=\\s*\"filelocation\"\\s*value\\s*=\\s*\"([^\"]*)\"");
	        String sessionId = null;
	    	while(true){
	    		
		    	if(!annotationFilePath.trim().startsWith("file:")){
				    String encodedPath = URLEncoder.encode(annotationFilePath, "UTF-8");
			
					//build query String
					String queryString = "?servicename=gaf-db-operations&command=runrules&view=gafjson";
				    
				    if(annotationFilePath.startsWith("http://") || annotationFilePath.startsWith("ftp://")){
				    	queryString += "&remote-gaf="+ encodedPath;   	
				    }else
				    	queryString += "&filelocation="+ encodedPath;   	
		
				    if(sessionId != null){
				    	queryString += "&id=" + sessionId;
				    }
				    
				    LOG.info("Sending GET Request: "+ adminServletURL+queryString);
				    method = new GetMethod(adminServletURL+queryString);
				    
				    // Provide custom retry handler is necessary
				    method.getParams().setParameter(HttpMethodParams.RETRY_HANDLER, 
				    		new DefaultHttpMethodRetryHandler(3, false));		
		    	}else{
		    		//This part of the program build post method to upload file.
		    		LOG.info("Sending request with POST method");
	
		    		PostMethod post = new PostMethod(adminServletURL);
		    		method = post;
		    		List<Part> parts = new ArrayList<Part>();
		    		File f = new File( new URI(annotationFilePath));
		    		FilePart filePart = new FilePart(f.getName(), f);
		    		
		    		parts.add(filePart);
		    		parts.add(new StringPart("servicename", "gaf-db-operations", "UTF-8"));
		    		parts.add(new StringPart("view", "gafjson", "UTF-8"));
		    		parts.add(new StringPart("command", "runrules", "UTF-8"));
	
		    		post.setRequestEntity(new MultipartRequestEntity(parts.toArray(new Part[parts.size()]), post.getParams()));	    		
		    		
		    		
		    	}
			    
				HttpClient httpclient = new HttpClient();
				
				  // Create a method instance.
		    	
		    	// Execute the method.
		        int statusCode = httpclient.executeMethod(method);
	
		        if (statusCode != HttpStatus.SC_OK) {
		          System.err.println("Method failed: " + method.getStatusLine());
		        }
	
		        //Reading response from the method
		        InputStream is = method.getResponseBodyAsStream();
		        
		        BufferedReader reader = new BufferedReader(new InputStreamReader(is));
		         
		         String line = null;
		     
		         boolean breakLoop = false;
		         while((line = reader.readLine()) != null ){
		        	// buf.append(line + "\n");
		        	 if(!postSent){
			        	 Matcher matcher = pattern.matcher(line);
			        	 if(matcher.find()){
			        		 annotationFilePath = matcher.group();
			        		 String[] s= annotationFilePath.split("=");
			        		 annotationFilePath = s[2].trim();
			        		 postSent = true;
			        		 annotationFilePath = annotationFilePath.replaceAll("\"", "");
			        		 //checkAnnotations(adminServletURL, annotationFilePath, outFile);
			        		// return;
			        		break;
			        	 }
		        	 }else{
			        	 // ps.println(line);
	
			        	 if(sessionId == null && line.trim().contains("session:")){
			        		 sessionId = line.split(":")[1].trim();
			        	 }		        	 
			        	 
			        	 else if(line.trim().contains("NO_DATA")){
			        		 breakLoop = true;
			        		 break;
			        	 }else{
			        		 ps.println(line);
			        	 }
	
			        	 if(line.contains("\"error\"")){
			        		 breakLoop = true;
			        		 break;
			        	 }
		        	 }
		        	 
		        	 
		         }
		         
		        if(breakLoop)
		        	break;
		         
		        Thread.sleep(30000); 
	      
        	 
	    	} 
        	 
 	      } catch (Exception e) {
	        System.err.println("Fatal protocol violation: " + e.getMessage());
	        e.printStackTrace();
	      } finally {
	        // Release the connection.
	        method.releaseConnection();
	        if(outFile != null){
	        	ps.close();
	        }
	      }  	    
	    
	}
	
	
}
