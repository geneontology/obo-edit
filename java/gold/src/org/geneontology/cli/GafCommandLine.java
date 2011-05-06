package org.geneontology.cli;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.Writer;
import java.net.URLEncoder;
import org.apache.commons.httpclient.DefaultHttpMethodRetryHandler;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.params.HttpMethodParams;
import org.eclipse.jetty.util.log.Log;
import org.geneontology.conf.GeneOntologyManager;

public class GafCommandLine {

	private static void usage(){
		System.out.println();
		System.out.println();

		System.out.println("*********Gene Association File (GAF) tool**********");
		System.out.println("The tool submit a GAF file to the admin servlet, the  \n" +
				"servlet parse the GAF file and runs QC checks on the file. The output is returned as a \n" +
				"json which can be saved in a file or printed on the standard output.");
		System.out.println("gaf-tools [-serverurl url -o outfilepath  gafile-path/url] ");
		System.out.println();
		System.out.println("\tAll arguments are optional. The default server url is http://localhost:8080/gold/. The" +
				"\n\tThe default output is printed on stdout. " +
				"\n\tThe default gaf file read through the geneontology.gold.ontologylocation property" +
				"\n\tin the conf/gold.properties file. The gafile-path/url can refer to a in the local system and http or ftp url.");
		System.out.println();
		System.out.println("\tExamples:");
		System.out.println("\t\tbin/gaf-runner http://www.geneontology.org/gene-associations/gene_association.GeneDB_Spombe.gz");
		System.out.println("\t\tbin/gaf-runner -server http://localhost:8080/gold/ -o out.json  http://www.geneontology.org/gene-associations/gene_association.GeneDB_Spombe.gz");
		System.out.println("\t\tbin/gaf-runner -o out.json");
		
		System.exit(0);
	}
	
	public static void main(String args[]){
		
		String serverURL = "http://localhost:8080/gold/";
		String gafFileLocation = GeneOntologyManager.getInstance().getDefaultGafFileLocations().get(0).toString();
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
				gafFileLocation = arg;
			}
				
		}
		
		
		checkAnnotations(serverURL, gafFileLocation, outFile);
		
	}
	
	
	private static void checkAnnotations(String adminServletURL, String annotationFilePath, String outFile){
	
		
		if(!(annotationFilePath.startsWith("http://") || annotationFilePath.startsWith("file:///") 
				|| annotationFilePath.startsWith("ftp://"))){
			File f = new File(annotationFilePath);
			annotationFilePath = f.toURI().toString();
		}
		
		GetMethod method = null;
	    try {
		    String encodedPath = URLEncoder.encode(annotationFilePath, "UTF-8");
	
			//build query String
			String queryString = "?servicename=gaf-db-operations&runrules=&format=json";
		    
		    if(annotationFilePath.startsWith("file:/")){
		    	queryString += "&filelocation="+ encodedPath;   	
		    }else
		    	queryString += "&remote-gaf="+ encodedPath;   	

//		    	params.setParameter("remote-gaf", encodedPath);

			HttpClient httpclient = new HttpClient();
			
			  // Create a method instance.
		    method = new GetMethod(adminServletURL+queryString);
		    
		    // Provide custom retry handler is necessary
		    method.getParams().setParameter(HttpMethodParams.RETRY_HANDLER, 
		    		new DefaultHttpMethodRetryHandler(3, false));		
	    	
	    	// Execute the method.
	        int statusCode = httpclient.executeMethod(method);

	        if (statusCode != HttpStatus.SC_OK) {
	          System.err.println("Method failed: " + method.getStatusLine());
	        }

	        InputStream is = method.getResponseBodyAsStream();
	        
	         BufferedReader reader = new BufferedReader(new InputStreamReader(is));
	         
	         String line = null;
	     
	         PrintStream ps = System.out;
	         
	         if(outFile != null){
	        	 ps = new PrintStream(new File(outFile));
	         }
	         
	         while((line = reader.readLine()) != null ){
	        	 ps.println(line);
	         }
	        

	      } catch (Exception e) {
	        System.err.println("Fatal protocol violation: " + e.getMessage());
	        e.printStackTrace();
	      } finally {
	        // Release the connection.
	        method.releaseConnection();
	      }  	    
	    
	}
	
	
}
