package org.geneontology.cli;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;

import org.apache.commons.httpclient.DefaultHttpMethodRetryHandler;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.params.HttpMethodParams;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gold.io.DbOperationsListenerToReportTime;

/**
 * The class provides command line interface to users to run the db operations from
 * the shell prompt.
 * @author Shahid Manzoor
 *
 */
public class GoldCommandLine {

	private static HashSet<String> operations = buildOperations();
	private static Hashtable<String, String[]> options = buildOptions();

	
	private static Hashtable<String, String[]> buildOptions(){
		Hashtable<String, String[]> options = new Hashtable<String, String[]>();
	
		options.put("-d", new String[]{"=databasename", "geneontology.gold.db"});
		options.put("-u", new String[]{"=username", "geneontology.gold.username"});
		options.put("-p",new String[]{"=password", "geneontology.gold.password"});
		options.put("-h", new String[]{"=host", "geneontology.gold.host"});
		options.put("-tsv=", new String[]{"directorypath\t\tPath of the TSV files", "geneontology.gold.tsvfiles"});
		options.put("-ontologylocation", new String[]{"=filepath\t\tPath of the OBO file to updated", ""});
		options.put("-prefix", new String[]{"=text\t\tPrefix of the table names to be used in delta updte", "geneontology.gold.deltatableprefix"});
		options.put("-force", new String[]{"\t\t\tDrop exsiting schema and create new one", ""});
		options.put("-debug", new String[]{"\t\t\tEnabling log4j output", ""});
		options.put("-gaf", new String[]{"\t\t\tEnabling log4j output", ""});
		
		
		return options;
	}
	
	
	private static HashSet<String> buildOperations(){
		HashSet<String> operations = new HashSet<String>();
		operations.add("bulkload");
		operations.add("update");
		operations.add("buildschema");
		operations.add("buildtsv");
		operations.add("loadtsv");

		
		return operations;
	}
	
	private static void exit(String message){
		usage();
		System.err.println(message);
		System.exit(0);
		
	}
	
	public static void main(String args[]) throws Exception{
		boolean force = false;
		String tableprefix = "";

		Logger.getRootLogger().setLevel(Level.INFO);
		
		
		System.out.println("****************GOLD DB*******************");
		
		//atleast two arguments are required
		if(args.length>0){
			//last argument is the operation name that to be peforme by this program. 
			String operation = args[args.length-1];
			if(!operations.contains(operation)){
				exit("The '" + operation + "' operation is not valid option. Please see the usage.");
			}
			GeneOntologyManager manager= GeneOntologyManager.getInstance();
			//validate the options
			//update properties in the GeneOntologyManager at the system level with the
			//options provided through command line arguments
			String ontologyLocation = null;

			String dbType = "gold";
			
			for(int i=0;i<args.length-1;i++){
				String option = args[i].trim();
				String[] tokens = option.split("=");
				String v[] = options.get(tokens[0]);
				
				if(v == null){
					exit("The '" + option + "' is not a valid option");
				}

				
				if("-ontologylocation".equals(tokens[0])){
					ontologyLocation = tokens[1];
				}else if("-force".equals(tokens[0])){
					force =true;
				}else if("-debug".equals(tokens[0])){
					Logger.getRootLogger().setLevel(Level.DEBUG);
				}else if ("-gaf".equals(tokens[0])){
					dbType = "gaf";
				}else{
					if ("-prefix".equals(tokens[0]) )
						tableprefix = tokens[1];
					manager.setProperty(v[1], tokens[1]);
				}
			}
			
			//perform operations
			if("loadtsv".equals(operation) || "buildschema".equals(operation)){
				DbOperationsListenerToReportTime db = new DbOperationsListenerToReportTime(dbType , operation, null, force, tableprefix, manager.getTsvFilesDir());
				db.run();
			}else if("bulkload".equals(operation) || "update".equals(operation) 
					|| "buildtsv".equals(operation)){
				

				String ontologyLocations[] = null;
				
				if(ontologyLocation != null){
					ontologyLocations = new String[]{ontologyLocation};
				}else{
					
					if("gaf".equals(dbType)){
						List list = manager.getDefaultGafFileLocations();
						ontologyLocations = new String[list.size()];
						list.toArray(ontologyLocations);
						
					}else{
						List list = manager.getDefaultOntologyLocations();
						ontologyLocations = new String[list.size()];
						list.toArray(ontologyLocations);
					}
					
				}
				
				DbOperationsListenerToReportTime db = new DbOperationsListenerToReportTime(dbType, operation, 
						ontologyLocations, 
						force, tableprefix, manager.getTsvFilesDir());
				
				db.run();
				
			}
			
		
		}else
			exit("Not valid arguments are passed. Please the usage");
	}
	
	
	
	private static void usage(){
		System.out.println("Description:");
		System.out.println("\tGold Database Operations Utility");

		System.out.println("Syntax:");
		System.out.println("\tgold [-OPTIONS] OPERATION");
	
		System.out.println("Examples:");
		System.out.println("\tgold -ontologylocation=path bulkload");
		System.out.println("\tgold  -ontologylocation=path update");
		System.out.println("\tgold -ontologylocation=path -u=user =p=password -h=host.com -force bulkload");
		
		
		System.out.println("OPTIONS:");
		System.out.println("----------------------------------------------");
		System.out.println("If no options are supplied then the default values are taken");
		System.out.println("from the '{project.home}/conf/gold.properties' file");
		System.out.println("----------------------------------------------");
		
		for(String option: options.keySet()){
			System.out.println("\t" + option + options.get(option)[0]);
		}

		System.out.println("OPERATIONS:");
		for(String operation: operations){
			System.out.println("\t"+operation);
		}
		
	}
	
}
