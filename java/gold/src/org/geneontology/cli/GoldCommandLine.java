package org.geneontology.cli;

import java.util.HashSet;
import java.util.Hashtable;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.Priority;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gold.io.DbOperations;

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
		options.put("-obo", new String[]{"=filepatht\t\tPath of the OBO file to updated", "geneontology.gold.obofile"});
		options.put("-prefix", new String[]{"=text\t\tPrefix of the table names to be used in delta updte", "geneontology.gold.deltatableprefix"});
		options.put("-force", new String[]{"\t\t\tDrop exsiting schema and create new one", ""});
		options.put("-debug", new String[]{"\t\t\tEnabling log4j output", ""});
		
		
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
		System.err.println(message);
		usage();
		System.exit(0);
		
	}
	
	public static void main(String args[]) throws Exception{
		boolean force = false;
		String tableprefix = "";
		//atleast two arguments are required
		if(args.length>1){
			//last argument is the operation name that to be peforme by this program. 
			String operation = args[args.length-1];
			if(!operations.contains(operation)){
				exit("The '" + operation + "' operation is not valid option. Please see the usage.");
			}
			GeneOntologyManager manager= GeneOntologyManager.getInstance();
			//validate the options
			//update properties in the GeneOntologyManager at the system level with the
			//options provided through command line arguments
			for(int i=1;i<args.length-1;i++){
				String option = args[i].trim();
				String[] tokens = option.split("=");
				String v[] = options.get(tokens[0]);
				
				if(v == null){
					exit("The '" + option + "' is not a valid option");
				}

				if("-force".equals(tokens[0])){
					force =true;
				}else if("-debug".equals(tokens[0])){
					Logger.getRootLogger().setLevel(Level.DEBUG);
				}else{
					if ("-prefix".equals(tokens[0]) )
						tableprefix = tokens[1];
					manager.setProperty(v[1], tokens[1]);
				}
			}
			
			
			//perform the operations
			DbOperations db = new DbOperations();
			
			if("bulkload".equals(operation)){
				db.bulkLoad(force);
			}else if("update".equals(operation)){
				db.updateGold();
			}else if("buildschema".equals(operation)){
				db.buildSchema(force, tableprefix);
			}else if("buildtsv".equals(operation)){
				db.dumpFiles(tableprefix, manager.getDefaultOboFile());
			}else if ("loadtsv".equals(operation)){
				db.loadTsvFiles(manager.getTsvFilesDir());
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
		System.out.println("\tgold bulkload");
		System.out.println("\tgold update");
		System.out.println("\tgold -u=user =p=password -h=host.com -force bulkload");
		
		
		System.out.println("OPTIONS:");
		System.out.println("----------------------------------------------");
		System.out.println("If no options are supplied then the default values are taken");
		System.out.println("from the '{project.home}/conf/gold.properties' file");
		System.out.println("----------------------------------------------");
		
		for(String option: options.keySet()){
			System.out.println("\t" + option + options.get(option)[0]);
		}

		System.out.println("OPERATION:");
		for(String operation: operations){
			System.out.println("\t"+operation);
		}
		
	}
	
}
