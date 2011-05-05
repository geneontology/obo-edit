package org.geneontology.cli;

import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.jetty.JettyStarter;

/**
 * This class is the main CLI interface of all GOLD applications CLI.
 * Through this interface other CLI interfaces(,e.g gold and jetty) are invoked.
 * @author Shahid Manzoor
 *
 */
public class CLI {

	private CLI(){
		
	}
	
	private static void exit(String message){
		System.err.println(message);
		usage();
		System.exit(0);
		
	}
	
	private static void usage(){
		System.out.println("Descripton:");
			System.out.println("\tCommand Line Utility of GOLD applications");
		
		System.out.println("Syntax:");
			System.out.println("\tgold");
			System.out.println("\tjetty");
	}
	
	
	public static void main(String args[]) throws Exception{
	
		//initialize the system
		GeneOntologyManager.getInstance();
		
		if(args.length==0){
			exit("Invalid arguments");
		}
		
		String command = args[0];
		
		String[] ar = new String[args.length-1];

		for(int i=1;i<args.length;i++){
			ar[i-1] = args[i];
		}
		
		if("gold".equals(command)){
			GoldCommandLine.main(ar);
		}else if ("jetty".equals(command)){
			JettyStarter.main(ar);
		}else if("gaf".equals(command)){
			GafCommandLine.main(ar);
		}
	}
	
}
