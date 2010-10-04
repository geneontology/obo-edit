package owltools.cli;

import java.util.ArrayList;
import java.util.Collection;

public class CommandLineInterface {
	
	public static void main(String[] args) {

		Collection<String> paths = new ArrayList<String>();
		int i=0;
                // REDUNDANT: see new method
		String reasonerClassName = "uk.ac.manchester.cs.factplusplus.owlapiv3.Reasoner";
		String reasonerName = null;
		boolean createNamedRestrictions = false;
		boolean createDefaultInstances = false;

		while (i < args.length) {
			String opt = args[i];
			System.out.println("processing arg: "+opt);
			i++;
			if (opt.equals("--pellet")) {
				reasonerClassName = "com.clarkparsia.pellet.owlapiv3.Reasoner";
				reasonerName = "pellet";
			}
			else if (opt.equals("--hermit")) {
				reasonerClassName = "org.semanticweb.HermiT.Reasoner";
				reasonerName = "hermit";
			}
			else if (opt.equals("--no-reasoner")) {
				reasonerClassName = "";
				reasonerName = "";
			}
			else if (opt.equals("-r") || opt.equals("--namerestr")) {
				createNamedRestrictions = true;
			}
			else if (opt.equals("-i") || opt.equals("--inst")) {
				createDefaultInstances = true;
			}
			else if (opt.equals("-m") || opt.equals("--mireot")) {
				args = runMireot(args);
			}
			else {
				paths.add(opt);
			}
		}
	}

	private static String[] runMireot(String[] args) {
		int i = 0;
	
		while (i < args.length) {
			
		}
		return null;
	}

}
