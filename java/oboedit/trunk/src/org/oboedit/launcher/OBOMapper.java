package org.oboedit.launcher;

import java.util.Collection;
import java.util.LinkedList;

import org.obo.util.IDMapper;
import org.obo.util.IDMapper.IDFileMetadata;
import org.oboedit.gui.Preferences;

public class OBOMapper {








	public static void main(String[] args) throws Exception {
		System.err.println("version = "+Preferences.getVersion());
		if (args.length == 0)
			printUsage(1);
		for (int i = 0; i < args.length; i++)
			System.err.println("args[" + i + "] = |" + args[i] + "|");

		Collection<String> ontPaths = new LinkedList<String>();
		Collection<String> inputPaths = new LinkedList<String>();
		String out;
		boolean countMode = false;
		IDMapper mapper = new IDMapper();

		for (int i = 0; i < args.length; i++) {
			if (args[i].equals("-ontology")) {
				if (i >= args.length - 1)
					printUsage(1);
				i++;
				ontPaths.add(args[i]);
			} else if (args[i].equals("-o")) {
				if (i >= args.length - 1)
					printUsage(1);
				i++;
				out = args[i];
			} else if (args[i].equals("-c")) {
				countMode=true;
			} else if (args[i].equals("-?")) {
				printUsage(0);
			} else {
				inputPaths.add(args[i]);
			}
		}
		for (String inputPath : inputPaths) {
			mapper.mapIDsInFile(inputPath);
		}
		
	}

	protected static void printUsage(int exitCode) {
		System.err
				.println("obo2obo [-?] [-formatversion <versionid>] <filename 1> ... <filename N> \\\n"
						 + "             [-semanticparse [-addsynonyms]] \\\n"
						 + "             [-parsecomments] [-writecomments] \\\n"
					 + "             [-runscript <scriptname> [arg1 arg2 ... argN] \\;] \\\n"
					 + "             [-o [-reasonerfactory <class>] [-f <filterfile1.xml>] <outputfile1>] ... \\\n"
					 + "             [-o [-f <filterfileN.xml>] <outputfileN>]\n");
		System.err
				.println("  -?                         - Writes this page to stderr and exits.");
		System.err
		.println("  -semanticparse [-addsynonyms] - Does a semantic parse on term name/synonyms; optionally makes synonyms\n"
				+ "                               generates intersection_of definition (which can be reasoned over)\n"
				+ "                               so far only implemented for regulation terms.");
		System.err
		.println("  -reasonerfactory            - When saving implied links, calls a non-default reasoner\n"
				+ "                               Options: ");
		System.err
		.println("  -parsecomments             - Parses comments in obsolete terms looking for\n"
				+ "                               GO-style formatted comments containing parseable\n"
				+ "                               replacement and consider terms.");
		System.err
				.println("  -writecomments             - Writes replaced_by and consider tags into parseable\n"
						+ "                               GO-style formatted comments.");
		System.err
				.println("  -formatversion <versionid> - The version of OBO to write. Allowed values are\n"
						+ "                               OBO_1_0 and OBO_1_2. The default is OBO_1_2.\n"
						+ "                               Optional.");
		System.err
				.println("  -runscript <scriptname> <args> \\; - Runs an OSL script on the ontology. A script tag's\n"
					 + "                               arguments MUST be followed by \\; so that obo2obo knows\n"
					 + "                               where the script arguments stop and obo2obo arguments resume.");
		System.err
				.println("  <filenameN>                - An OBO file to load. Any number of OBO files may\n"
					 + "                               be loaded.");
		System.err
				.println("  -o [-f <objectfilterfile.xml>] [-lf <linkfilterfile.xml>] [-allowdangling] [-p <prefilter property id>] [-strictrootdetection] [-saveimpliedlinks|-saveallimpliedlinks] [-realizeimpliedlinks] <outputfile.obo>\n"
						+ "        An output file to write. The optional -f and -lf flags may be used to specify a\n"
						+ "        filter file or a link filter file to apply to the output file before writing.\n"
						+ "        If the -allowdangling flag is specified, dangling links will not be written.\n"
						+ "        The optional -p flag specifies the id of a property to use for \n"
						+ "        reasoner pre-filtering. The optional -strict-root-detection flag\n"
						+ "        applies filters using strict root detection.");
		System.exit(exitCode);
	}
}
