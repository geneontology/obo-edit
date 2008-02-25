package org.oboedit.launcher;

import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.Reader;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.zip.GZIPInputStream;
import java.util.zip.ZipInputStream;

import org.apache.commons.lang.StringUtils;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.TermCategory;
import org.obo.identifier.IDWarning;
import org.obo.util.AdapterUtil;
import org.obo.util.IDMapper;
import org.obo.util.IDMapper.IDFileMetadata;
import org.obo.util.IDMapper.SimpleAnnotation;
import org.oboedit.gui.Preferences;

public class OBOMapper {

	public static void mapIDsInFile(IDMapper mapper,
			String inputPath, String out, boolean countMode) throws IOException {

		IDFileMetadata fileMetadata = mapper.getFileMetadata();
		int ENTITY = fileMetadata.getEntityColumn()-1;
		int ID = fileMetadata.getOboIDColumn()-1;
		Reader reader;
		//FileReader fr = new FileReader(inputPath);
		if (inputPath.endsWith(".gz")) {
			GZIPInputStream input =
				new GZIPInputStream(new FileInputStream(inputPath));
			reader = new LineNumberReader(new InputStreamReader(input));
		}
		else {
			reader = new FileReader(inputPath);
		}
		BufferedWriter bw = new BufferedWriter(new FileWriter(out));
		LineNumberReader lnr =  new LineNumberReader(reader);
			Map<String, Collection<String>> e2ids = new HashMap<String,Collection<String>>();
		for (String line=lnr.readLine(); line != null; line = lnr.readLine()) {
			SimpleAnnotation annot = mapper.parseAndFilterLine(line);
			if (annot == null)
				continue;
			String[] colVals = annot.getColVals();
			String oboID = annot.getOboID();
			String entityID = annot.getEntityID();
			if (countMode) {
				if (!e2ids.containsKey(entityID))
					e2ids.put(entityID, new HashSet<String>());
				e2ids.get(entityID).add(oboID);
			}
			else {
				Collection<OBOObject> objs = mapper.mapIdentifierViaCategories(oboID,false);
				for (OBOObject obj : objs) {
					colVals[ID] = obj.getID();
					// TODO - name
					// TODO - allow passing in of handler
					printLine(bw,colVals,fileMetadata.getColumnDelimiter());
				}
				if (objs.size() == 0) {
					System.err.println("cannot map: "+oboID);
				}
			}
		}
		if (countMode) {
			mapper.calcEntityCountByOboID(e2ids);
			Map<String, Integer> entityCountByOboIDMap = mapper.getEntityCountByOboIDMap();
			for (String id : entityCountByOboIDMap.keySet()) {
				OBOObject obj = (OBOObject)mapper.getSession().getObject(id);
				// TODO - handler class
				bw.write(entityCountByOboIDMap.get(id)+"\t"+id+"\t"+obj.getName()+"\n");
			}

		}
		bw.close();
	}
	
	private static void printLine(BufferedWriter bw, String[] colVals, String sep) throws IOException {
		bw.write(StringUtils.join(colVals,sep));
		bw.write("\n");
	}



	public static void main(String[] args) throws Exception {
		System.err.println("version = "+Preferences.getVersion());
		if (args.length == 0)
			printUsage(1);
		for (int i = 0; i < args.length; i++)
			System.err.println("args[" + i + "] = |" + args[i] + "|");

		Collection<String> ontPaths = new LinkedList<String>();
		Collection<String> inputPaths = new LinkedList<String>();
		Collection<String> categoryNames = new LinkedList<String>();
		String out = null;
		boolean countMode = false;
		boolean allSlims = false;
		boolean followConsiderTags = false;
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
			} else if (args[i].equals("-subset")) {
				if (i >= args.length - 1)
					printUsage(1);
				i++;
				categoryNames.add(args[i]);
			} else if (args[i].equals("-followconsider")) {
				followConsiderTags = true;
			} else if (args[i].equals("-c")) {
				countMode=true;
			} else if (args[i].equals("-?") || args[i].equals("-help")) {
				printUsage(0);
			} else {
				inputPaths.add(args[i]);
			}
		}
		OBOSession session = AdapterUtil.parseFiles(ontPaths);
		mapper.setSession(session);
		for (String cat : categoryNames) {
			System.err.println("filtering on: "+cat);
			mapper.addCategory(cat);
		}
		if (followConsiderTags)
			mapper.setAutoReplaceConsiderTags(followConsiderTags);
		if (out == null) {
			System.err.println("You must specify an output file with -o FILE");
			printUsage(1);
		}
		for (String inputPath : inputPaths) {
			System.err.println("mapping: "+inputPath);
			if (allSlims) {
				mapIDsInFile(mapper,inputPath,out,countMode);
			}
			else {
				for (TermCategory cat : session.getCategories()) {
					mapper.reset();
					mapper.setSession(session);
					mapper.setCategory(cat);
					mapIDsInFile(mapper,inputPath,out,countMode);
				}
			}
		}
		for (IDWarning warning : mapper.getWarnings()) {
			System.err.println(warning);
		}
		
	}

	protected static void printUsage(int exitCode) {
		System.err
				.println("obo-mapper [-?] [-formatversion <versionid>] <filename 1> ... <filename N> \\\n"
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
