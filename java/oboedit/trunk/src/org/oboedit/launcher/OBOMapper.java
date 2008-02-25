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
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.ReasonerFactory;
import org.obo.reasoner.impl.DefaultReasonerFactory;
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
		boolean allowDangling = false;
		boolean useReasoner = false;
		ReasonerFactory reasonerFactory = new DefaultReasonerFactory();
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
			} else if (args[i].equals("-usereasoner")) {
				useReasoner=true;
			} else if (args[i].equals("-reasonerfactory")) {
				if (i >= args.length - 1)
					printUsage(1);
				i++;
				reasonerFactory = (ReasonerFactory)Class.forName(args[i]).newInstance();
				useReasoner=true;
			} else if (args[i].equals("-allowdangling")) {
				allowDangling = true;
			} else if (args[i].equals("-?") || args[i].equals("-help")) {
				printUsage(0);
			} else {
				inputPaths.add(args[i]);
			}
		}
		OBOSession session = AdapterUtil.parseFiles(ontPaths, allowDangling);
		mapper.setSession(session);
		if (useReasoner) {
			System.err.println("Setting up reasoner...");
			ReasonedLinkDatabase reasoner = reasonerFactory.createReasoner();
			mapper.setReasoner(reasoner);
			reasoner.setLinkDatabase(session.getLinkDatabase());
			System.err.println("Revving up reasoner...");
			reasoner.recache();
		}
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
		
		// now perform the mapping
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
				.println("obo-mapper [-?] -ontology <filename 1> ... -ontology <filename N> \\\n"
						 + "            [-subset <subset1> ... -subset <subsetN>]\\\n"
						 + "             [-c] [-followconsider] \\\n"
						 + "             [-o <outputfile>] \\\n"
						 + "             assocfile1 [assocfile2 ..assocfileN]");
		System.err
				.println("  -?                         - Writes this page to stderr and exits.");
		System.err
		.println("  -ontology <filename> - Loads the ontology with this filename\n"
				+ "                      multiple ontologies can be passed; use -ontology before each one\n"
				+ "                      If you are mapping to a subset, you do not need to pass an extra 'slim' file\n"
				+ "                      However, if the subset categorization is NOT included in the main .obo file, you\n"
				+ "                      can pass in more files this way. E.g."
				+ "                      -ontology gene_ontology_edit.obo -ontology my_slims.obo\n");
		System.err
		.println("  -subset <subsetID> - Maps to this subset. E.g. 'goslim_generic'\n"
				+ "                      multiple subsets can be passed; use -subset before each one\n"
				+ " \n");
		System.err
		.println("  -followconsider    - If set, annotations to obsolete nodes will be first mapped"
				+ "                      via the consider link\n"
				+ "                      Note that traversal through the replaced_by link is automatic\n");
		System.err
		.println("  -c                   - Count mode"
				+ "                      If set, the total distinct number of entities (eg gene products) .\n"
				+ "                      Will be summarised for each mapped class. *Implied* counts will be used\n");
		System.err
		.println("  -out <outfile> - Output file\n"
				+ "                      Format depends on whether -c option is set.\n"
				+ "                      If not set, output file with mirror input files, with OBO IDs mapped forward\n");

			System.exit(exitCode);
	}
}
