package org.obo.dataadapter;

import java.io.BufferedOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.regex.Pattern;

import org.bbop.dataadapter.AdapterConfiguration;
import org.bbop.dataadapter.DataAdapterException;
import org.bbop.dataadapter.DataAdapterUI;
import org.bbop.dataadapter.DataAdapterUIException;
import org.bbop.dataadapter.FileAdapterUI;
import org.bbop.dataadapter.GraphicalUI;
import org.bbop.dataadapter.IOOperation;
import org.bbop.io.ProgressableInputStream;
import org.bbop.io.SafeFileOutputStream;
import org.obo.annotation.datamodel.Annotation;
import org.obo.annotation.datamodel.impl.AnnotationImpl;
import org.obo.dataadapter.OBOFileAdapter.OBOAdapterConfiguration;
import org.obo.dataadapter.OBOSerializationEngine.FilteredPath;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Instance;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.NamespacedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.PropertyValue;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.SynonymedObject;
import org.obo.datamodel.impl.InstancePropertyValue;
import org.obo.datamodel.impl.OBOSessionImpl;
import org.obo.datamodel.impl.SynonymImpl;
import org.obo.util.IDUtil;
import org.obo.util.TermUtil;

/**
 * @author cjm
 *
 * GO Annotation format
 * http://www.geneontology.org/doc/GO.annotation.shtml#file
 * http://www.geneontology.org/GO.format.annotation.shtml
 */
import org.apache.log4j.*;

public class GOStyleAnnotationFileAdapter implements OBOAdapter {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GOStyleAnnotationFileAdapter.class);

	protected String path;
	protected AdapterConfiguration config;
	protected ProgressableInputStream pfis;
	protected boolean cancelled = false;
	protected List listeners = new Vector();
	protected OBOAdapterConfiguration ioprofile;
	protected List streams = new LinkedList();
	protected OBOSession session;
	protected String lastSubjectID;
	protected static int nextEvidenceID = 0;
	protected GraphicalUI advancedUI;
	protected Map<Namespace,String> NamespaceCodeMap = new HashMap<Namespace,String>();



	protected String lastObjectID;

	public void cancel() {
		try {
			cancelled = true;
			if (pfis != null)
				pfis.close();
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}
	

	public AdapterConfiguration getConfiguration() {
		return config;
	}
	
	
	public <INPUT_TYPE, OUTPUT_TYPE> OUTPUT_TYPE doOperation(
			IOOperation<INPUT_TYPE, OUTPUT_TYPE> op, AdapterConfiguration configuration,
			INPUT_TYPE  o) throws DataAdapterException {
		if (!(configuration instanceof OBOAdapterConfiguration)) {
			throw new DataAdapterException("Invalid configuration; this "
					+ "adapter requires an "
					+ "OBOAdapterConfiguration object.");
		}
		cancelled = false;
		this.ioprofile = (OBOAdapterConfiguration) configuration;
		if (op.equals(OBOAdapter.READ_ONTOLOGY)) {
			try {
				session = new OBOSessionImpl();
				for (String fp : ioprofile.getReadPaths()) {
					LineNumberReader lnr = 
						new LineNumberReader(new FileReader(fp));
					for (String line=lnr.readLine(); line != null; line = lnr.readLine()) {
						if (line.startsWith("!"))
							continue;

						logger.info("line= "+line);
						//parse based on tab...will be delimiter in future
						String[] colvals1 = line.split("\t");
						String[] colvals = new String[16];
						for (int i=0; i<colvals1.length; i++) {
							colvals[i]=colvals1[i];
						}
						parseAnnotation(colvals);
						logger.info("parsed: "+line);

					}
					lnr.close();
				}
			}
			catch (Exception e) {
				logger.info(e);
				throw new DataAdapterException(e, "read error");
			}
			return (OUTPUT_TYPE)session;
		} else if (op.equals(OBOAdapter.WRITE_ONTOLOGY)) {
			session = (OBOSession)o;
			java.util.List<FilteredPath> filteredPaths = new LinkedList<FilteredPath>();

			if (ioprofile.getBasicSave()) {
				filteredPaths.add(new OBOSerializationEngine.FilteredPath(
						null, null, ioprofile.getWritePath()));
			} else {
				logger.info("gsr="+ioprofile.getSaveRecords());
				filteredPaths.addAll(ioprofile.getSaveRecords());
			}
			streams.clear();
			Iterator<FilteredPath> it = filteredPaths.iterator();

			while (it.hasNext()) {
				FilteredPath filteredPath = it.next();
				try {
					logger.info("fp="+filteredPath);
					logger.info("fpp="+filteredPath.getPath());
					SafeFileOutputStream sfos = new SafeFileOutputStream(
							filteredPath.getPath());
					streams.add(sfos);
					PrintStream stream = new PrintStream(new BufferedOutputStream(
							sfos));
					write((OBOSession) o, stream, filteredPath);
				return (OUTPUT_TYPE)o;
				}  catch (IOException ex) {
					throw new DataAdapterException("Bad configuration");
				// write((OBOSession) o);
				}
			}
		}
		return null;
	}
	
	public OBOProperty getEntityToTaxonProperty() {
		LinkedObject lo =getSessionLinkedObject("has_taxon",OBOClass.OBO_PROPERTY);
		return TermUtil.castToProperty(lo);
	}
	
	protected Annotation parseAnnotation(String[] colvals) {
		Annotation ann = new AnnotationImpl(IDUtil.fetchTemporaryID(session));
		String dbspace = colvals[0];
		String localID = colvals[1];
		String name = colvals[2];
		String subjectID = dbspace+":"+localID;
		String qual = colvals[3];
		String objectID = colvals[4];
		String refVal = colvals[5];
		String evCode = colvals[6];
		String ref = colvals[7];
		String aspect = colvals[8];
		String fullName = colvals[9];
		String synVal = colvals[10];
		String type = colvals[11]; 
		String taxID = colvals[12]; // TODO - multi-species
		String date = colvals[13];
		String assignedBy = colvals[14];
		
		session.addObject(ann);
		logger.info("new ann:"+ann);
		
		parseReferenceField(ann, refVal);
	
		logger.info("  parsing ev");
        parseEvidence(ann,evCode,ref);
        logger.info("  parsed ev");
        Namespace subjectNS = session.getNamespace(dbspace);
        if (subjectNS == null)
        	subjectNS = session.getObjectFactory().createNamespace(dbspace, "");

        // give the annotation the same namespace as the source
        ann.setNamespace(subjectNS);
        
		LinkedObject subj =  getSessionLinkedObject(subjectID);
		if (subj.getNamespace() == null) {
			subj.setNamespace(subjectNS);			
		}
		OBOProperty rel =  
			TermUtil.castToProperty(getSessionLinkedObject("OBO_REL:has_role",
					OBOClass.OBO_PROPERTY));
		ann.setRelationship(rel);
		logger.info("  setting subj to "+subj);
		ann.setSubject(subj);
		if (subjectID != lastSubjectID) {
			lastSubjectID = subjectID;
			subj.setName(name);
			parseSynonymField(ann,fullName,subj);
			parseSynonymField(ann,synVal,subj);
						
		}

		LinkedObject obj = (LinkedObject)session.getObject(objectID);
		if (obj == null) {
			obj =
				(LinkedObject) session.getObjectFactory().
				createDanglingObject(objectID, false);
			//logger.info("  got obj:"+obj);
			session.addObject(obj);
		}

		ann.setObject(obj);
		parseAspect(ann,aspect);
		if (objectID != lastObjectID) {
			lastObjectID = objectID;
			
		}
		parseQualifierField(ann,qual);
		// TODO parseTypeField(ann,type);
		parseTaxonField(ann,taxID);
		parseDateField(ann,date);
		parseAssignedByField(ann,assignedBy);

		logger.info("  done ann");
		//items.add(item);
		//session.addObject(ann);
		return ann;
	}
	
	protected void parseQualifierField(Annotation ann, String qualField) {
		for(String q : splitOn(qualField,"\\|")) {
			if (q.equals("NOT")) {
				ann.setIsNegated(true);
			}
			else {
				logger.info("Cannot handle qual:"+q);
				// TODO
			}
		}
	}
	
	protected void parseAspect(Annotation ann, String aspect) {

		LinkedObject obj = ann.getObject();
		if (obj.getNamespace() != null)
			return; // no need of aspect
		if (aspect == null || aspect.equals(""))
			return;
		
		// hard-coded for GO
		// only required for roundtripping GO annotation files 
		String ns;
		
		if (aspect.equals("C"))
			ns = "cellular_component";
		else if (aspect.equals("P"))
			ns = "biological_process";
		else if (aspect.equals("F"))
			ns = "molecular_function";
		else
			ns = aspect;
		Namespace nsObj = session.getNamespace(ns);
		if (nsObj == null)
			nsObj = session.getObjectFactory().createNamespace(ns, "");
		if (obj instanceof NamespacedObject)
			obj.setNamespace(nsObj);
	}
	
	protected void parseSynonymField(Annotation ann, String synField, LinkedObject ae) {
		for(String s : splitOn(synField,"\\|")) {
			Synonym longname = new SynonymImpl(s);
			((SynonymedObject)ae).addSynonym(longname);
		}
	}

	protected void parseReferenceField(Annotation ann, String refField) {
		for(String s : refField.split("\\|")) {
			ann.addSource(s);
		}
	}

	protected void parseDateField(Annotation ann, String dateField) {
		
		//ann.setModificationDate(date) TODO
	}

	protected void parseAssignedByField(Annotation ann, String abField) {
		// TODO
		ann.setAssignedBy(abField);
	}
	protected void parseEvidence(Annotation ann, String evCode, String withExpr) {
		Pattern p = Pattern.compile("|");
		String[] withVals = p.split(withExpr);
		String evidenceID = "_:ev"+nextEvidenceID;
			nextEvidenceID++;
		Instance ev = 
			(Instance) session.getObjectFactory().
			createObject(evidenceID, OBOClass.OBO_INSTANCE, true);
		session.addObject(ev);
		OBOClass evCodeClass = TermUtil.castToClass(getSessionLinkedObject(evCode));
		ev.setType(evCodeClass);
		//ev.setType(evCode);
		for (String s: withVals) {
			LinkedObject withObj =
				(LinkedObject) session.getObjectFactory().
				createObject(s, OBOClass.OBO_INSTANCE, true);
			OBOProperty ev2withRel = null;
			// TODO: 
			session.getObjectFactory().createPropertyValue("with", s);
			Link ev2with = new InstancePropertyValue(ev);
			ev2with.setParent(withObj);
			ev2with.setChild(ev);
		}
		ann.addEvidence(ev);
	}

	protected void parseTaxonField(Annotation ann, String taxVal) {
		Pattern p = Pattern.compile("\\|");
		String[] taxIDs = p.split(taxVal);
		
		for (int i=0; i<taxIDs.length; i++) {
			String taxID = taxIDs[i];
			LinkedObject taxObj = getSessionLinkedObject(taxID);
			if (i==0) {
				LinkedObject ae = ann.getSubject();
				//makeLink(ae,null,taxObj);
				OBOProperty subj2taxRel = getEntityToTaxonProperty();
				// TODO: link of correct metatype
				Link subj2tax = 
					session.getObjectFactory()
					.createOBORestriction(ae, subj2taxRel, taxObj, false);
				ae.addParent(subj2tax);
					//new OBORestrictionImpl(ae, subj2taxRel, taxObj);
			}
			else {
				// multi-species interactions, 2ary taxon. TODO
				// makeLink(ann,null,taxObj);
			}
		}
	}
	
	
	
	// TODO: should we return a DanglingObject?
	// can we do this and resolve later?
	public LinkedObject getSessionLinkedObject(String id) {
		return getSessionLinkedObject(id, OBOClass.OBO_CLASS);
	}
	
	// TODO: move somewhere more generic
	public LinkedObject getSessionLinkedObject(String id, OBOClass metaclass) {
		logger.info(session+" getting/adding obj:"+id);
		LinkedObject obj = (LinkedObject)session.getObject(id);
		if (obj == null) {
			obj =
				(LinkedObject) session.getObjectFactory().
				createObject(id, metaclass, true);
//				(LinkedObject) session.getObjectFactory().
//				createDanglingObject(id, metaclass.equals(OBOClass.OBO_PROPERTY));
		//		createObject(id, metaclass, true);
			logger.info("  got obj:"+obj);
			session.addObject(obj);
		}
		return obj;
	}
	
	

	public String getID() {
		return "OBO:GOStyleAnnotation";
	}

	public String getName() {
		return "GO Association File Adapter";
	}

	public IOOperation[] getSupportedOperations() {
		IOOperation[] supported = { OBOAdapter.READ_ONTOLOGY, OBOAdapter.WRITE_ONTOLOGY };
		return supported;
	}

	public DataAdapterUI getPreferredUI() {

		FileAdapterUI ui = new FileAdapterUI() {

			/**
			 * 
			 */
			private static final long serialVersionUID = 8709597443707849569L;

			@Override
			public AdapterConfiguration createEmptyConfig() {
				return new OBOAdapterConfiguration();
			}

			@Override
			public void acceptComponentConfig(boolean storeonly)
					throws DataAdapterUIException {
				super.acceptComponentConfig(storeonly);
				((OBOAdapterConfiguration) config).setBasicSave(true);
			}

		};
		ui.setReadOperation(READ_ONTOLOGY);
		ui.setWriteOperation(WRITE_ONTOLOGY);
		GraphicalUI advancedUI = getAdvancedUI();
		if (advancedUI != null) {
			advancedUI.setSimpleUI(ui);
			ui.setAdvancedUI(advancedUI);
		}
		return ui;
	}
	
	// TODO - DRY
	public GraphicalUI getAdvancedUI() {
		return advancedUI;
	}
	public void setAdvancedUI(GraphicalUI advancedUI) {
		this.advancedUI = advancedUI;
	}

	public OBOSession write(OBOSession session, PrintStream stream, 
						FilteredPath filteredPath) throws DataAdapterException {
		try {
			logger.info("writing to" + stream);
			
			//ReusableProgressEvent rpe = new ReusableProgressEvent(this);
			//rpe.setFastVal(-1);
			//rpe.setDescription("Writing file...");
			//fireProgressEvent(rpe);
			
			LinkDatabase ldb = session.getLinkDatabase();
			logger.info("ldb= "+ldb);
			for (IdentifiedObject io : ldb.getObjects()) {
				if (io instanceof Annotation) {
					writeAnnotation(stream,(Annotation)io);					
				}
			}
			stream.close();
			return session;
		} catch (Exception e) {
			throw new DataAdapterException(e, "Write error");
		}
	}
	
	protected void writeAnnotation(PrintStream stream, Annotation annot) throws Exception {
		String[] colvals = new String[16];
		logger.info("writing annot= "+annot);
		IdentifiedObject su = annot.getSubject();

		// TODO: split
		String[] idPartArray = splitOn(su.getID(),":");
		colvals[0] = idPartArray[0];
		StringBuffer sb = new StringBuffer(idPartArray[1]);
		for (int i=2;i<idPartArray.length;i++) {
			sb.append(":"+idPartArray[i]);
		}
		colvals[1] = sb.toString();
		IdentifiedObject ob = annot.getObject();

		colvals[2] = su.getName();
		
		//TODO: qualifiers
		colvals[3] = "";
		if (annot.getIsNegated()) {
			colvals[3] = "NOT";
		}
		
		colvals[4] = ob.getID();

		Collection<LinkedObject> sources = annot.getSources();
		colvals[5] = flattenSet(sources);

		Collection<LinkedObject> evs = annot.getEvidence();
		if (evs.size() >1)
			throw new Exception("can't deal with >1 evidence per annotation yet!");
		colvals[6] = "";
		colvals[7] = "";
		for (LinkedObject ev : evs) {
			colvals[6] = ((Instance)ev).getType().getID();
			colvals[7] = flattenSet(ev.getPropertyValues());
			for (PropertyValue pv : ev.getPropertyValues()) {
				colvals[7] = "x";
			}
		}
		
		colvals[8] = getAspect(ob); // TODO 
		
		colvals[9] = "";
		// colvals[9] = flattenSet(); // TODO - synonym category, fullname
		colvals[10] = "";
		if (su instanceof SynonymedObject) {
			Set<Synonym> syns = ((SynonymedObject)su).getSynonyms();
			Collection<String> strs = new LinkedList<String>();
			for (Synonym syn : syns) {
				strs.add(syn.getText());
			}
			colvals[10] = flattenSet(strs);
			//colvals[10] = syns.toString();
		}
		// type
		colvals[11]="";
		if (su instanceof Instance)
			colvals[11] = ((Instance)su).getType().getID();
		
		// taxa
		colvals[12]="";
		OBOProperty taxRel = getEntityToTaxonProperty();
		for (Link link : ((LinkedObject)su).getParents()) {
			if (link.getType().equals(taxRel)) {
				colvals[12] = link.getParent().getID();
			}
		}
		
		colvals[13] = annot.getModificationDate() == null ?
				"" : annot.getModificationDate().toString();
		
		colvals[14] = annot.getAssignedBy() != null ? annot.getAssignedBy().getID() : "";
		
		colvals[15] = "";
		
		// write entire line
		stream.print(colvals[0]);
		for (int i=1; i<16; i++) {
			stream.print("\t");
			stream.print(colvals[i]);
		}
		stream.print("\n");
		logger.info(" su+ob "+su+" "+ob);
	}
	
	protected String flattenSet(Collection set) {
		StringBuffer s = new StringBuffer();
		for (Object o : set) {
			String token;
			if (o instanceof IdentifiedObject)
				token = ((IdentifiedObject)o).getID();
			else
				token = (String)o;
			if (s.length() == 0) {
				s.append(token);
			}
			else {
				s.append("|");
				s.append(token);
			}
		}
		return s.toString();
	}
	
	protected String getAspect (IdentifiedObject io) {
		Namespace ns = io.getNamespace();
		if (ns == null) {
			return "";
		}
		String nsId = ns.getID();
		if (nsId.equals("cellular_component"))
			return "C";
		else if (nsId.equals("molecular_function"))
			return "F";
		else if (nsId.equals("biological_process"))
			return "P";
		else
			return nsId;
	}
	
		

	public String getTermText(IdentifiedObject term)
			throws DataAdapterException {
		final StringBuffer buffer = new StringBuffer();
		OutputStream os = new OutputStream() {
			@Override
			public void write(int b) {
				buffer.append((char) b);
			}
		};
		try {
			ObjectOutputStream stream = new ObjectOutputStream(os);
			stream.writeObject(os);
		} catch (IOException ex) {
		}
		return buffer.toString();
	}

	public String[] splitOn(String s, String delim) {
		Pattern p = Pattern.compile(delim);
		return p.split(s);
		
	}

	public String getProgressString() {
		return null;
	}

	public Number getProgressValue() {
		return null;
	}

}
