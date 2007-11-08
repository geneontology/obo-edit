package org.obo.dataadapter;

import java.io.*;
import java.util.*;

import org.bbop.dataadapter.*;
import org.bbop.io.ProgressableInputStream;
import org.bbop.io.SafeFileOutputStream;
import org.obo.annotation.datamodel.Annotation;
import org.obo.annotation.datamodel.impl.AnnotationImpl;
import org.obo.dataadapter.OBOFileAdapter.OBOAdapterConfiguration;
import org.obo.dataadapter.OBOSerializationEngine.FilteredPath;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.util.IDUtil;
import org.obo.util.TermUtil;

import java.util.regex.Pattern;  

/**
 * @author cjm
 *
 * GO Annotation format
 * http://www.geneontology.org/doc/GO.annotation.shtml#file
 * http://www.geneontology.org/GO.format.annotation.shtml
 */
public class GOStyleAnnotationFileAdapter implements OBOAdapter {

	protected String path;
	protected AdapterConfiguration config;
	protected ProgressableInputStream pfis;
	protected boolean cancelled = false;
	protected List listeners = new Vector();
	protected OBOAdapterConfiguration ioprofile;
	protected List streams = new LinkedList();
	protected OBOSession session;
	protected String lastSubjectID;
	protected String lastObjectID;
	protected static int nextEvidenceID = 0;

	public DataAdapterUI getPreferredUI() {
		FileAdapterUI ui = new FileAdapterUI();
		ui.setReadOperation(READ_ONTOLOGY);
		ui.setReadOperation(WRITE_ONTOLOGY);
		return ui;
	}

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
	
	
	public Object doOperation(IOOperation op, AdapterConfiguration configuration,
			Object o) throws DataAdapterException {
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

						System.err.println("line= "+line);
						Pattern p = Pattern.compile("\t");
						//parse based on tab...will be delimiter in future
						String[] colvals1 = p.split(line);
						String[] colvals = new String[16];
						for (int i=0; i<colvals1.length; i++) {
							colvals[i]=colvals1[i];
						}
						parseAnnotation(colvals);
						System.out.println("parsed: "+line);

					}
					lnr.close();
				}
			}
			catch (Exception e) {
				System.out.println(e);
				throw new DataAdapterException(e, "read error");
			}
			return session;
		} else if (op.equals(OBOAdapter.WRITE_ONTOLOGY)) {
			session = (OBOSession)o;
			java.util.List<FilteredPath> filteredPaths = new LinkedList<FilteredPath>();

			if (ioprofile.getBasicSave()) {
				filteredPaths.add(new OBOSerializationEngine.FilteredPath(
						null, null, ioprofile.getWritePath()));
			} else {
				System.err.println("gsr="+ioprofile.getSaveRecords());
				filteredPaths.addAll(ioprofile.getSaveRecords());
			}
			streams.clear();
			Iterator<FilteredPath> it = filteredPaths.iterator();

			while (it.hasNext()) {
				FilteredPath filteredPath = it.next();
				try {
					System.err.println("fp="+filteredPath);
					System.err.println("fpp="+filteredPath.getPath());
					SafeFileOutputStream sfos = new SafeFileOutputStream(
							filteredPath.getPath());
					streams.add(sfos);
					PrintStream stream = new PrintStream(new BufferedOutputStream(
							sfos));
					write((OBOSession) o, stream, filteredPath);
				return o;
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
		String subjectID = colvals[0]+":"+colvals[1];
		String objectID = colvals[4];
		String evCode = colvals[6];
        
		String taxID = colvals[12]; // TODO - multi-species

		session.addObject(ann);
		System.out.println("new ann:"+ann);
		
		ann.addSource(colvals[5]);

		System.out.println("  parsing ev");
        parseEvidence(ann,evCode,colvals[7]);
        System.out.println("  parsed ev");
        
		LinkedObject subj =  getSessionLinkedObject(subjectID);
		OBOProperty rel =  (OBOProperty)getSessionLinkedObject("OBO_REL:has_role",OBOClass.OBO_PROPERTY);
		ann.setRelationship(rel);
		System.out.println("  setting subj to "+subj);
		ann.setSubject(subj);
		if (subjectID != lastSubjectID) {
			lastSubjectID = subjectID;
			subj.setName(colvals[2]);
			parseSynonymField(ann,colvals[9],subj);
			parseSynonymField(ann,colvals[10],subj);
						
		}

		LinkedObject obj = getSessionLinkedObject(objectID);
		ann.setObject(obj);
		parseAspect(ann,colvals[8]);
		if (objectID != lastObjectID) {
			lastObjectID = objectID;
			
		}
		parseQualifierField(ann,colvals[3]);
		// TODO parseTypeField(ann,colvals[11]);
		parseTaxonField(ann,colvals[12]);
		parseDateField(ann,colvals[13]);
		parseAssignedByField(ann,colvals[14]);

		System.out.println("  done ann");
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
				System.err.println("Cannot handle qual:"+q);
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
		obj.setNamespace(nsObj);
	}
	
	protected void parseSynonymField(Annotation ann, String synField, LinkedObject ae) {
		for(String s : splitOn(synField,"\\|")) {
			Synonym longname = new SynonymImpl(s);
			((SynonymedObject)ae).addSynonym(longname);
		}
	}

	protected void parseReferenceField(Annotation ann, String refField) {
		for(String s : splitOn(refField,"|")) {
			// TODO
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
		OBOClass evCodeClass = (OBOClass)getSessionLinkedObject(evCode);
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
		System.out.println(session+" getting/adding obj:"+id);
		LinkedObject obj = (LinkedObject)session.getObject(id);
		if (obj == null) {
			obj =
				(LinkedObject) session.getObjectFactory().
				createObject(id, metaclass, true);
			System.out.println("  got obj:"+obj);
			session.addObject(obj);
		}
		return obj;
	}

	public String getID() {
		return "OBO:GOStyleAnnotation";
	}

	public String getName() {
		return "OBO Annotation Adapter";
	}

	public IOOperation[] getSupportedOperations() {
		IOOperation[] supported = { OBOAdapter.READ_ONTOLOGY, OBOAdapter.WRITE_ONTOLOGY };
		return supported;
	}


	public OBOSession write(OBOSession session, PrintStream stream, 
						FilteredPath filteredPath) throws DataAdapterException {
		try {
			System.err.println("writing to" + stream);
			
			//ReusableProgressEvent rpe = new ReusableProgressEvent(this);
			//rpe.setFastVal(-1);
			//rpe.setDescription("Writing file...");
			//fireProgressEvent(rpe);
			
			LinkDatabase ldb = session.getLinkDatabase();
			System.out.println("ldb= "+ldb);
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
		System.out.println("writing annot= "+annot);
		IdentifiedObject su = annot.getSubject();

		// TODO: split
		colvals[0] = su.getID();
		colvals[1] = su.getID();
		IdentifiedObject ob = annot.getObject();

		colvals[2] = su.getName();
		
		//TODO: qualifiers
		colvals[3] = "";
		if (annot.getIsNegated()) {
			colvals[3] = "NOT";
		}
		
		colvals[4] = ob.getID();

		Collection<String> sources = annot.getSources();
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
		
		colvals[14] = annot.getAssignedBy();
		
		colvals[15] = "";
		
		// write entire line
		stream.print(colvals[0]);
		for (int i=1; i<16; i++) {
			stream.print("\t");
			stream.print(colvals[i]);
		}
		stream.print("\n");
		System.out.println(" su+ob "+su+" "+ob);
	}
	
	protected String flattenSet(Collection set) {
		StringBuffer s = new StringBuffer();
		for (Object o : set) {
			if (s.length() == 0) {
				s.append((String)o);
			}
			else {
				s.append("|");
				s.append((String)o);
			}
		}
		return s.toString();
	}
	
	protected String getAspect (IdentifiedObject io) {
		Namespace ns = io.getNamespace();
		if (ns == null) {
			return "";
		}
		return ns.getID();
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
