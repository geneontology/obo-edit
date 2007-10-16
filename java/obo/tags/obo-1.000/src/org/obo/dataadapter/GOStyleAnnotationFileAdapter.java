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
public class GOStyleAnnotationFileAdapter implements OBOEditAdapter {

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
		if (op.equals(OBOEditAdapter.READ_ONTOLOGY)) {
			try {
				session = new OBOSessionImpl();
				for (String fp : ioprofile.getReadPaths()) {
					LineNumberReader lnr = 
						new LineNumberReader(new FileReader(fp));
					for (String line=lnr.readLine(); line != null; line = lnr.readLine()) {

						System.out.println("line= "+line);
						Pattern p = Pattern.compile("\t");
						//parse based on tab...will be delimiter in future
						String[] colvals = p.split(line);
						parseAnnotation(colvals);
						System.out.println("parsed: "+line);

					}
					lnr.close();
				}
			}
			catch (Exception e) {
				// TODO
			}
			return session;
		} else if (op.equals(OBOEditAdapter.WRITE_ONTOLOGY)) {
			java.util.List<FilteredPath> filteredPaths = new LinkedList<FilteredPath>();

			if (ioprofile.getBasicSave()) {
				filteredPaths.add(new OBOSerializationEngine.FilteredPath(
						null, ioprofile.getWritePath()));
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
		System.out.println("  setting subj to "+subj);
		ann.setSubject(subj);
		if (subjectID != lastSubjectID) {
			lastSubjectID = subjectID;
			subj.setName(colvals[2]);
			Synonym longname = new SynonymImpl(colvals[9]);
			((OBOClass)subj).addSynonym(longname);
			
			LinkedObject tax = getSessionLinkedObject(taxID);
			OBOProperty subj2taxRel = null;
			Link subj2tax = new OBORestrictionImpl(subj, subj2taxRel, tax);
		}

		LinkedObject obj = getSessionLinkedObject(objectID);
		ann.setObject(obj);
		if (objectID != lastObjectID) {
			lastObjectID = objectID;
			
		}
		System.out.println("  done ann");
		//items.add(item);
		//session.addObject(ann);
		return ann;
	}
	
	protected void parseEvidence(Annotation ann, String evCode, String withExpr) {
		Pattern p = Pattern.compile("|");
		String[] withVals = p.split(withExpr);
		String evidenceID = "_:ev"+nextEvidenceID;
			nextEvidenceID++;
		LinkedObject ev = 
			(LinkedObject) session.getObjectFactory().
			createObject(evidenceID, OBOClass.OBO_INSTANCE, true);
		session.addObject(ev);
		//ev.setType(evCode);
		for (String s: withVals) {
			LinkedObject withObj =
				(LinkedObject) session.getObjectFactory().
				createObject(s, OBOClass.OBO_INSTANCE, true);
			OBOProperty ev2withRel = null;
			Link ev2with = new InstancePropertyValue(ev);
			ev2with.setParent(withObj);
		}
		ann.addEvidence(ev);
	}

	public LinkedObject getSessionLinkedObject(String id) {
		System.out.println("getting/adding obj:"+id);
		LinkedObject obj = (LinkedObject)session.getObject(id);
		if (obj == null) {
			obj =
				(LinkedObject) session.getObjectFactory().
				createObject(id, OBOClass.OBO_CLASS, true);
			System.out.println("  got obj:"+obj);
			session.addObject(obj);
		}
		return obj;
	}

	public String getID() {
		return "OBOEDIT:GOStyleAnnotation";
	}

	public String getName() {
		return "OBO-Edit Annotation Adapter";
	}

	public IOOperation[] getSupportedOperations() {
		IOOperation[] supported = { OBOEditAdapter.READ_ONTOLOGY, OBOEditAdapter.WRITE_ONTOLOGY };
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
	
	protected void writeAnnotation(PrintStream stream, Annotation annot) {
		System.out.println("writing annot= "+annot);
		IdentifiedObject su = annot.getSubject();
		stream.print(su);
		
		stream.print("\t");
		IdentifiedObject ob = annot.getObject();
		stream.print(ob);
		stream.print("\t");
		stream.print("\n");
		
		System.out.println(" su+ob "+su+" "+ob);
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

	public String getProgressString() {
		return null;
	}

	public Number getProgressValue() {
		return null;
	}

}
