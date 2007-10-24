package org.obo.dataadapter;

import java.io.*;
import java.util.*;

import org.bbop.dataadapter.*;
import org.bbop.io.ProgressableInputStream;
import org.bbop.io.SafeFileOutputStream;
import org.bbop.util.AbstractProgressValued;
import org.obo.dataadapter.OBOFileAdapter.OBOAdapterConfiguration;
import org.obo.dataadapter.OBOSerializationEngine.FilteredPath;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.util.TermUtil;

/**
 * @author cjm
 *
 * Writes out a reasoned link database as CHILD-REL-PARENT-ISIMPLIED tab delimited quads
 * TODO: Reasoner factory?
 */
public class SimpleLinkFileAdapter extends AbstractProgressValued implements OBOAdapter {

	protected String path;
	protected AdapterConfiguration config;
	protected ProgressableInputStream pfis;
	protected boolean cancelled = false;
	protected OBOAdapterConfiguration ioprofile;
	protected List streams = new LinkedList();

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
		if (op.equals(READ_ONTOLOGY)) {
			return null;
		} else if (op.equals(WRITE_ONTOLOGY)) {
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

	public String getID() {
		return "OBO:SimpleLinkAdapter";
	}

	public String getName() {
		return "OBO Simple Link Adapter";
	}

	public IOOperation[] getSupportedOperations() {
		IOOperation[] supported = { READ_ONTOLOGY, WRITE_ONTOLOGY };
		return supported;
	}

	protected ReasonedLinkDatabase reasoner;
	
	public void setReasoner(ReasonedLinkDatabase reasoner) {
		this.reasoner = reasoner;
	}
	
	public ReasonedLinkDatabase getReasoner() {
		return reasoner;
	}

	public OBOSession write(OBOSession session, PrintStream stream, 
						FilteredPath filteredPath) throws DataAdapterException {
		try {
			setProgressString("Writing file...");
			
			ReasonedLinkDatabase fullReasoner;
			if (filteredPath.getUseSessionReasoner() &&
					getReasoner() != null) {
				fullReasoner = reasoner;
			} else {
				System.err.println("new reasoner...");
				fullReasoner = new ForwardChainingReasoner();
				fullReasoner.setLinkDatabase(new DefaultLinkDatabase(session));
				fullReasoner.recache();
			}

			for (IdentifiedObject io : fullReasoner.getObjects()) {
				if (io instanceof LinkedObject) {
					LinkedObject lo = (LinkedObject) io;
					for (Link link : fullReasoner.getParents(lo)) {
						stream.print(link.getChild().getID());
						stream.print("\t");
						stream.print(link.getType().getID());
						stream.print("\t");
						stream.print(link.getParent().getID());
						stream.print("\t");
						stream.print(TermUtil.isImplied(link));
						stream.print("\n");
					}
				}
			}
			stream.close();
			return session;
		} catch (Exception e) {
			throw new DataAdapterException(e, "Write error");
		}
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

}
