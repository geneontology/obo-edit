package org.obo.dataadapter;

import java.util.*;

import org.bbop.dataadapter.*;
import org.bbop.io.ProgressableInputStream;
import org.obo.datamodel.*;

import java.io.*;
import java.net.URLEncoder;

public class OWLURLReaderAdapter implements OBOAdapter {

	protected static final String RESTFUL_URL = "http://www.berkeleybop.org/obo-conv.cgi";

	protected String path;

	protected AdapterConfiguration config;

	protected ProgressableInputStream pfis;

	protected boolean cancelled = false;

	protected List listeners = new Vector();

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

	public Object doOperation(IOOperation op, AdapterConfiguration oldconfig,
			Object o) throws DataAdapterException {
		this.config = oldconfig;
		cancelled = false;
		if (op.equals(READ_ONTOLOGY)) {
			if (oldconfig instanceof FileAdapterConfiguration) {
				FileAdapterConfiguration config = (FileAdapterConfiguration) oldconfig;
				if (config.getReadPaths().size() == 1) {
					path = (String) config.getReadPaths().iterator().next();
					try {
						return getRoot();
					} catch (DataAdapterException ex) {
						if (cancelled)
							throw new CancelledAdapterException();
						else
							throw ex;
					}
				}
			}
			throw new DataAdapterException("Bad configuration");
		}
		return null;
	}

	public String getID() {
		return "OBO:OWLURL";
	}

	public String getName() {
		return "OWL URL Reader";
	}

	public IOOperation[] getSupportedOperations() {
		IOOperation[] supported = { WRITE_ONTOLOGY, READ_ONTOLOGY };
		return supported;
	}
	
	public String getProgressString() {
		return engine.getProgressString();
	}
	
	public Number getProgressValue() {
		return engine.getProgressValue();
	}
	
	protected OBOParseEngine engine;

	public OBOSession getRoot() throws DataAdapterException {
		try {
			String readPath;
			try {
				readPath = RESTFUL_URL+"?url="+URLEncoder.encode(path, "UTF-8")+"&format=obo&style=basic";
			} catch (UnsupportedEncodingException ex) {
				throw new DataAdapterException("Could not construct URL", ex);
			}
			DefaultOBOParser parser = new DefaultOBOParser();
			parser.setAllowDanglingParents(true);
			engine = new OBOParseEngine(parser);

			engine.setPath(readPath);
			engine.parse();
			
			OBOSession history = parser.getSession();
			return history;
		} catch (Exception e) {
			throw new DataAdapterException(e, "Load error");
		}
	}
}
