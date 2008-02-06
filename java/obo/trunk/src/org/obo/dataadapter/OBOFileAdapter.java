package org.obo.dataadapter;

import java.util.*;

import org.bbop.dataadapter.*;
import org.bbop.io.IOUtil;
import org.bbop.util.*;
import org.obo.dataadapter.OBOSerializationEngine.FilteredPath;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.WriteCachedOBOSession;
import org.obo.identifier.IDProfile;
import org.obo.nlp.SemanticParser;

public class OBOFileAdapter implements OBOAdapter {

	protected OBOAdapterConfiguration ioprofile;

	protected java.util.List progressListeners = new LinkedList();

	protected ParseEngine engine;

	protected OBOSerializationEngine serializeEngine;

	protected StringBuffer buffer = new StringBuffer();

	protected boolean cancelled = false;

	protected OBOMetaData metaData;

	protected GraphicalUI advancedUI;

	protected String userName;
	protected String autogenString;
	protected IDProfile idProfile;

	protected IOOperation<?, ?> op;

	public OBOFileAdapter() {
	}

	public static class OBOAdapterConfiguration extends
			FileAdapterConfiguration {
		
		// WARNING: you need to set this AND basicSave=false. --CJM
		protected boolean allowDangling = false;

		protected boolean failFast = false;

		protected boolean saveImplied;

		protected java.util.List saveRecords = new ArrayList();

		protected boolean basicSave = true;

	        protected String serializer = "OBO_1_2";  // new default

		protected String impliedType = "Save for presentation";
		
		protected SemanticParser semanticParser;

		public OBOAdapterConfiguration() {
		}

		public void setSerializer(String serializer) {
			this.serializer = serializer;
		}

		public void setFailFast(boolean failFast) {
			this.failFast = failFast;
		}

		public boolean getFailFast() {
			return failFast;
		}

		public String getSerializer() {
			return serializer;
		}

		public boolean getBasicSave() {
			return basicSave;
		}

		public void setBasicSave(boolean basicSave) {
			this.basicSave = basicSave;
		}

		public java.util.List getSaveRecords() {
			return saveRecords;
		}

		public void setSaveRecords(java.util.List saveRecords) {
			if (saveRecords.contains(null))
				(new Exception("Null save record added to profile"))
						.printStackTrace();
			this.saveRecords = saveRecords;
		}

		public boolean getAllowDangling() {
			return allowDangling;
		}

		public void setAllowDangling(boolean allowDangling) {
			this.allowDangling = allowDangling;
		}

		public SemanticParser getSemanticParser() {
			return semanticParser;
		}

		public void setSemanticParser(SemanticParser semanticParser) {
			this.semanticParser = semanticParser;
		}
		
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

	public String getID() {
		return "OBO:OBO_Adapter";
	}

	public String getName() {
		return "OBO Flat File Adapter";
	}

	public IOOperation[] getSupportedOperations() {
		IOOperation[] supported = { READ_ONTOLOGY, WRITE_ONTOLOGY };
		return supported;
	}

	public AdapterConfiguration getConfiguration() {
		return ioprofile;
	}

	public String getProgressString() {
		if (op.equals(READ_ONTOLOGY)) {
			if (engine != null)
				return engine.getProgressString();
		}

		if (op.equals(WRITE_ONTOLOGY)) {
			if (serializeEngine != null)
				return serializeEngine.getProgressString();
		}

		return null;
	}

	public Number getProgressValue() {
		if (op.equals(READ_ONTOLOGY)) {
			if (engine != null)
				return engine.getProgressValue();
		}

		if (op.equals(WRITE_ONTOLOGY)) {
			if (serializeEngine != null)
				return serializeEngine.getProgressValue();
		}

		return null;
	}

	public <INPUT_TYPE, OUTPUT_TYPE> OUTPUT_TYPE doOperation(
			IOOperation<INPUT_TYPE, OUTPUT_TYPE> op,
			AdapterConfiguration configuration, INPUT_TYPE input)
			throws DataAdapterException {
		if (!(configuration instanceof OBOAdapterConfiguration)) {
			throw new DataAdapterException("Invalid configuration; this "
					+ "adapter requires an "
					+ "OBOAdapterConfiguration object.");
		}
		this.op = op;
		if (op.equals(READ_ONTOLOGY)) {
			try {
				cancelled = false;
				this.ioprofile = (OBOAdapterConfiguration) configuration;
				DefaultOBOParser parser = new DefaultOBOParser();
				// CJM: Not sure I understand the following logic
				// be careful - setting allowdangling is not enough
				parser.setAllowDanglingParents(ioprofile.getAllowDangling()
						&& !ioprofile.getBasicSave());
				parser.setFailFast(ioprofile.getFailFast());
				engine = new OBOParseEngine(parser);

				engine.setPaths(ioprofile.getReadPaths());
				System.err.println("Reading " + ioprofile.getReadPaths());
				engine.parse();

				OBOSession history = parser.getSession();
				metaData = parser.getMetaData();
				history.setLoadRemark(createLoadRemark());
				// return new WriteCachedOBOSession(history);
				return (OUTPUT_TYPE) history;
			} catch (OBOParseException e) {
				e.printStackTrace();
				if (cancelled)
					throw new CancelledAdapterException();

				throw new DataAdapterException(e, "Load error, line "
						+ e.getLineNum());
			} catch (Throwable e) {
				e.printStackTrace();
				if (cancelled)
					throw new CancelledAdapterException();

				throw new DataAdapterException(e, "Load error");
			}
		} else if (op.equals(WRITE_ONTOLOGY)) {
			try {
				cancelled = false;
				OBOSerializer serializer = null;
				this.ioprofile = (OBOAdapterConfiguration) configuration;

				if (ioprofile.getSerializer().equals("OBO_1_2"))
					serializer = new OBO_1_2_Serializer();
				else if (ioprofile.getSerializer().equals("OBO_1_0"))
					serializer = new OBO_1_0_Serializer();

				if (serializer == null)
					throw new DataAdapterException("Could not serialize to "
							+ "serializer " + ioprofile.getSerializer());
				serializeEngine = new OBOSerializationEngine();
				serializeEngine.setUsername(getUserName());
				serializeEngine.setAutogenString(getAutogenString());
				serializeEngine.setCurrentProfile(getIDProfile());

				java.util.List<FilteredPath> filteredPaths = new LinkedList<FilteredPath>();

				if (ioprofile.getBasicSave()) {
					filteredPaths.add(new OBOSerializationEngine.FilteredPath(
							null, null, ioprofile.getWritePath()));
				} else {
					filteredPaths.addAll(ioprofile.getSaveRecords());
				}

				Iterator it = progressListeners.iterator();

				serializeEngine.serialize((OBOSession) input, serializer,
						filteredPaths);

				return (OUTPUT_TYPE) input;
				/*
				 * serializer.setParentAdapter(this); return
				 * serializer.write(ioprofile, (OBOSession) input);
				 */
			} catch (DataAdapterException e) {
				e.printStackTrace();
				throw e;
			}
		} else
			throw new DataAdapterException("Operation " + op
					+ " not supported!");
	}

	public void cancel() {
		cancelled = true;
		if (engine != null)
			engine.cancel();
		if (serializeEngine != null)
			serializeEngine.cancel();
	}

	protected String createLoadRemark() {
		StringBuffer out = new StringBuffer();
		Iterator it = ioprofile.getReadPaths().iterator();
		for (int i = 0; it.hasNext(); i++) {
			String path = (String) it.next();
			path = IOUtil.getShortName(path);

			if (i != 0)
				out.append(", ");
			out.append(path);
		}
		return out.toString();
	}

	public OBOMetaData getMetaData() {
		return metaData;
	}

	public GraphicalUI getAdvancedUI() {
		return advancedUI;
	}

	public void setAdvancedUI(GraphicalUI advancedUI) {
		this.advancedUI = advancedUI;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public String getAutogenString() {
		return autogenString;
	}

	public void setAutogenString(String autogenString) {
		this.autogenString = autogenString;
	}

	public IDProfile getIDProfile() {
		return idProfile;
	}

	public void setIDProfile(IDProfile idProfile) {
		this.idProfile = idProfile;
	}

}
