package org.obo.dataadapter;

import java.io.File;
import java.util.*;

import org.bbop.dataadapter.*;
import org.bbop.io.IOUtil;
import org.obo.dataadapter.OBOSerializationEngine.FilteredPath;
import org.obo.datamodel.*;
import org.obo.identifier.IDProfile;
import org.obo.nlp.SemanticParser;

import org.apache.log4j.*;

public class OBOFileAdapter implements OBOAdapter {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBOFileAdapter.class);

	protected OBOAdapterConfiguration ioprofile;

    //	protected List progressListeners = new LinkedList(); // not used

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

		protected boolean allowDangling = false;

		protected boolean doIsaClosure = false;

		protected boolean followImports = true;

		protected boolean failFast = false;

		protected boolean saveImplied;

		protected List saveRecords = new ArrayList();

		// If we make this false, it gets made true anyway in doOperation
		// and if we comment that out, save doesn't work at all!
		protected boolean basicSave = true;

		protected String serializer = "OBO_1_2";  // new default

		protected String impliedType = "Save trimmed links";

		protected SemanticParser semanticParser;

		protected boolean includeExplanations = false;

		protected boolean includeNames = false;


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

		public List getSaveRecords() {
			return saveRecords;
		}

		public void setSaveRecords(List saveRecords) {
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

		public boolean getClosureforDangling() {
			return allowDangling;
		}

		public void setClosureforDangling(boolean doIsaClosure) {
			this.doIsaClosure = doIsaClosure;
		}

		public boolean getFollowImports() {
			return followImports;
		}

		public void setFollowImports(boolean followImports) {
			this.followImports = followImports;
		}


		public SemanticParser getSemanticParser() {
			return semanticParser;
		}

		public void setSemanticParser(SemanticParser semanticParser) {
			this.semanticParser = semanticParser;
		}

		/**
		 * @return  - true if the adapter is requested to report reasoner explanations for implied links
		 */
		public boolean isIncludeExplanations() {
			return includeExplanations;
		}

		/**
		 * @param includeExplanations - true if the adapter is requested to report reasoner explanations for implied links
		 */
		public void setIncludeExplanations(boolean includeExplanations) {
			this.includeExplanations = includeExplanations;
		}

		public boolean isIncludeNames() {
			return includeNames;
		}

		public void setIncludeNames(boolean includeNames) {
			this.includeNames = includeNames;
		}
	}

	public DataAdapterUI getPreferredUI() {

		FileAdapterUI ui = new FileAdapterUI() {
			private static final long serialVersionUID = 8709597443707849569L;

			@Override
			public AdapterConfiguration createEmptyConfig() {
				return new OBOAdapterConfiguration();
			}

			@Override
			public void acceptComponentConfig(boolean storeonly)
			throws DataAdapterUIException {
				super.acceptComponentConfig(storeonly);
				// ?
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
				//                                logger.debug("doOperation: ioprofile = " + ioprofile); // DEL
				DefaultOBOParser parser = new DefaultOBOParser();
				// CJM: Not sure I understand the following logic
				// be careful - setting allowdangling is not enough
				parser.setAllowDanglingParents(ioprofile.getAllowDangling());
				parser.setFailFast(ioprofile.getFailFast());
				parser.setFollowImports(ioprofile.getFollowImports());
				engine = new OBOParseEngine(parser);

				engine.setPaths(ioprofile.getReadPaths());
				logger.info("Reading " +	ioprofile.getReadPaths()
						+ "\n\t (allowDangling = " + ioprofile.getAllowDangling() + ")"
						+ "\n\t (followImports = " + ioprofile.getFollowImports() + ")");
				engine.parse();
				logger.debug("Done parsing file");
				OBOSession history = parser.getSession();
				metaData = parser.getMetaData();
                                // Set title string for OE titlebar
				// (Sets the title string, but the actual changing of frame title happens when FrameNameUpdateTask hears a reconfigEvent)
				history.setLoadRemark(createLoadRemark(ioprofile.getReadPaths()));
                                // Remember most recently loaded file(s) so it can be checked by CheckOriginalFileTask to see if it changes on disk
                                history.setCurrentFilenames(ioprofile.getReadPaths());
				// return new WriteCachedOBOSession(history);
				return (OUTPUT_TYPE) history;
			} catch (OBOParseException e) {
				e.printStackTrace();
				if (cancelled)
					throw new CancelledAdapterException();

				throw new DataAdapterException(e, "Load Error, line "
						+ e.getLineNum());
			} catch (Throwable e) {
				e.printStackTrace();
				if (cancelled)
					throw new CancelledAdapterException();

				throw new DataAdapterException(e, "Load error with path "+engine.getCurrentPath());
			}
		} else if (op.equals(WRITE_ONTOLOGY)) {
			try {
				cancelled = false;
				OBOSerializer serializer = null;
                                //                                logger.debug("OBOFileAdapter.doOperation: this.ioprofile was " + ioprofile + "; setting it to passed configuration " + configuration); // DEL
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

				List<FilteredPath> filteredPaths = new LinkedList<FilteredPath>();

				if (ioprofile.getBasicSave()) {
					filteredPaths.add(new OBOSerializationEngine.FilteredPath(null, null, ioprofile.getWritePath()));
				} else {
					filteredPaths.addAll(ioprofile.getSaveRecords());
				}
				/**FilteredPath sets the allowdangling modifier to false on initialization by default.
				 * this works for all other cases wrt the gui where allowdangling is false by default.
				 * it is true by default for obo2obo though, hence
				 * explicitly setting allowdangling as per ioprofile settings since the ioprofile.getSaveRecords does not import that modifier.
				 * 
				 * */
                                // Using the ioprofile to setAllowDangling for the filteredPath(s)
                                // fixed that problem with obo2obo but broke saves with allowDangling true
                                // in OBO-Edit.  Commenting out the filteredPath.setAllowDangling(ioprofile.allowDangling)
                                // calls fixed OE but broke obo2obo.
                                // In trying to debug this, I was startled to find that the ioprofile
                                // that was available in this method was often not the right ioprofile.
                                // I'm not sure what's up with that and why it doesn't have more bad effects.
                                // My compromise fix is to propagate ioprofile's allowDangling setting
                                // to the filteredPaths only if ioprofile.allowDangling is true.
                                // This seems to work for both obo2obo and OBO-Edit for the cases I tried,
                                // but more testing would be good.
                                if (ioprofile.allowDangling) {
                                    for(FilteredPath filteredPath : filteredPaths) {
                                        //                                        logger.debug("OBOFileAdapter: filteredPath = " + filteredPath + "; filteredPath.getAllowDangling = " + filteredPath.getAllowDangling()); // DEL
                                        filteredPath.setAllowDangling(ioprofile.allowDangling);
                                    }
                                }
				logger.info("Writing " + filteredPaths + " (serializer = " + ioprofile.getSerializer() + ", basicSave = " + ioprofile.getBasicSave() + ")");
                                //                                logger.debug("ioprofile = " + ioprofile + "; ioprofile.allowDangling = " + ioprofile.allowDangling); // DEL

				serializeEngine.serialize((OBOSession) input, serializer,filteredPaths);

				Collection<String> writePath = new Vector<String>();
				if (ioprofile.getBasicSave()) {
                                    writePath.add(ioprofile.getWritePath());
				} else {
                                    // In the Advanced Save profile, the write path
                                    // is not saved in writePath but in saveRecords.
                                    List<FilteredPath> saveRecords = ioprofile.getSaveRecords();
                                    if (!saveRecords.isEmpty()) {
                                        FilteredPath filteredPath = saveRecords.get(0);
                                        writePath.add(filteredPath.getPath());
                                    } // What if it IS empty? What then?
				}
                                //                                logger.debug("writePath = " + writePath); // DEL
				OBOSession history = (OBOSession) input;
                                // Set title string for OE titlebar
				// (Sets the title string, but the actual changing of frame title happens when FrameNameUpdateTask hears a reconfigEvent)
                                history.setLoadRemark(createLoadRemark(writePath));
                                // Remember most recently saved file so it can be checked by CheckOriginalFileTask to see if it changes on disk
                                history.setCurrentFilenames(getFirstPath(writePath));

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
	    return createLoadRemark(ioprofile.getReadPaths());
	}

	protected String createLoadRemark(Collection<String> paths) {
		StringBuffer out = new StringBuffer();
		Iterator it = paths.iterator();
		for (int i = 0; it.hasNext(); i++) {
			String path = (String) it.next();
                        //                        logger.debug("createLoadRemark: path " + i + " = " + path); // DEL
			path = IOUtil.getShortName(path);
                        if (path == null)
                            continue;

			if (i != 0)
				out.append(", ");
			out.append(path);
		}
		return out.toString();
	}

    private String getFirstPath(Collection<String> paths) {
        Iterator it = paths.iterator();
        for (int i = 0; it.hasNext(); i++) {
            String relativePath = (String) it.next();
            if (relativePath == null) // Actually, this shouldn't happen anymore
                continue;
            File file = new File(relativePath);
            return file.getAbsolutePath();
        }
        return null;
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
          if (userName == null) {
            userName = System.getProperty("user.name");
          }
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
