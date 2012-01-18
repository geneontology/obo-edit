package org.obo.owl.dataadapter;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import org.apache.log4j.Logger;
import org.bbop.dataadapter.AdapterConfiguration;
import org.bbop.dataadapter.CancelledAdapterException;
import org.bbop.dataadapter.DataAdapter;
import org.bbop.dataadapter.DataAdapterException;
import org.bbop.dataadapter.DataAdapterUI;
import org.bbop.dataadapter.DataAdapterUIException;
import org.bbop.dataadapter.DefaultIOOperation;
import org.bbop.dataadapter.FileAdapterConfiguration;
import org.bbop.dataadapter.FileAdapterUI;
import org.bbop.dataadapter.GraphicalUI;
import org.bbop.dataadapter.IOOperation;
import org.bbop.io.IOUtil;
import org.bbop.util.AbstractProgressValued;
import org.obo.dataadapter.DefaultOBOParser;
import org.obo.dataadapter.OBOParseEngine;
import org.obo.dataadapter.OBOParseException;
import org.obo.dataadapter.OBOSerializationEngine;
import org.obo.dataadapter.OBOSerializer;
import org.obo.dataadapter.OBO_1_2_Serializer;
import org.obo.dataadapter.OBOSerializationEngine.FilteredPath;
import org.obo.datamodel.OBOSession;
import org.obo.identifier.IDProfile;
import org.obolibrary.obo2owl.Obo2Owl;
import org.obolibrary.obo2owl.Owl2Obo;
import org.obolibrary.oboformat.model.OBODoc;
import org.obolibrary.oboformat.parser.OBOFormatParser;
import org.obolibrary.oboformat.writer.OBOFormatWriter;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

public class OWLAdapter extends AbstractProgressValued implements DataAdapter {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OWLAdapter.class);

	private boolean cancelled = false;
	private OWLAdapterConfiguration ioprofile;
	private GraphicalUI advancedUI;

	protected String userName;
	protected String autogenString;
	protected IDProfile idProfile;
	
	public static final IOOperation<OBOSession, OBOSession> WRITE_ONTOLOGY = new DefaultIOOperation<OBOSession, OBOSession>(
			"WRITE_ONTOLOGY", "write ontology", OBOSession.class,
			OBOSession.class);

	public static final IOOperation<Void, OBOSession> READ_ONTOLOGY = new DefaultIOOperation<Void, OBOSession>(
			"READ_ONTOLOGY", "read ontology", Void.class, OBOSession.class);


	public static class OWLAdapterConfiguration extends
	FileAdapterConfiguration {
		protected boolean allowDangling = false;

		protected boolean allowLossy = true;
		
		protected boolean followImports = true;

		protected boolean failFast = false;

		protected List<FilteredPath> saveRecords = new ArrayList<FilteredPath>();

		protected boolean basicSave = true;

		protected String impliedType = "Save for presentation";

		public OWLAdapterConfiguration() {
		}

		public boolean getBasicSave() {
			return basicSave;
		}

		public void setBasicSave(boolean basicSave) {
			this.basicSave = basicSave;
		}

		public List<FilteredPath> getSaveRecords() {
			return saveRecords;
		}

		public void setSaveRecords(List<FilteredPath> saveRecords) {
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

		public boolean isAllowLossy() {
			return allowLossy;
		}

		/**
		 * Not all OWL Constructs can be converted at this time. For example
		 * universal restrictions.
		 * 
		 * @param allowLossy -- if false, then untranslatable constructs will be fatal
		 */
		public void setAllowLossy(boolean allowLossy) {
			this.allowLossy = allowLossy;
		}

		public void setFailFast(boolean failFast) {
			this.failFast = failFast;
		}

		public boolean getFailFast() {
			return failFast;
		}
		
		public boolean getFollowImports() {
			return followImports;
		}

		public void setFollowImports(boolean followImports) {
			this.followImports = followImports;
		}
	}

	public String getID() {
		return "OBO:OWLAdapter";
	}

	public String getName() {
		return "OBO OWL Adapter";
	}

	public IOOperation<?,?>[] getSupportedOperations() {
		IOOperation<?,?>[] supported = { READ_ONTOLOGY, WRITE_ONTOLOGY };
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
				return new OWLAdapterConfiguration();
			}

			@Override
			public void acceptComponentConfig(boolean storeonly)
			throws DataAdapterUIException {
				super.acceptComponentConfig(storeonly);
				((OWLAdapterConfiguration) config).setBasicSave(true);
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

	public GraphicalUI getAdvancedUI() {
		return advancedUI;
	}
	public void setAdvancedUI(GraphicalUI advancedUI) {
		this.advancedUI = advancedUI;
	}

	public void cancel() {
		cancelled = true;
	}

	public AdapterConfiguration getConfiguration() {
		return ioprofile;
	}
	public void setConfiguration(OWLAdapterConfiguration config) {
		ioprofile = config;
	}

	public <INPUT_TYPE, OUTPUT_TYPE> OUTPUT_TYPE doOperation(
			IOOperation<INPUT_TYPE, OUTPUT_TYPE> op,
			AdapterConfiguration configuration, INPUT_TYPE input)
	throws DataAdapterException {
		if (!(configuration instanceof OWLAdapterConfiguration)) {
			logger.info("conf="+configuration.getClass());
			throw new DataAdapterException(" - Invalid configuration; this "
					+ "adapter requires an "
					+ "OWLAdapterConfiguration object.");
		}
		cancelled = false;
		this.ioprofile = (OWLAdapterConfiguration) configuration;

		if (op.equals(READ_ONTOLOGY)) {
			OBOSession session = convertOWLOntologies(ioprofile.getReadPaths());
			return (OUTPUT_TYPE) session;
		}
		if (op.equals(WRITE_ONTOLOGY)) {
			convertToOWL((OBOSession) input);
			return (OUTPUT_TYPE) input;
		}
		return null;
	}

	private OBOSession convertOWLOntologies(Collection<String> sources) throws DataAdapterException {
		List<String> tempOboFiles = new ArrayList<String>(sources.size());
		try {
			for (String f : sources) {
				OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
				URI uri;
				if (f.indexOf(":") > 0)  // already has scheme
					uri = URI.create(f);
				else // file path
					uri = URI.create("file:" + f);
		
				logger.info("reading OWL ontology from URI " + uri);
				try {
					// load OWL
					OWLOntology ontology = manager.loadOntologyFromOntologyDocument(IRI.create(uri));
					checkCancelled();
					
					// convert to OBO
					Owl2Obo owl2Obo = new Owl2Obo();
					owl2Obo.setStrictConversion(!ioprofile.isAllowLossy());
					OBODoc oboDoc = owl2Obo.convert(ontology);
					checkCancelled();
					
					// write to temporary file
					File tempFile = File.createTempFile("owl2obo", ".obo");
					String tempFilePath = tempFile.getAbsolutePath();
					tempOboFiles.add(tempFilePath);
					
					OBOFormatWriter writer = new OBOFormatWriter();
					writer.write(oboDoc, tempFilePath);
					checkCancelled();
				}
				catch (OWLOntologyCreationException e) {
					String message = "Error reading OWL ontology from " + uri + ": "+e.getMessage();
					logger.info(message, e);
					throw new DataAdapterException(message, e);
				} catch (IOException e) {
					String message = "Error converting OBO to OWL ontology from " + uri + ": "+e.getMessage();
					logger.info(message, e);
					throw new DataAdapterException(message, e);
				}
			}
                        OBOSession session = loadOBOSession(tempOboFiles);
                        // Set title string for OE titlebar
                        // (Sets the title string, but the actual changing of frame title happens when FrameNameUpdateTask hears a reconfigEvent)
                        session.setLoadRemark(createLoadRemark(sources));
                        // Remember most recently loaded file(s) so it can be checked by CheckOriginalFileTask to see if it changes on disk
                        session.setCurrentFilenames(sources);
			return session;
		}
		finally {
			// remove temporary files
			if (!tempOboFiles.isEmpty()) {
				for (String tempFile : tempOboFiles) {
					deleteTempFile(new File(tempFile));
				}
			}
		}
	}

	private OBOSession loadOBOSession(List<String> paths) throws DataAdapterException {
		try {
			// load content from temporary files into OBOSession
			DefaultOBOParser parser = new DefaultOBOParser();
			parser.setAllowDanglingParents(ioprofile.getAllowDangling());
			parser.setFailFast(ioprofile.getFailFast());
			parser.setFollowImports(ioprofile.getFollowImports());
			
			OBOParseEngine engine = new OBOParseEngine(parser);
			engine.setPaths(paths);
			engine.parse();
			OBOSession session = parser.getSession();
			session.setLoadRemark(createLoadRemark(paths));
			return session;
			
		} catch (IOException e) {
			throw new DataAdapterException("Could not convert OWL ontology to OBO ", e);
		} catch (OBOParseException e) {
			throw new DataAdapterException("Could not load converted OBO file", e);
		}
	}
	
	private String createLoadRemark(String path) {
            return IOUtil.getShortName(path);
        }

	private String createLoadRemark(Collection<String> paths) {
		StringBuilder out = new StringBuilder();
		Iterator<String> it = paths.iterator();
		for (int i = 0; it.hasNext(); i++) {
			String path = it.next();
			path = IOUtil.getShortName(path);

			if (i != 0)
				out.append(", ");
			out.append(path);
		}
		return out.toString();
	}

	private void convertToOWL(OBOSession session) throws DataAdapterException {
		FilteredPath filteredPath;
		if (ioprofile.getBasicSave()) {
			filteredPath = new FilteredPath(null, null, ioprofile.getWritePath());
			filteredPath.setAllowDangling(ioprofile.getAllowDangling());
		} else {
			List<FilteredPath> saveRecords = ioprofile.getSaveRecords();
			if (saveRecords.isEmpty()) {
				filteredPath = new FilteredPath(null, null, ioprofile.getWritePath());
				if (ioprofile.allowDangling) {
					filteredPath.setAllowDangling(ioprofile.getAllowDangling());
				}
			}
			else if (saveRecords.size() == 1) {
				filteredPath = saveRecords.get(0);
			}
			else {
				throw new DataAdapterException("The OWL adapter does not support multiple filterPathes.");
			}
		}
		write(session, filteredPath);
	}

	private void write(OBOSession session, FilteredPath filteredPath) throws DataAdapterException {
		File tempFile = null;
		
		try {
			// create temporary file
			tempFile = File.createTempFile("obo2owl", ".owl");
			checkCancelled();
			
			// write OBO to temporary File
			OBOSerializer serializer = new OBO_1_2_Serializer();
			OBOSerializationEngine serializeEngine = new OBOSerializationEngine();
			serializeEngine.setUsername(getUserName());
			serializeEngine.setAutogenString(getAutogenString());
			serializeEngine.setCurrentProfile(getIDProfile());
			FilteredPath tempPath = createTempFilteredPath(filteredPath, tempFile);
			serializeEngine.serialize(session, serializer, Collections.singleton(tempPath));
			checkCancelled();
			
			// load convert OBO to OWL
			OBOFormatParser parser = new OBOFormatParser();
			OBODoc oboDoc = parser.parse(tempFile);
			Obo2Owl obo2Owl = new Obo2Owl();
			OWLOntology owlOntology = obo2Owl.convert(oboDoc);
			checkCancelled();
			
			// write OWL
			OWLOntologyManager manager = owlOntology.getOWLOntologyManager();
			manager.saveOntology(owlOntology, IRI.create(new File(filteredPath.getPath())));
                        session.setCurrentFilenames(filteredPath.getPath());
                        // Set title string for OE titlebar
                        // (Sets the title string, but the actual changing of frame title happens when FrameNameUpdateTask hears a reconfigEvent)
                        session.setLoadRemark(createLoadRemark(filteredPath.getPath()));
			
		} catch (IOException e) {
			throw new DataAdapterException("Could not create temp file for obo2owl conversion.", e);
		} catch (OWLOntologyCreationException e) {
			throw new DataAdapterException("Could not convert OBO to OWL.", e);
		} catch (OWLOntologyStorageException e) {
			throw new DataAdapterException("Could not save OWL to file: "+filteredPath.getPath(), e);
		}
		finally {
			deleteTempFile(tempFile);
		}
	}
	

	private FilteredPath createTempFilteredPath(FilteredPath path, File tempFile) {
		FilteredPath newPath = new FilteredPath(path.getLinkFilter(), path.getObjectFilter(), path.getTagFilter(), tempFile.getAbsolutePath());
		newPath.setAllowDangling(path.getAllowDangling());
		newPath.setAssertImpliedLinks(path.getAllowDangling());
		newPath.setDiscardUnusedCategories(path.getDiscardUnusedCategories());
		newPath.setDoFilter(path.getDoFilter());
		newPath.setDoLinkFilter(path.getDoFilter());
		newPath.setDoTagFilter(path.getDoTagFilter());
		newPath.setIDRuleMode(path.getIDRuleMode());
		newPath.setImpliedType(path.getImpliedType());
		newPath.setIsaClosure(path.getIsaClosure());
		newPath.setPrefilterProperty(path.getPrefilterProperty());
		newPath.setReasonerFactory(path.getReasonerFactory());
		newPath.setRemark(path.getRemark());
		newPath.setRootAlgorithm(path.getRootAlgorithm());
		newPath.setSaveImplied(path.getSaveImplied());
		newPath.setSaveTypes(newPath.getSaveTypes());
		newPath.setTagsToWrite(path.getTagsToWrite());
		newPath.setUseSessionReasoner(path.getUseSessionReasoner());
		newPath.setWriteModificationData(path.getWriteModificationData());
		return newPath;
	}
	
	private void deleteTempFile(File file) {
		if (file != null) {
			try {
				file.delete();
			} catch (Exception e) {
				logger.warn("Could not delete temp file: "+file.getAbsolutePath(), e);
			}
		}
	}
	
	private void checkCancelled() throws CancelledAdapterException {
		if (cancelled)
			throw new CancelledAdapterException();
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
