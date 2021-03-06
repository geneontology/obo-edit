package org.obo.dataadapter;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.sql.DataSource;

import org.apache.log4j.Logger;
import org.bbop.dataadapter.AdapterConfiguration;
import org.bbop.dataadapter.DataAdapterException;
import org.bbop.dataadapter.DataAdapterUI;
import org.bbop.dataadapter.FileAdapterUI;
import org.bbop.dataadapter.IOOperation;
import org.bbop.dataadapter.JDBCAdapterConfiguration;
import org.bbop.io.ProgressableInputStream;
import org.bbop.rdbms.RelationalQuery;
import org.bbop.rdbms.WhereClause;
import org.bbop.rdbms.impl.SqlQueryImpl;
import org.bbop.rdbms.impl.SqlWhereClauseImpl;
import org.bbop.util.AbstractProgressValued;
import org.obo.annotation.datamodel.Annotation;
import org.obo.annotation.datamodel.AnnotationOntology;
import org.obo.annotation.datamodel.impl.AnnotationImpl;
import org.obo.dataadapter.OBOSerializationEngine.FilteredPath;
import org.obo.datamodel.CommentedObject;
import org.obo.datamodel.DanglingObject;
import org.obo.datamodel.DatatypeValue;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.DbxrefedObject;
import org.obo.datamodel.DefinedObject;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Instance;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.MultiIDObject;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.NamespacedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.ObjectFactory;
import org.obo.datamodel.ObsoletableObject;
import org.obo.datamodel.PropertyValue;
import org.obo.datamodel.SubsetObject;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.SynonymType;
import org.obo.datamodel.SynonymedObject;
import org.obo.datamodel.TermSubset;
import org.obo.datamodel.Value;
import org.obo.datamodel.ValueLink;
import org.obo.datamodel.impl.DefaultLinkDatabase;
import org.obo.datamodel.impl.DefaultObjectFactory;
import org.obo.datamodel.impl.InstanceImpl;
import org.obo.datamodel.impl.InstancePropertyValue;
import org.obo.datamodel.impl.OBOClassImpl;
import org.obo.datamodel.impl.OBOPropertyImpl;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.datamodel.impl.OBOSessionImpl;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.util.AnnotationUtil;
import org.obo.util.TermUtil;


/**
 * @author cjm
 *
 * Relational Database Adapter for any Schema that implements the OBD SQL API
 * Status of OBD API, per schema
 *   OBD: mostly implemented
 *   Chado: ontology save/store partially implemented
 *   GODB: No
 *   BioSQL: No
 *   
 * Current capabilities:
 *   Read & write objects, links and annotations
 *   Save reasoner results (TODO: "distances")
 *   
 * The OBD SQL API consists of a schema-neutral
 *  (a) a view layer
 *  and
 *  (b) SQL functions. Example store_node(...)
 *  
 * This API can be implemented on a per schema / DBMS vendor basis
 * 
 * The SQL API route was the most economical for us to implement, and
 * it has many benefits. Those with a pathological aversion to DBMS logic
 * can override methods in this adapter and implement the update logic in java
 * (this may also turn out to be more efficient)
 * 
 * See the obo-database svn repository on obo.sourceforge.net for the OBD API
 * 		obo-database/sql/api/obd-mutable-api
 * 
 * Note that this adapter is for both ontologies and annotations;
 * Schema implementations may choose to implement store_annotation()
 * in any way they choose
 * 
 * The best way of seeing how to use this is via junit tests:
 *   org.obo.test.OBDSaveTest
 *   org.obo.test.OBDAnnotationSaveTest
 *   phenote.datamodel.BasicAnnotationModelTest
 *   
 *  You can also use some of the launcher scripts:
 *    obo2database
 *    database2obo
 *    
 *  TODO: AdapterUI 
 * 
 * Currently this adapter is incomplete
 * 
 *  TODO: Query/filtering implementation
 *  Currently the default is to save everything or read everything.
 *  Implementing filters on save should be trivial.
 *  Reading would be more difficult. Ideally we want to be able to fetch
 *  subsets of objects and links according to queries of the form:
 *   - All ontologies in namespace "NS"
 *   - All annotations to "X"
 *   - All annotations assignedBy "P"
 *   - All annotations in namespace "NS"
 *   - All annotations to objects in namespace "MGI"
 *   - All annotations to any object linked to "X"
 *   - All annotations to X, where X allele_of G and G has_ancestor G2
 *   
 *   Together with boolean combinations of the above
 *   
 *   org.obo.query gives us the framework to do this (complete?)
 *   However, we would need smart query implementations that would generate the SQL
 *   
 *  For now query/filtering is very simple. In the config object, you can do
 *  a setAnnotationMode() depending on whether you want ontologies, annotations or
 *  both.
 *  You can also filter by namespaces using config.addNamespace(..)
 *  
 *   
 * At some point in the future there will be a LinkDatabase implementation;
 * this will bring various advantages:
 *  - using a RDBMS as a reasoner
 *  - using MergedLinkDatabase to wrap multiple sources
 *  
 *  Seel also:
 *  "Perhaps more importantly, the OBO-Edit editing paradigm is non-interactive.
 *   That is, we do not want immediate database writeback; we want to write the data back when the user chooses. 
 *   We already have a robust, well-tested way of tracking ontology changes and merging those changes with changes
 *   made by other users
 *   - all this code could readily and easily be reused to implement a check-in/check-out system for a database adapter."
 *  TODO: use history objects
 * 
 * 
 */
public class OBDSQLDatabaseAdapter extends AbstractProgressValued implements OBOAdapter {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBDSQLDatabaseAdapter.class);

	protected String path;
	protected AdapterConfiguration config;
	protected List streams = new LinkedList();

	protected ProgressableInputStream pfis;
	protected boolean cancelled = false;
	protected OBDSQLDatabaseAdapterConfiguration ioprofile;
	protected ObjectFactory objectFactory = new DefaultObjectFactory();

	protected Map<Connection,HashMap<Integer,IdentifiedObject>> conn2objmap =
		new HashMap <Connection,HashMap<Integer,IdentifiedObject>>();
	protected Map<Integer,IdentifiedObject> iid2obj =
		new HashMap <Integer,IdentifiedObject>();
	protected Map<IdentifiedObject,Integer> obj2iid =
		new HashMap <IdentifiedObject,Integer>();

	Collection<IdentifiedObject> savedObjects = new LinkedList<IdentifiedObject>();
	Map<Link,Integer> link2iid = new HashMap<Link,Integer>();

	LinkDatabase linkDatabase;


	protected Connection connection;


	public static class OBDSQLDatabaseAdapterConfiguration extends JDBCAdapterConfiguration {
		protected boolean allowDangling = false;

		protected boolean failFast = true;

		protected boolean saveImplied;

		// true if all links for saved terms should be replaced
		protected boolean replaceLinks = false; 

		protected java.util.List saveRecords = new ArrayList();

		protected boolean basicSave = true;

		protected Collection<Namespace> namespaces = 
			new LinkedList<Namespace>();

		public enum AnnotationMode {
			BOTH, ANNOTATIONS_ONLY, ONTOLOGY_ONLY
		}

		protected AnnotationMode annotationMode = AnnotationMode.BOTH;

		protected String serializer = "OBDSQL";

		protected String impliedType = "Save trimmed links";

		public OBDSQLDatabaseAdapterConfiguration() {
		}
		
		public OBDSQLDatabaseAdapterConfiguration(DataSource dataSource) {
		    super(dataSource);
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

		public boolean isSaveImplied() {
			return saveImplied;
		}

		public void setSaveImplied(boolean saveImplied) {
			this.saveImplied = saveImplied;
		}

		public AnnotationMode getAnnotationMode() {
			return annotationMode;
		}

		public void setAnnotationMode(AnnotationMode annotationMode) {
			this.annotationMode = annotationMode;
		}

		public Collection<Namespace> getNamespaces() {
			return namespaces;
		}

		public void setNamespaces(Collection<Namespace> namespaces) {
			this.namespaces = namespaces;
		}

		public void addNamespace(String namespace) {
			namespaces.add(new Namespace(namespace));
		}
	}

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



	public <INPUT_TYPE, OUTPUT_TYPE> OUTPUT_TYPE doOperation(
			IOOperation<INPUT_TYPE, OUTPUT_TYPE> op,
			AdapterConfiguration configuration, INPUT_TYPE input)
	//	public Object doOperation(IOOperation op, AdapterConfiguration configuration,
	//			Object o)
	throws DataAdapterException {
		if (!(configuration instanceof OBDSQLDatabaseAdapterConfiguration)) {
			throw new DataAdapterException("Invalid configuration; this "
					+ "adapter requires an "
					+ "OBOAdapterConfiguration object.");
		}
		cancelled = false;
		this.ioprofile = (OBDSQLDatabaseAdapterConfiguration) configuration;
		if (op.equals(READ_ONTOLOGY)) {
			OBOSession session = objectFactory.createSession();
			session.setDefaultNamespace(objectFactory.createNamespace("test", "test")); // TODO

			try {
				//connect(ioprofile.getReadPath());
				connect();
				fetchAll(session);
				return (OUTPUT_TYPE) session;
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				throw new DataAdapterException("SQL error");
			}


		} else if (op.equals(WRITE_ONTOLOGY)) {
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

			OBOSession session = (OBOSession) input;
			while (it.hasNext()) {
				FilteredPath filteredPath = it.next();
				LinkDatabase ldb = session.getLinkDatabase();
				if (filteredPath.saveImplied) {

					if (reasoner == null) {
						reasoner = new ForwardChainingReasoner();
						reasoner.setLinkDatabase(new DefaultLinkDatabase(session));
						reasoner.recache();
					}
					ldb = reasoner;
					logger.info("will save implied");
				}
				logger.info("ldb="+ldb);

				try {
					logger.info("fp="+filteredPath);
					ioprofile.setReadPath(filteredPath.getPath());
					this.connect();
					logger.info("conn="+connection);

					storeAll(session,ldb);
					return (OUTPUT_TYPE) input;
				}  catch (Exception ex) {
					logger.info(ex);
					ex.printStackTrace();

					throw new DataAdapterException("Bad configuration", ex);
					// write((OBOSession) o);
				}
			}
		}
		return null;
	}

	public String getID() {
		return "OBO:OBDSQLDatabaseAdapter";
	}



	/*
	 * This function was comment out because the connection parameters for the database connection
	 * should be entirely specified in the JDBCconfiguration (this.ioprofile) part of the OBDSQLDatabaseAdaptor object.
	 * 
	public void connect(String readPath) throws SQLException, ClassNotFoundException {
		connection = ioprofile.getConnection(readPath,"cjm","");
		logger.info("connecting "+readPath+" "+connection);
	}
	 */

	public void disconnect() throws SQLException {
		connection.close();
	}



	public void connect() throws SQLException, ClassNotFoundException {
		// TODO
		this.connection = this.ioprofile.getConnection();
	}


	public String getName() {
		return "OBD-compliant SQL Database adapter";
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

	public void fetchAll(OBOSession session) throws SQLException {

		// TODO : add filters
		// for now we just slurp everything
		Statement stmt = connection.createStatement();
		RelationalQuery nodeQuery = new SqlQueryImpl();
		nodeQuery.addTable("node_with_source");
		WhereClause whereClause = nodeQuery.getWhereClause();
		if (ioprofile.getNamespaces().size() > 0) {
			// TODO
			// use obo.query
			whereClause.addInConstraint("source_uid", ioprofile.getNamespaces());
		}
		ResultSet rs = stmt.executeQuery(nodeQuery.toSQL());
		LinkedList<IdentifiedObject> ios = new LinkedList<IdentifiedObject>();
		while (rs.next()) {
			IdentifiedObject io = fetchObject(session, rs);
			session.addObject(io);
			ios.add(io);
		}

		// get links after we have fetched all objects
		for (IdentifiedObject io : ios) {
			fetchObjectInfo(session, io);
			fetchObjectLinks(session, io);
		}

	}

	public Collection<Annotation> fetchAnnotationsByObject(OBOSession session, LinkedObject obj) throws SQLException {

		RelationalQuery q = new SqlQueryImpl();
		q.addTable("node_link_node_with_pred_and_source");
		WhereClause whereClause = q.getWhereClause();
		//whereClause.addEqualityConstraint("is_inferred", false);
		//whereClause.addEqualityConstraint("object_uid",obj.getID());
		whereClause.addConstraint("reiflink_node_id IS NOT NULL");

		// we use a subquery, as transitive closure may not incl annot statements
		// example: G has_role X, X part_of Y
		whereClause.addConstraint("object_id IN (SELECT DISTINCT node_id FROM link_to_node WHERE object_uid ='"+
				obj.getID()+"')"); 		// TODO: use SQL constructors


		ResultSet rs = q.execute(connection);

		OBOSession tempSession = new OBOSessionImpl();
		while (rs.next()) {
			includeLinkResultSetInSession(tempSession, rs);
		}
		return AnnotationUtil.getAnnotations(tempSession);
	}

	public Collection<Link> fetchLinksByObject(String id, String ns) throws SQLException {

		WhereClause whereClause = new SqlWhereClauseImpl();
		if (ns != null) {
			whereClause.addEqualityConstraint("source_uid",ns);
		}
		// TODO
		whereClause.addEqualityConstraint("object_uid", id);
		return fetchLinks(whereClause);
	}

	public Collection<Link> fetchLinks(WhereClause whereClause) throws SQLException {

		RelationalQuery q = new SqlQueryImpl();
		q.addTable("node_link_node_with_pred_and_source");
		q.setWhereClause(whereClause);
		q.getSelectClause().setDistinct(true);
		ResultSet rs = q.execute(connection);

		OBOSession tempSession = new OBOSessionImpl();
		while (rs.next()) {
			includeLinkResultSetInSession(tempSession, rs);
		}
		HashSet<Link> links = new HashSet<Link>();
		for (IdentifiedObject io : tempSession.getObjects()) {
			if (io instanceof LinkedObject && !io.isBuiltIn())
				links.addAll(((LinkedObject)io).getParents());
		}
		return links;
	}



	public Collection<Link> fetchLinksByObject(String id) throws SQLException {

		return fetchLinksByObject(id,null);
	}





	// TODO: DRY
	public int fetchAnnotationCountByObject(OBOSession session, LinkedObject obj) throws SQLException {

		RelationalQuery q = new SqlQueryImpl();
		/*
		q.addTable("link");
		WhereClause whereClause = q.getWhereClause();
		whereClause.addConstraint("reiflink_node_id IS NOT NULL");
		whereClause.addConstraint("object_id IN (SELECT node_id FROM link_to_node WHERE object_uid ='"+
				obj.getID()+"')");
		q.getSelectClause().addColumn("COUNT(DISTINCT reiflink_node_id) AS c");
		 */
		// TODO: use cached node_id if available?
		q.addTable("annotation_count_by_class INNER JOIN node USING (node_id)");
		WhereClause whereClause = q.getWhereClause();
		whereClause.addEqualityConstraint("uid", obj.getID());
		q.getSelectClause().addColumn("annotation_count");

		ResultSet rs = q.execute(connection);

		if (rs.next())
			return rs.getInt("annotation_count");
		else
			return 0;
	}

	public float fetchAnnotationInformationContentByObject(OBOSession session, LinkedObject obj) throws SQLException {

		RelationalQuery q = new SqlQueryImpl();
		q.addTable("class_node_entropy_by_evidence INNER JOIN node USING (node_id)");
		WhereClause whereClause = q.getWhereClause();
		whereClause.addEqualityConstraint("uid", obj.getID());

		ResultSet rs = q.execute(connection);
		if (rs.next())
			return rs.getFloat("shannon_information");
		else
			return 0;
	}



	// fetches into temporary session
	public Collection<Annotation> retrieveAllAnnotations(OBOSession session) throws SQLException, ClassNotFoundException {

		connect();
		RelationalQuery q = new SqlQueryImpl();
		q.addTable("obd_prejoins_view.node_link_node_with_pred_and_source");
		WhereClause whereClause = q.getWhereClause();
		//whereClause.addEqualityConstraint("is_inferred", false);
		whereClause. addConstraint("reiflink_node_id IS NOT NULL");

		PreparedStatement stmt = connection.prepareStatement(q.toSQL());

		ResultSet rs = stmt.executeQuery();
		OBOSession tempSession = new OBOSessionImpl();
		while (rs.next()) {
			includeLinkResultSetInSession(tempSession, rs);
		}
		return AnnotationUtil.getAnnotations(tempSession);
	}



	public IdentifiedObject fetchObjectByInternalID(OBOSession session, int iid) throws SQLException {

		RelationalQuery nodeQuery = new SqlQueryImpl();
		nodeQuery.addTable("node_with_source");
		WhereClause whereClause = nodeQuery.getWhereClause();
		whereClause.addEqualityConstraint("node_id", iid);

		ResultSet rs = nodeQuery.execute(connection);
		if (rs.next())
			return fetchObject(session, rs);
		return null;
	}

	public IdentifiedObject fetchObject(OBOSession session, ResultSet rs) throws SQLException {
		IdentifiedObject io;
		String metatype = rs.getString("metatype");
		if (metatype == null)
			metatype="";
		Integer iid = rs.getInt("node_id");
		String id = rs.getString("uid");
		if (metatype.equals("C")) {
			io = new OBOClassImpl(id);
		}
		else if (metatype.equals("R")) {
			io = new OBOPropertyImpl(id);
		}
		else {
			io = new InstanceImpl(id);
		}
		io.setName(rs.getString("label"));
		String nsId = rs.getString("source_uid");
		if (rs.wasNull()) {
			// TODO
		}
		else {
			Namespace ns = session.getNamespace(nsId);
			if (ns == null) {
				ns = objectFactory.createNamespace(nsId, nsId);
				//logger.info(io+" adding ns="+ns+" / "+nsId);
				session.addNamespace(ns);
			}
			//logger.info(io+" setting ns="+ns);
			io.setNamespace(ns);
		}
		//conn2objmap.get(conn).put(iid, io);
		iid2obj.put(iid,io);
		obj2iid.put(io,iid);
		//fetchObjectInfo(session, io);
		return io;
	}

	public void fetchObjectInfo(OBOSession session, IdentifiedObject obj) throws SQLException {

		fetchObjectDescription(session, obj);
		fetchObjectAliases(session, obj);
		//fetchObjectLinks(session, obj);
	}
	public void fetchObjectDescription(OBOSession session, IdentifiedObject obj) throws SQLException {

		RelationalQuery descriptionQuery = new SqlQueryImpl();
		descriptionQuery.addTable("description_d");
		WhereClause whereClause = descriptionQuery.getWhereClause();
		whereClause.addEqualityConstraint("node_uid",obj.getID());


		PreparedStatement stmt = connection.prepareStatement(descriptionQuery.toSQL());
		int i=1;
		for (Object v : descriptionQuery.getPlaceHolderVals()) {
			stmt.setString(i, (String)v);
			i++;
		}

		ResultSet rs = stmt.executeQuery();
		while (rs.next()) {
			attachDescription(session, rs);
		}

	}

	public void fetchObjectAliases(OBOSession session, IdentifiedObject obj) throws SQLException {

		RelationalQuery aliasQuery = new SqlQueryImpl();
		aliasQuery.addTable("alias_d");
		WhereClause whereClause = aliasQuery.getWhereClause();
		whereClause.addEqualityConstraint("node_uid",obj.getID());


		PreparedStatement stmt = connection.prepareStatement(aliasQuery.toSQL());
		int i=1;
		for (Object v : aliasQuery.getPlaceHolderVals()) {
			stmt.setString(i, (String)v);
			i++;
		}

		ResultSet rs = stmt.executeQuery();
		while (rs.next()) {
			attachAlias(session, rs);
		}

	}

	public void fetchObjectLinks(OBOSession session, IdentifiedObject obj) throws SQLException {

		RelationalQuery q = new SqlQueryImpl();
		q.addTable("node_link_node_with_pred_and_source");
		WhereClause whereClause = q.getWhereClause();
		whereClause.addEqualityConstraint("is_inferred", false);
		whereClause.addEqualityConstraint("node_uid",obj.getID());

		PreparedStatement stmt = connection.prepareStatement(q.toSQL());
		int i=1;
		for (Object v : q.getPlaceHolderVals()) {
			if (v instanceof String)
				stmt.setString(i, (String)v);
			else if (v instanceof Boolean)
				stmt.setBoolean(i, (Boolean)v);
			else
				throw new SQLException("dunno what to do with "+v);
			// TODO
			i++;
		}

		ResultSet rs = stmt.executeQuery();
		while (rs.next()) {
			includeLinkResultSetInSession(session, rs);
		}

	}

	public void includeLinkResultSetInSession(OBOSession session, ResultSet rs) throws SQLException {
		includeLinkResultSetInSession(session, rs, new HashMap<String,Link>());
	}

	public void includeLinkResultSetInSession(OBOSession session, ResultSet rs, HashMap<String,Link> positsMap) throws SQLException {
		Link link;
		LinkedObject node = (LinkedObject) lookupObject(session, rs.getString("node_uid"));
		OBOProperty pred = lookupProperty(session, rs.getString("pred_uid"));
		LinkedObject obj = (LinkedObject) lookupObject(session, rs.getString("object_uid"));
		Namespace source =  lookupNamespace(session, rs.getString("source_uid"));
		boolean isImplied = rs.getBoolean("is_inferred");

		String metatype = rs.getString("node_metatype"); 
		if (metatype != null && metatype.equals("C")) {
			link = new OBORestrictionImpl(node,pred,obj,isImplied);
			String combinator = rs.getString("combinator");
			if (combinator.equals("I"))
				((OBORestriction)link).setCompletes(true);
		}
		else {
			link = new InstancePropertyValue(node,pred,obj);
		}
		Integer annotId = rs.getInt("reiflink_node_id");
		if (!rs.wasNull()) {
			IdentifiedObject o = iid2obj.get(annotId);
			if (o == null) {
				o = fetchObjectByInternalID(session, annotId);
				//o = session.getObjectFactory().createObject(annotId.toString(), OBOClass.OBO_INSTANCE, false);
				iid2obj.put(annotId,o);
			}

			Instance inst = TermUtil.castToInstance((LinkedObject)o);
			//logger.info("this is a reified link; annotId="+annotId+" inst="+inst+" link="+link);
			Annotation annot = new AnnotationImpl(inst, link);
			positsMap.put(inst.getID(), link);
			session.addObject(annot);
		}
		//logger.info("included link: "+link);

		link.setNamespace(source);

		includeLinkInSession(session,link);
	}


	// transforms from generic links to OBO metadata model
	public void includeLinkInSession(OBOSession session, Link link) {
		if (link.getType() == null)
			return;
		String pid = link.getType().getID();
		LinkedObject lo = link.getChild();
		LinkedObject p = link.getParent();


		// TODO: use metadata ontology
		if (pid.equals("oboInOwl:inSubset")) {
			//logger.info("subset "+link+"//"+p);
			TermUtil.castToClass(lo);
			TermSubset category = session.getCategory(p.getID());
			if (category == null) {
				category = objectFactory.createSubset(p.getID(), "");
				session.addSubset(category);
			}
			TermUtil.castToClass(lo).addCategory(category);
		}
		else if (pid.equals("oboInOwl:hasDbXref")) {
			OBOClass oboClass = 
				TermUtil.castToClass(lo);
			if (oboClass == null) {
				logger.info("cannot cast "+lo);
			}
			else 
				oboClass.addDbxref(getDbxref(p.getID()));
		}
		else if (pid.equals("OBO_REL:instance_of")) {

			if (p.getID().equals("oboInOwl:Subset")) {
				TermSubset cat = objectFactory.createSubset(lo.getID(), lo.getName());
				session.addSubset(cat);
				// TODO: remove this at the end; still required as object of subset links
				// session.removeObject(lo);
			}
			else if (p.getID().equals("oban:Annotation")) {
				//Annotation annot = new AnnotationImpl((Instance)lo);
				//session.addObject(annot);
			}
			else {
				TermUtil.castToInstance(lo).setType(TermUtil.castToClass(p));
			}
		}
		else {
			lo.addParent(link);
		}
	}

	protected Dbxref getDbxref(String dbx) {
		int index = dbx.indexOf(':');
		String id;
		String db;
		if (index < 0) {
			db = "";
			id = dbx;
		} else {
			db = dbx.substring(0, index);
			id = dbx.substring(index + 1, dbx.length());
		}
		Dbxref ref = objectFactory.createDbxref(db, id, "", Dbxref.RELATED_SYNONYM, null);
		return ref;
	}

	public void storeAll(OBOSession session, LinkDatabase ldb) throws DataAdapterException {
		try {
			setProgressString("Saving to db...");

			linkDatabase = ldb;

			savedObjects = new LinkedList<IdentifiedObject>();

			for (TermSubset cat : session.getSubsets()) {
				saveCategory(cat);
			}
			for (IdentifiedObject io : session.getObjects()) {
				if (io.isBuiltIn() && !io.equals(OBOProperty.IS_A)) {

				}
				else {
					// do not save any annotations if annotation mode is on 
					if (!(io instanceof Annotation) &&
							ioprofile.getAnnotationMode().equals(
									OBDSQLDatabaseAdapterConfiguration.AnnotationMode.ANNOTATIONS_ONLY))
						continue;
					//System.out.println((io instanceof Annotation ? "A" : "-") + " " +io);
					saveObject(io);

				}
			}

			// save links for all objects that have been saved
			for (IdentifiedObject io : savedObjects) {
				if (io instanceof LinkedObject) {
					if (io instanceof Annotation) {
						//logger.info("saving annot links:"+io);
					}
					for (Link link : ldb.getParents((LinkedObject) io)) {
						if (link.getType().equals(AnnotationOntology.POSITS_REL())) {
							continue;
						}
						saveLink(link);
					}
				}
			}


			/*
			if (!ioprofile.getAnnotationMode().equals(
				OBDSQLDatabaseAdapterConfiguration.AnnotationMode.ANNOTATIONS_ONLY)) {
				for (IdentifiedObject io : ldb.getObjects()) {
					if (io instanceof LinkedObject) {
						for (Link link : ldb.getParents((LinkedObject) io)) {
							saveLink(link);
						}
					}
				}
			}
			 */

		} catch (Exception e) {
			logger.info(e);
			throw new DataAdapterException(e, "Write error");
		}
	}

	protected int saveObject(IdentifiedObject lo) throws SQLException {
		//logger.info("saving "+lo);
		int iid;
		String ns = "";
		if (ioprofile.replaceLinks) {
			// TODO
			callSqlFunc("remove_links_for_node", lo.getID());
		}
		if (lo instanceof NamespacedObject && lo.getNamespace() != null) {
			ns = lo.getNamespace().getID();
		}
		if (lo instanceof Annotation) {
			Annotation annot = (Annotation)lo;
			LinkedObject obj = annot.getObject();
			if (obj == null) {
				// TODO
				logger.info("no object for: "+annot);
				return 0;
			}
			if (obj.isAnonymous()) {
				saveObject(obj);
				for (Link link : obj.getParents())
					saveLink(link);
			}

			iid =
				callSqlFunc("store_annotation",
						annot.getSubject(),
						annot.getRelationship(),
						annot.getObject(),
						annot.getNamespace() == null ? "_" : annot.getNamespace().getID());
			link2iid.put(annot.getPositedLink(), iid);

			//			for (Link link : annot.getParents())
			//			saveLink(link);
		}
		else if ((lo instanceof DanglingObject || lo.getName() == null) && 
				lo instanceof OBOClass) {
			iid =
				callSqlFunc("store_class_node",
						lo.getID());


		}
		else if (lo instanceof OBOClass) {
			iid =
				callSqlFunc("store_node",
						lo.getID(),
						lo.getName(),
						ns,
				"C");

		}
		else if (lo instanceof Instance) {
			if (lo.getType() == null)
				iid =
					callSqlFunc("store_instance_node",
							lo.getID(),
							lo.getName(),
							ns);
			else
				iid =
					callSqlFunc("store_instance_node",
							lo.getID(),
							lo.getName(),
							ns,
							lo.getType().getID());

		}
		else if (lo instanceof OBOProperty) {
			iid =
				callSqlFunc("store_node",
						lo.getID(),
						lo.getName(),
						ns,
				"R");
			OBOProperty prop = (OBOProperty)lo;
			if (prop.isTransitive())
				callSqlFunc("set_node_is_transitive_i",iid,true);
			if (prop.isSymmetric())
				callSqlFunc("set_node_is_symmetric_i",iid,true);
			if (prop.isNonInheritable())
				callSqlFunc("set_node_is_metadata_i",iid,true);
		}
		else {
			iid =
				callSqlFunc("store_node",
						lo.getID());

		}

		// node is saved in database. Now for additional metadata:

		if (lo instanceof ObsoletableObject) {
			ObsoletableObject olo = (ObsoletableObject)lo;
			callSqlFunc("set_node_is_obsolete_i",iid,olo.isObsolete());
			for (IdentifiedObject x : olo.getConsiderReplacements())
				callSqlFunc("store_link_si",iid,"oboInOwl:consider",x.getID(),"",false);
			for (IdentifiedObject x : olo.getReplacedBy())
				callSqlFunc("store_link_si",iid,"oboInOwl:replacedBy",x.getID(),"",false);
		}
		if (lo instanceof DbxrefedObject) {
			for (Dbxref x : ((DbxrefedObject) lo).getDbxrefs()) {
				callSqlFunc("store_node_dbxref_i",iid,x.toString());
			}
		}
		if (lo instanceof SubsetObject) {
			for (TermSubset x : ((SubsetObject) lo).getSubsets()) {
				callSqlFunc("store_node_subset_link_i",iid,x.getName());
			}
		}
		if (lo instanceof MultiIDObject) {
			for (String x : ((MultiIDObject) lo).getSecondaryIDs()) {
				callSqlFunc("store_node_alt_id_i",iid,x);
			}
		}
		if (lo instanceof SynonymedObject) {
			for (Synonym x : ((SynonymedObject) lo).getSynonyms()) {
				callSqlFunc("store_node_synonym_i",
						iid,TermUtil.getScopeLabel(x.getScope()),x.getSynonymType(),x.getText());
			}
		}
		if (lo instanceof DefinedObject) {
			String def = ((DefinedObject) lo).getDefinition();
			if (def != null && def != "") {
				callSqlFunc("store_textdef_i",
						iid,def);

			}
		}
		if (lo instanceof CommentedObject) {
			String cmt = ((CommentedObject) lo).getComment();
			if (cmt != null && cmt != "") {
				callSqlFunc("store_comment_i",
						iid,
						cmt);
			}
		}
		savedObjects.add(lo); // redundant with obj2iid?
		obj2iid.put(lo, iid);

		/* Property values for RANK are handled here - Cartik [070209] */
		if(lo.getID().startsWith("TTO")){
			for(PropertyValue pv : lo.getPropertyValues()){
				 if(pv.getValue().contains("has_rank")){
					 String propValue = pv.getValue();
					 if(propValue != null && propValue != ""){
						 String[] propValueComponents = propValue.split("\\s");
						 if(propValueComponents.length == 2){
							 callSqlFunc("store_link",
									 lo.getID(),
									 propValueComponents[0],
									 propValueComponents[1], 
									 "");
						 }
					 }
				 }
			}
		}
		
		/* Property values for EXTINCT species are handled here - Cartik [070709] */
		if(lo.getID().startsWith("TTO")){
			for(PropertyValue pv : lo.getPropertyValues()){
				 if(pv.getValue().contains("is_extinct")){
					 callSqlFunc("store_tagval",
							 lo.getID(), 
							 "is_extinct",
							 "true", 
							 "",
							 lo.getNamespace().toString());
				 }
			}
		}
		
		if (false) {
			//			for (Link link : linkDatabase.getParents((LinkedObject) lo)) {
			for (Link link : ((LinkedObject) lo).getParents()) {
				if (link.getType().equals(AnnotationOntology.POSITS_REL()))
					continue;
				saveLink(link);
			}
		}

		return iid;
	}

	protected int saveCategory(TermSubset cat) throws SQLException {
		return
		callSqlFunc("store_subset",
				cat.getName(),
				cat.getDesc());

	}


	protected int saveLink(Link link) throws SQLException {

		// never save a link twice
		if (link2iid.containsKey(link))
			return link2iid.get(link);
		LinkedObject child = link.getChild();
		// the OBO parse does not generally provide links with a namespace
		// we can use the namespace of the child object provide an OBD link with a namespace
		if (link.getNamespace() == null) {
		    link.setNamespace(child.getNamespace());
		}
		//logger.info("child="+child);
		Integer childDbId = obj2iid.get(child);
		//logger.info("saving "+link);
		int iid;
		if (link instanceof ValueLink) {
			ValueLink pv = (ValueLink)link;
			Value v = pv.getValue();
			if (v instanceof DatatypeValue) {
				DatatypeValue dv = (DatatypeValue) v;
				//logger.info("dv type="+dv.getType());
				iid = callSqlFunc("store_tagval",
						link.getChild().getID(),
						link.getType().getID(),
						dv.getValue(),
						dv.getType(),
						link.getNamespace());
				//					((DatatypeValue) v).getType());

			}
			else if (v instanceof IdentifiedObject) {
				// 
				iid =
					callSqlFunc("store_link_si",
							childDbId,
							link.getType().getID(),
							link.getParent().getID(),
							(TermUtil.isIntersection(link) ? "I" : ""),
							TermUtil.isImplied(link),
							link.getNamespace());
			}
			else {
				iid = 0;
			}
		}
		else {
			iid = callSqlFunc("store_link_si",
					//link.getChild().getID(),
					childDbId,
					link.getType().getID(),
					link.getParent().getID(),
					(TermUtil.isIntersection(link) ? "I" : ""),
					TermUtil.isImplied(link),
					link.getNamespace());
		}
		link2iid.put(link, iid);
		return iid;

	}

	public IdentifiedObject attachDescription(OBOSession session, ResultSet rs) throws SQLException {
		IdentifiedObject io;
		String id = rs.getString("node_uid");
		io = lookupObject(session, id);
		String type = rs.getString("type_uid");
		if (!rs.wasNull()) {
			String label = rs.getString("label");
			if (type.equals("oboInOwl:hasDefinition")) {
				//logger.info(io+" def: "+label);
				if (io instanceof DefinedObject)
					((DefinedObject)io).setDefinition(label);
			}
			else {
				// TODO
			}
		}
		else {
			// TODO
		}
		return io;
	}

	public IdentifiedObject attachAlias(OBOSession session, ResultSet rs) throws SQLException {
		IdentifiedObject io;
		String id = rs.getString("node_uid");
		io = lookupObject(session, id);
		if (io instanceof SynonymedObject) {
			String scope = rs.getString("scope");
			String category = rs.getString("type_uid");
			String label = rs.getString("label");
			int scopeEnum = TermUtil.getScopeEnum(scope);
			Synonym syn = objectFactory.createSynonym(label, scopeEnum);
			if (category != null) {
				SynonymType categoryObj = objectFactory.createSynonymType(category, "", 0);
				syn.setSynonymType(categoryObj);
			}

			((SynonymedObject)io).addSynonym(syn);
		}
		return io;
	}



	public IdentifiedObject lookupObject(OBOSession session, String id) {
		IdentifiedObject io = session.getObject(id);
		if (io == null) {
			io = objectFactory.createDanglingObject(id, false);
			session.addObject(io);
			//logger.info("creating dangling for "+id);
		}
		return io;
	}

	public OBOProperty lookupProperty(OBOSession session, String id) {
		IdentifiedObject io = session.getObject(id);
		if (io == null) {
			io = objectFactory.createDanglingObject(id, true);
		}
		return TermUtil.castToProperty(io);
	}

	public Namespace lookupNamespace(OBOSession session, String id) {
		Namespace io = session.getNamespace(id);
		return io;
	}


	// TODO: move to generic place
	protected int callSqlFunc(String func, Object... args) throws SQLException {
		StringBuffer sql = new StringBuffer();
		sql.append("{?= call "+func + "(");
		boolean isFirst = true;
		for (Object arg : args) {
			if (isFirst)
				isFirst = false;
			else 
				sql.append(",");
			sql.append("?");
		}
		sql.append(")}");
		logger.debug("sql="+sql);
		for (Object ob : args) {
			logger.debug("  arg:"+ob);
		}
		CallableStatement stmt = connection.prepareCall(sql.toString());
		stmt.registerOutParameter(1, Types.INTEGER);
		// logger.info("stmt="+stmt);
		for (int i=0; i<args.length; i++) {
			Object arg = args[i];
			if (arg instanceof Integer) {
				stmt.setInt(i+2, (Integer)arg);
			}
			else if (arg instanceof Boolean) {
				stmt.setBoolean(i+2, (Boolean)arg);
			}
			else if (arg instanceof IdentifiedObject) {
				stmt.setString(i+2, ((IdentifiedObject) arg).getID());
			}
			else if (arg instanceof SynonymType) {
				stmt.setString(i+2, ((SynonymType) arg).getID());
			} else if (arg instanceof Namespace) {
			    stmt.setString(i+2, ((Namespace)arg).getID());
			}
			else {
				stmt.setString(i+2, arg == null ? (String)arg : arg.toString());
			}
		}
		//logger.info("stmt="+stmt+" :: "+args);

		boolean rs = stmt.execute();	

		//logger.debug("rs="+rs);
		return stmt.getInt(1);
	}

	// TODO: move
	protected String join(String sep, Collection<String> tokens) {
		StringBuffer s = new StringBuffer();
		for (String t : tokens) {
			if (s.length() == 0) {
				s.append(t);
			}
			else {
				s.append(sep);
				s.append(t);
			}
		}
		return s.toString();
	}

	public AdapterConfiguration getConfiguration() {
		return ioprofile;
	}
	public void setConfiguration(OBDSQLDatabaseAdapterConfiguration conf) {
		ioprofile = conf;
	}

	public Connection getConnection() {
		return connection;
	}

	public void setConnection(Connection connection) {
		this.connection = connection;
	}


}
