package org.obo.dataadapter;

import java.io.*;
import java.util.*;
import java.sql.*;

import org.bbop.dataadapter.*;
import org.bbop.io.ProgressableInputStream;
import org.bbop.io.SafeFileOutputStream;
import org.bbop.rdbms.FromClause;
import org.bbop.rdbms.RelationalQuery;
import org.bbop.rdbms.WhereClause;
import org.bbop.rdbms.impl.SqlQueryImpl;
import org.bbop.util.AbstractProgressValued;
import org.bbop.util.StringUtil;
import org.obo.annotation.datamodel.Annotation;
import org.obo.annotation.datamodel.AnnotationOntology;
import org.obo.annotation.datamodel.impl.AnnotationImpl;
import org.obo.dataadapter.OBOParser.XrefPair;
import org.obo.dataadapter.OBOSerializationEngine.FilteredPath;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.DanglingObjectImpl;
import org.obo.datamodel.impl.DefaultLinkDatabase;
import org.obo.datamodel.impl.DefaultObjectFactory;
import org.obo.datamodel.impl.InstanceImpl;
import org.obo.datamodel.impl.InstancePropertyValue;
import org.obo.datamodel.impl.OBOClassImpl;
import org.obo.datamodel.impl.OBOPropertyImpl;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.datamodel.impl.PropertyValueImpl;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.impl.ForwardChainingReasoner;
import org.obo.util.TermUtil;


/**
 * @author cjm
 *
 * Relational Database Adapter for any Schema that implements the OBD SQL API
 * Status of OBD API, per schema
 *   OBD: partially implemented
 *   Chado: No
 *   GODB: No
 *   BioSQL: No
 *   
 * The OBD SQL API consists of a schema-neutral
 *  (a) a view layer; and
 *  (b) SQL functions, such as store_node(...)
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
 * Currently this adapter is incomplete
 * 
 *   Reads: No
 *   Writes: Partial
 *   Filters on read: unimplemented
 *   Filters on write: unimplemented
 *   Lazy read: unimplemented
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
 */
public class OBDSQLDatabaseAdapter extends AbstractProgressValued implements OBOAdapter {

	protected String path;
	protected AdapterConfiguration config;
	protected ProgressableInputStream pfis;
	protected boolean cancelled = false;
	protected OBDSQLDatabaseAdapterConfiguration ioprofile;
	protected List streams = new LinkedList();
	protected ObjectFactory objectFactory = new DefaultObjectFactory();
	
	protected HashMap<Connection,HashMap<Integer,IdentifiedObject>> conn2objmap =
		new HashMap <Connection,HashMap<Integer,IdentifiedObject>>();
	protected HashMap<Integer,IdentifiedObject> iid2obj =
		new HashMap <Integer,IdentifiedObject>();
	protected HashMap<IdentifiedObject,Integer> obj2iid =
		new HashMap <IdentifiedObject,Integer>();
	
	protected Connection conn;

	public static class OBDSQLDatabaseAdapterConfiguration extends
	JDBCAdapterConfiguration {
		protected boolean allowDangling = false;

		protected boolean failFast = true;

		protected boolean saveImplied;
		
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

		protected String impliedType = "Save for presentation";

		public OBDSQLDatabaseAdapterConfiguration() {
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
			session.setDefaultNamespace(objectFactory.createNamespace("test", "test"));
			for (String readPath : ioprofile.getReadPaths()) {
				try {
					conn = ioprofile.getConnection(readPath,"cjm","");
					fetchAll(session);
					return (OUTPUT_TYPE) session;
				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
					throw new DataAdapterException("SQL error");
				}
			}
			return (OUTPUT_TYPE) session;
			
		} else if (op.equals(WRITE_ONTOLOGY)) {
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
					System.err.println("will save implied");
				}
				System.err.println("ldb="+ldb);

				try {
					System.err.println("fp="+filteredPath);
					conn = ioprofile.getConnection(filteredPath.getPath(),"cjm","");
					System.out.println("conn="+conn);
					
					storeAll(session,ldb);
					return (OUTPUT_TYPE) input;
				}  catch (Exception ex) {
					System.err.println(ex);
					
					throw new DataAdapterException("Bad configuration");
				// write((OBOSession) o);
				}
			}
		}
		return null;
	}

	public String getID() {
		return "OBO:OBDSQLDatabaseAdapter";
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
		Statement stmt = conn.createStatement();
		RelationalQuery nodeQuery = new SqlQueryImpl();
		nodeQuery.addTable("node_with_source");
		WhereClause whereClause = nodeQuery.getWhereClause();
		if (ioprofile.getNamespaces().size() > 0) {
			// TODO
			// use obo.query
			whereClause.addInConstraint("source_uid", ioprofile.getNamespaces());
		}
		System.out.println(nodeQuery.toSQL());
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
	


	public IdentifiedObject fetchObject(OBOSession session, ResultSet rs) throws SQLException {
		IdentifiedObject io;
		String metatype = rs.getString("metatype");
		if (metatype == null)
			metatype="";
		Integer iid = rs.getInt("node_id");
		String id = rs.getString("uid");
		System.err.println(id);
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
				System.err.println(io+" adding ns="+ns+" / "+nsId);
				session.addNamespace(ns);
			}
			System.err.println(io+" setting ns="+ns);
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
		

		PreparedStatement stmt = conn.prepareStatement(descriptionQuery.toSQL());
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
		

		PreparedStatement stmt = conn.prepareStatement(aliasQuery.toSQL());
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
		
		PreparedStatement stmt = conn.prepareStatement(q.toSQL());
		System.out.println(q.toSQL());
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
			fetchLink(session, rs);
		}

	}
	
	public void fetchLink(OBOSession session, ResultSet rs) throws SQLException {
		Link link;
		LinkedObject node = (LinkedObject) lookupObject(session, rs.getString("node_uid"));
		OBOProperty pred = lookupProperty(session, rs.getString("pred_uid"));
		LinkedObject obj = (LinkedObject) lookupObject(session, rs.getString("object_uid"));
		Namespace source =  lookupNamespace(session, rs.getString("source_uid"));
		String metatype = rs.getString("node_metatype"); 
		if (metatype != null && metatype.equals("C")) {
			link = new OBORestrictionImpl(node,pred,obj);
		}
		else {
			link = new InstancePropertyValue(node,pred,obj);
		}
		Integer annotId = rs.getInt("reiflink_node_id");
		if (!rs.wasNull()) {
			Instance inst = TermUtil.castToInstance((LinkedObject)iid2obj.get(annotId));
			System.err.println("this is a reified link; annotId="+annotId+" "+inst);
			Annotation annot = new AnnotationImpl(inst, link);
			session.addObject(annot);
		}
		System.err.println("read link: "+link);
		link.setNamespace(source);
		addLink(session,link);
	}
	

	// transforms from generic links to OBO metadata model
	public void addLink(OBOSession session, Link link) {
		String pid = link.getType().getID();
		LinkedObject lo = link.getChild();
		LinkedObject p = link.getParent();
		
		if (pid.equals("oboMetaModel:inSubset")) {
			System.err.println("subset "+link+"//"+p);
			TermUtil.castToClass(lo);
			TermCategory category = session.getCategory(p.getID());
			if (category == null) {
				category = objectFactory.createCategory(p.getID(), "");
				session.addCategory(category);
			}
			TermUtil.castToClass(lo).addCategory(category);
		}
		else if (pid.equals("oboMetaModel:xref")) {
			TermUtil.castToClass(lo).addDbxref(getDbxref(p.getID()));
		}
		else if (pid.equals("OBO_REL:instance_of")) {
	
			if (p.getID().equals("subsetdef")) {
				TermCategory cat = objectFactory.createCategory(lo.getID(), lo.getName());
				session.addCategory(cat);
				// TODO: remove this at the end; still required as object of subset links
				// session.removeObject(lo);
			}
			else if (p.getID().equals("oban:Annotation")) {
				//Annotation annot = new AnnotationImpl((Instance)lo);
				//session.addObject(annot);
			}
			else {
				
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
	
			for (TermCategory cat : session.getCategories()) {
				saveCategory(cat);
			}
			for (IdentifiedObject io : session.getObjects()) {
				if (io.isBuiltIn()) {
					
				}
				else {
					saveObject(io);
				}
			}
			for (IdentifiedObject io : ldb.getObjects()) {
				if (io instanceof LinkedObject) {
					for (Link link : ldb.getParents((LinkedObject) io)) {
						saveLink(link);
					}
				}
			}
			
		
		} catch (Exception e) {
			System.out.println(e);
			throw new DataAdapterException(e, "Write error");
		}
	}

	protected int saveObject(IdentifiedObject lo) throws SQLException {
		//System.out.println("saving "+lo);
		int iid;
		String ns = "";
		if (ioprofile.replaceLinks) {
			// TODO
			callSqlFunc("remove_links_for_node", lo.getID());
		}
		if (lo instanceof NamespacedObject && lo.getNamespace() != null) {
			ns = lo.getNamespace().getID();
		}
		if (lo instanceof DanglingObject) {
			iid =
				callSqlFunc("store_node",
						lo.getID());
	
		}
		else if (lo instanceof Annotation) {
			Annotation annot = (Annotation)lo;
			iid =
				callSqlFunc("store_annotation",
						annot.getSubject(),
						annot.getRelationship(),
						annot.getObject(),
						"f");
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
			iid =
				callSqlFunc("store_node",
						lo.getID(),
						lo.getName(),
						ns,
				"I");
	
		}
		else if (lo instanceof OBOProperty) {
			iid =
				callSqlFunc("store_node",
						lo.getID(),
						lo.getName(),
						ns,
				"R");
		
		}
		else {
			iid =
				callSqlFunc("store_node",
						lo.getID());
			
		}
		
		// node is saved in database. Now for additional metadata:
		if (lo instanceof DbxrefedObject) {
			for (Dbxref x : ((DbxrefedObject) lo).getDbxrefs()) {
				callSqlFunc("store_node_dbxref_i",iid,x.toString());
			}
		}
		if (lo instanceof CategorizedObject) {
			for (TermCategory x : ((CategorizedObject) lo).getCategories()) {
				callSqlFunc("store_node_subset_link_i",iid,x.getName());
			}
		}
		if (lo instanceof SynonymedObject) {
			for (Synonym x : ((SynonymedObject) lo).getSynonyms()) {
				callSqlFunc("store_node_synonym_i",
						iid,TermUtil.getScopeLabel(x.getScope()),x.getSynonymCategory(),x.getText());
			}
		}
		if (lo instanceof DefinedObject) {
			String def = ((DefinedObject) lo).getDefinition();
			if (def != null) {
				callSqlFunc("store_textdef_i",
						iid,def);
				
			}
		}
		return iid;
	}
	
	protected int saveCategory(TermCategory cat) throws SQLException {
		return
		callSqlFunc("store_subset",
					cat.getName(),
					cat.getDesc());
	
	}


	protected int saveLink(Link link) throws SQLException {
			System.out.println("saving "+link);
			if (link instanceof ValueLink) {
				ValueLink pv = (ValueLink)link;
				Value v = pv.getValue();
				if (v instanceof DatatypeValue) {
					DatatypeValue dv = (DatatypeValue) v;
					System.out.println("dv type="+dv.getType());
					return callSqlFunc("store_tagval",
							link.getChild().getID(),
							link.getType().getID(),
							dv.getValue(),
							dv.getType());
		//					((DatatypeValue) v).getType());
					
				}
				else if (v instanceof IdentifiedObject) {
	//				throw new Exception("eek");
					return 1;
				}
				return 1;
			}
			else {
				return callSqlFunc("store_link",
						link.getChild().getID(),
						link.getType().getID(),
						link.getParent().getID(),
						(TermUtil.isIntersection(link) ? "C" : ""),
						TermUtil.isImplied(link));
			}
			
		}

	public IdentifiedObject attachDescription(OBOSession session, ResultSet rs) throws SQLException {
		IdentifiedObject io;
		String id = rs.getString("node_uid");
		io = lookupObject(session, id);
		String type = rs.getString("type_uid");
		if (!rs.wasNull()) {
			String label = rs.getString("label");
			if (type.equals("definition")) {
				System.err.println(io+" def: "+label);
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
				SynonymCategory categoryObj = objectFactory.createSynonymCategory(category, "", 0);
				syn.setSynonymCategory(categoryObj);
			}

			((SynonymedObject)io).addSynonym(syn);
		}
		return io;
	}
	

	
	public IdentifiedObject lookupObject(OBOSession session, String id) {
		IdentifiedObject io = session.getObject(id);
		if (io == null) {
			io = objectFactory.createDanglingObject(id, false);
			System.err.println("creating dangling for "+id);
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
		System.out.print("sql="+sql+" :: ");
		for (Object ob : args) {
			System.out.print(ob+" ");
		}
		System.out.println("");
		CallableStatement stmt = conn.prepareCall(sql.toString());
		stmt.registerOutParameter(1, Types.INTEGER);
		// System.out.println("stmt="+stmt);
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
			else {
				stmt.setString(i+2, (String)arg);
			}
		}
		//System.out.println("stmt="+stmt+" :: "+args);
		
		boolean rs = stmt.execute();	
		
		System.out.println("rs="+rs);
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


}
