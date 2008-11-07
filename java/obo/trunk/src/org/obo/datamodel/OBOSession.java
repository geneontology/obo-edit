package org.obo.datamodel;

import java.util.*;
import java.io.Serializable;

import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.impl.DefaultLinkDatabase;
import org.obo.datamodel.impl.DefaultObjectFactory;
import org.obo.datamodel.impl.DefaultOperationModel;
import org.obo.history.CreateObjectHistoryItem;
import org.obo.history.DestroyObjectHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.history.OperationModel;
import org.obo.history.SessionHistoryList;
import org.obo.history.TermSubsetHistoryItem;
import org.obo.identifier.IDProfile;
import org.obo.query.QueryResolver;
import org.obo.query.impl.DefaultQueryResolver;
import org.obo.util.TermUtil;

public interface OBOSession extends IdentifiedObjectIndex, Serializable {

	/**
	 * Imports another OBOSession into this OBOSession. All ontology objects
	 * will be merged into a single session, and all dangling links that can be
	 * resolved will be.
	 * 
	 * This method may be run in one of two modes. If the executeImmediately
	 * parameter is true, the import will occur immediately, and null will be
	 * returned. If it is false, the import will not occur, but a HistoryItem
	 * will be returned that will execute the import when it is applied.
	 * 
	 * @param session
	 *            the session to import
	 * @param executeImmediately
	 *            whether to perform the import immediately, or return a history
	 *            item
	 * @return a history item or null, depending on the value of
	 *         executeImmediately
	 */
	public HistoryItem importSession(OBOSession session,
			boolean executeImmediately);

	/**
	 * Returns a link database representing all the objects and links in this
	 * ontology. The returned link database is typically an instance of
	 * {@link DefaultLinkDatabase}
	 * 
	 * @return a link database for this ontology
	 */
	public LinkDatabase getLinkDatabase();

	/**
	 * Returns the {@link ObjectFactory} used to create new objects for this
	 * ontology. Although this method usually returns a
	 * {@link DefaultObjectFactory}, OBOSessions that perform special caching
	 * (say, to provide a realtime database connection) may return a custom
	 * object factory.
	 * 
	 * @return the object factory used to create new objects for this session
	 */
	public ObjectFactory getObjectFactory();

	/**
	 * Returns the current record of edits in this session, as well as edit meta
	 * data like the ontology version and user name.
	 * 
	 * @return the current edit list
	 */
	public SessionHistoryList getCurrentHistory();

	/**
	 * Returns the {@link OperationModel} used to modify this session. This
	 * method usually returns a {@link DefaultOperationModel}, but OBOSessions
	 * that perform special caching (say, to provide a realtime database
	 * connection) may use a custom operation model.
	 * 
	 * @return the operation model used to apply edits to this session
	 */
	public OperationModel getOperationModel();

	/**
	 * Returns the {@link IdentifiedObject} in this session with the given id.
	 * Note that this method will return {@link DanglingObject}s or
	 * {@link LinkLinkedObject}s they match.
	 * 
	 * @param id
	 *            the identifier to find
	 * @return the IdentifiedObject with the given identifier
	 */
	public IdentifiedObject getObject(String id);

	/**
	 * Adds an IdentifiedObject to this session. This method should not normally
	 * be called directly by user code (unless the user code is a data adapter).
	 * To add an object to a session, apply a {@link CreateObjectHistoryItem} to
	 * the session's {@link #getOperationModel() OperationModel}.
	 * 
	 * @param obj
	 *            the object to add
	 */
	public void addObject(IdentifiedObject obj);

	/**
	 * Removes an object from this session. This method should not normally be
	 * called by user code. Even when objects are deleted they are not usually
	 * removed from the session; instead, they are made obsolete. Objects are
	 * only removed from the session when they are destroyed. Even then, to
	 * destroy an object, apply a {@link DestroyObjectHistoryItem} to the
	 * session's {@link #getOperationModel() OperationModel} rather than calling
	 * this method directly.
	 * 
	 * @param obj
	 *            the object to destroy
	 */
	public void removeObject(IdentifiedObject obj);

	/**
	 * Returns all the IdentifiedObjects in this ontology, including terms,
	 * instances, relationship types, built-in objects, and obsolete objects. To
	 * fetch only certain kinds of objects, see the utility methods in
	 * {@link TermUtil}.
	 * 
	 * @return a collection of all identified objects in the ontology
	 * @see {@link TermUtil#getObsoletes(OBOSession)}
	 * @see {@link TermUtil#getTerms(OBOSession)}
	 * @see {@link TermUtil#getInstances(OBOSession)}
	 * @see {@link TermUtil#getRelationshipTypes(OBOSession)}
	 */
	public Collection<IdentifiedObject> getObjects();

	/**
	 * Sets the default namespace for this session. When an object is created
	 * with no namespace (which normally only occurs for roots, other terms are
	 * automatically assigned to their parent namespace) this namespace is used.
	 * 
	 * @param ns
	 *            the default namespace
	 */
	public void setDefaultNamespace(Namespace ns);

	/**
	 * Gets the default namespace for this session. When an object is created
	 * with no namespace (which normally only occurs for roots, other terms are
	 * automatically assigned to their parent namespace) this namespace is used.
	 * 
	 * @return the default namespace
	 */
	public Namespace getDefaultNamespace();

	/**
	 * Adds a namespace to this session. This method should not normally be
	 * called directly by user code (unless the user code is a data adapter). To
	 * add a namespace to a session, apply a {@link TermNamespaceHistoryItem} to
	 * the session's {@link #getOperationModel() OperationModel}.
	 * 
	 * @param ns
	 *            the namespace to add
	 */
	public void addNamespace(Namespace ns);

	/**
	 * Removes a namespace from this session. This method should not normally be
	 * called directly by user code. To add a namespace to a session, apply a
	 * {@link TermNamespaceHistoryItem} to the session's
	 * {@link #getOperationModel() OperationModel}.
	 * 
	 * @param ns
	 *            the namespace to remove
	 */
	public void removeNamespace(Namespace ns);

	/**
	 * Returns the namespace with the given id
	 * 
	 * @param id
	 *            the id of the namespace to find
	 * @return the namespace with the given id
	 */
	public Namespace getNamespace(String id);

	/**
	 * Returns all namespaces in this ontology.
	 * 
	 * @return all namespaces in the ontology
	 */
	public Collection<Namespace> getNamespaces();

	/**
	 * Returns any special remark associated with this session. The contents of
	 * this remark may vary depending on how the OBOSession was loaded.
	 * 
	 * @return the session remark
	 */
	public String getLoadRemark();

	/**
	 * Sets a remark for this session. This remark may be used by a data adapter
	 * at save time. For example, the {@link OBOFileAdapter} places this remark
	 * in a remark: tag at the head of a saved file.
	 * 
	 * @return the session remark
	 */
	public void setLoadRemark(String loadRemark);

	/**
	 * Sets the default ID profile for this session. This ID profile can be
	 * saved and loaded by some data adapters so that all users of the ontology
	 * can use a consistent id generation scheme.
	 * 
	 * @param profile
	 *            the session's default id profile
	 */
	public void setIDProfile(IDProfile profile);

	/**
	 * Gets the default ID profile for this session. This ID profile can be
	 * saved and loaded by some data adapters so that all users of the ontology
	 * can use a consistent id generation scheme.
	 * 
	 * @return the session's default id profile
	 */
	public IDProfile getIDProfile();

	/**
	 * Adds an OBO tag-value pair to this session. When the session is saved,
	 * the session tag-value pairs will appear in the file header. This method
	 * should normally only be called by a data adapter to store data that
	 * cannot be handled by the adapter, but needs to be saved for
	 * round-tripping.
	 * 
	 * @param pv
	 *            the tag-value pair to add
	 */
	public void addPropertyValue(PropertyValue pv);

	/**
	 * Returns all the OBO tag-value pairs associated to this session. When the
	 * session is saved, the session tag-value pairs will appear in the file
	 * header. This method should normally only be called by a data adapter to
	 * roundtrip data that was not parsed by the data adapter that loaded the
	 * session.
	 * 
	 * @param pv
	 *            the tag-value pair to add
	 */
	public Collection<PropertyValue> getPropertyValues();

	/**
	 * Adds an UnknownStanza to this session. This method should only be called
	 * by a data adapter that is storing unrecognized OBO stanzas for the
	 * purposes of round-tripping.
	 * 
	 * @param us
	 *            the UnknownStanza to add
	 */
	public void addUnknownStanza(UnknownStanza us);

	/**
	 * Removes an UnknownStanza to this session. This method should only be
	 * called by code that needs to strip out certain UnknownStanzas before
	 * saving.
	 * 
	 * @param us
	 *            the UnknownStanza to remove
	 */
	public void removeUnknownStanza(UnknownStanza us);

	/**
	 * Returns all UnknownStanzas in this session. This method should only be
	 * called by a data adapter that is storing unrecognized OBO stanzas for the
	 * purposes of round-tripping.
	 * 
	 * @param us
	 *            the UnknownStanza to add
	 */
	public Collection<UnknownStanza> getUnknownStanzas();

	/**
	 * Returns all the {@link TermSubset TermCategories} in the session.
	 * 
	 * @return the session's term categories
	 */
	public Collection<TermSubset> getSubsets();

	/**
	 * Adds a {@link TermSubset TermCategory} to the session. This method
	 * should not normally be called directly by user code (unless the user code
	 * is a data adapter). To add a {@link TermSubset TermCategory} to a
	 * session, apply a {@link TermSubsetHistoryItem} to the session's
	 * {@link #getOperationModel() OperationModel}.
	 * 
	 * @return the category to add
	 */
	public void addSubset(TermSubset subset);

	/**
	 * Removes a {@link TermSubset TermCategory} from the session. This method
	 * should not normally be called directly by user code. To remove a
	 * {@link TermSubset TermCategory} from a session, apply a
	 * {@link TermSubsetHistoryItem} to the session's
	 * {@link #getOperationModel() OperationModel}.
	 * 
	 * @return the category to remove
	 */
	public void removeCategory(TermSubset cat);

	/**
	 * Returns the {@link TermSubset} with the given
	 * {@link TermSubset#getName() name}.
	 * 
	 * @param name
	 *            the name of the category to fetch
	 * @return the category with the given name, or null if there is no match
	 * 
	 */
	public TermSubset getCategory(String name);

	/**
	 * Returns all the {@link SynonymType SynonymCategories} in the session.
	 * 
	 * @return the session's synonym categories
	 */
	public Collection<SynonymType> getSynonymTypes();

	/**
	 * Adds a {@link SynonymType} to the session. This method should not
	 * normally be called directly by user code (unless the user code is a data
	 * adapter). To add a {@link SynonymType} to a session, apply a
	 * {@link SynonymCategoryHistoryItem} to the session's
	 * {@link #getOperationModel() OperationModel}.
	 * 
	 * @return the synonym category to add
	 */
	public void addSynonymType(SynonymType typ);

	/**
	 * Removes a {@link SynonymType} from the session. This method should
	 * not normally be called directly by user code. To remove a
	 * {@link SynonymType} from a session, apply a
	 * {@link SynonymCategoryHistoryItem} to the session's
	 * {@link #getOperationModel() OperationModel}.
	 * 
	 * @return the synonym category to remove
	 */
	public void removeSynonymCategory(SynonymType typ);

	/**
	 * Returns the {@link SynonymType} with the given
	 * {@link SynonymType#getName() name}.
	 * 
	 * @param name
	 *            the name of the synonym category to fetch
	 * @return the synonym category with the given name, or null if there is no
	 *         match
	 * 
	 */
	public SynonymType getSynonymType(String name);

	/**
	 * Returns a list of past editing sessions that were performed on the
	 * ontologies in this session. Most flat-file-based data adapters do not
	 * provide this information.
	 * 
	 * @return a list of {@link SessionHistoryList} objects representing
	 *         previous editing sessions
	 */
	public List<SessionHistoryList> getArchivedHistories();

	/**
	 * Returns a special {@link QueryResolver} to use when resolving queries
	 * against this session. Most implementations of OBOSession will return null
	 * for this method, signifying that the query system should use the
	 * {@link DefaultQueryResolver}. If this method <b>does</b> return a
	 * {@link QueryResolver}, the QueryResolver is given the first opportunity to
	 * resolve queries against this session.
	 * 
	 * This method is mainly used to take advantage of fast indexing provided by the
	 * datasource that created this session. For example, an OBOSession loaded by
	 * a database-backed data adapter might have a special query resolver that knows
	 * how to use pre-computed database indices to quickly resolve some queries.
	 * 
	 * @return
	 */
	public QueryResolver getQueryResolver();
	
	/**
	 * Returns the current user id for this session. This id is used when setting the "created_by" and
	 * "modified_by" fields of {@link ModificationMetadataObject}
	 * 
	 * @return
	 */
	public String getCurrentUser();

	/**
	 * Sets the current user id for this session.
	 * 
	 * @param user
	 */
	public void setCurrentUser(String user);

	/**
	 * Sets an OBO ID prefix -> URI prefix mapping for this session.
	 * 
	 * @param idspace
	 * @param uriPrefix
	 */
	public void addIDSpace(String idspace, String uriPrefix);
	
	public Collection<String> getIDSpaces();
	
	public String expandIDSpace(String idspace);
}
