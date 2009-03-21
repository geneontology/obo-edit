package org.obo.util;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.bbop.util.MultiMap;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.TermSubset;
import org.obo.filters.SubsetSearchCriterion;
import org.obo.filters.Filter;
import org.obo.filters.ObjectFilter;
import org.obo.filters.ObjectFilterImpl;
import org.obo.identifier.IDResolution;
import org.obo.identifier.IDWarning;
import org.obo.reasoner.ReasonedLinkDatabase;

import org.apache.log4j.*;

public class IDMapper extends SessionWrapper {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IDMapper.class);



	public final class MapperInputParams {
		private final boolean isSpecificOnly;
		private final Filter filter;
		
		public MapperInputParams(final boolean isSpecificOnly, final Filter filter) {
			super();
			this.isSpecificOnly = isSpecificOnly;
			this.filter = filter;
		}
		public final Filter getFilter() {
			return filter;
		}
		public final boolean isSpecificOnly() {
			return isSpecificOnly;
		}
		public final String toString() {
			String s = isSpecificOnly ? "Specific-only" : "Include-ancestors";
			return s + filter.toString();
		}
		@Override
		public boolean equals(Object o) {
			if (o instanceof MapperInputParams)
				return o.toString().equals(toString());
			else
				return false;
		}
		
		@Override
		public final int hashCode() {
			return toString().hashCode();
		}
		
	}
	/**
	 * Lightweight class for representing a line from an
	 * annotation file. This is intentionally record-centric
	 * 
	 * @author cjm
	 *
	 */
	public class SimpleAnnotation {
		private String entityID;
		private String oboID;
		private String[] colVals;
		public SimpleAnnotation(String entityID, String oboID) {
			super();
			this.oboID = oboID;
			this.entityID = entityID;
		}
		public final String getEntityID() {
			return entityID;
		}
		public final String getOboID() {
			return oboID;
		}
		public String[] getColVals() {
			return colVals;
		}
		public void setColVals(String[] colVals) {
			this.colVals = colVals;
		}

	}

	/**
	 * Defaults to GO association file format
	 * @author cjm
	 *
	 */
	public final class IDFileMetadata {

		private int entityColumn = 2;
		private int oboIDColumn = 5;
		private int qualifierColumn = 4;
		private int minCols = 6;
		private String columnDelimiter = "\t";
		private String commentCharacter = "!";



		public IDFileMetadata() {
			super();
			// TODO Auto-generated constructor stub
		}
		public int getEntityColumn() {
			return entityColumn;
		}
		public void setEntityColumn(int entityColumn) {
			this.entityColumn = entityColumn;
		}
		public int getOboIDColumn() {
			return oboIDColumn;
		}
		public void setOboIDColumn(int oboIDColumn) {
			this.oboIDColumn = oboIDColumn;
		}
		public int getQualifierColumn() {
			return qualifierColumn;
		}
		public void setQualifierColumn(int qualifierColumn) {
			this.qualifierColumn = qualifierColumn;
		}
		
		public final int getMinCols() {
			return minCols;
		}
		public final void setMinCols(int minCols) {
			this.minCols = minCols;
		}
		public String getColumnDelimiter() {
			return columnDelimiter;
		}
		public void setColumnDelimiter(String columnDelimiter) {
			this.columnDelimiter = columnDelimiter;
		}
		public final String getCommentCharacter() {
			return commentCharacter;
		}
		public final void setCommentCharacter(String commentCharacter) {
			this.commentCharacter = commentCharacter;
		}
		

	}

	private ReasonedLinkDatabase reasoner;
	private Collection<OBOProperty> propertiesToTraverse;
	private IDFileMetadata fileMetadata = new IDFileMetadata();
	private Collection<TermSubset> categories = new HashSet<TermSubset>();
	private Filter filter;
	private MapperInputParams inputParams;


	private Map<MapperInputParams,Map<String,Collection<OBOObject>>> cacheMap =
		new HashMap<MapperInputParams,Map<String,Collection<OBOObject>>>();
	private Map<String,Integer> entityCountByOboIDMap;
	private MultiMap<String, IdentifiedObject> secondaryIDMap = null;
	private Collection<IDWarning> warnings = new HashSet<IDWarning>();
	private boolean autoReplaceConsiderTags = false;


	public MultiMap<String, IdentifiedObject> getSecondaryIDMap() {
		if (secondaryIDMap == null)
			secondaryIDMap = IDUtil.getSecondaryIDMap(getSession());
		return secondaryIDMap;
	}



	public  Collection<IDWarning> getWarnings() {
		return warnings;
	}

	public  void setWarnings(Collection<IDWarning> warnings) {
		this.warnings = warnings;
	}
	public void addWarning(IDWarning warning) {
		getWarnings().add(warning);
	}



	public Map<String, Integer> getEntityCountByOboIDMap() {
		return entityCountByOboIDMap;
	}

	public Collection<TermSubset> getCategories() {
		return categories;
	}

	public void setCategories(Collection<TermSubset> categories) {
		this.categories = categories;
	}

	public void addCategory(String cat) {
		categories.add(session.getCategory(cat));
	}

	public void addCategory(TermSubset cat) {
		categories.add(cat);
	}
	public void setCategory(TermSubset cat) {
		categories = new  HashSet<TermSubset>();
		addCategory(cat);
	}

	public IDFileMetadata getFileMetadata() {
		return fileMetadata;
	}

	public void setFileMetadata(IDFileMetadata fileMetadata) {
		this.fileMetadata = fileMetadata;
	}


	public ReasonedLinkDatabase getReasoner() {
		return reasoner;
	}

	public void setReasoner(ReasonedLinkDatabase reasoner) {
		this.reasoner = reasoner;
		reasoner.setLinkDatabase(session.getLinkDatabase());
	}
	
	

	public Collection<OBOProperty> getPropertiesToTraverse() {
		return propertiesToTraverse;
	}



	public void setPropertiesToTraverse(Collection<OBOProperty> propertiesToTraverse) {
		this.propertiesToTraverse = propertiesToTraverse;
	}
	
	public void addPropertyToTraverse(String propID) {
		 OBOProperty prop = (OBOProperty)session.getObject(propID);
		 addPropertyToTraverse(prop);
	}
	
	public void addPropertyToTraverse(OBOProperty prop) {
		 if (propertiesToTraverse == null)
			 propertiesToTraverse = new HashSet<OBOProperty>();
		 propertiesToTraverse.add(prop);
	}




	public final boolean isAutoReplaceConsiderTags() {
		return autoReplaceConsiderTags;
	}



	public final void setAutoReplaceConsiderTags(boolean autoReplaceConsiderTags) {
		this.autoReplaceConsiderTags = autoReplaceConsiderTags;
	}



	public IDMapper() {
		super();
	}

	public IDMapper(OBOSession session, ReasonedLinkDatabase linkDatabase) {
		super();
		this.session = session;
		this.reasoner = linkDatabase;
	}


	/**
	 * returns all the ancestors of the specified OBOObject.
	 * If the reasoner is NOT set, this will do a basic blind transitive
	 * closure ignoring link type.
	 * If the reasoner IS set, then only links computed by the reasoner are used.
	 * - the reasoned link type must be in the set provided in {@link #setPropertiesToTraverse()}
	 *   (or the IS_A link). If not properties are set, then any link matches
	 *   
	 * Example:
	 * 
	 * if the DAG says
	 *  A is_a B regulates C is_a D part_of E is_a F foo G
	 * 
	 * If we do not have the reasoner set then F is an ancestor object of A
	 * 
	 * If we *do* have the reasoner set, then:
	 *  - C is an ancestor of A by type regulates
	 *  - E is an ancestor of C by type part_of
	 *  - F is an ancestor of A by type regulates (assuming regulates is transitive over part_of)
	 *  - G is an ancestor of E by type foo
	 *  - G is *NOT* an ancestor of A-D
	 * 
	 * @param object  - focus object
	 * @return all ancestor objects
	 * @see #setPropertiesToTraverse()
	 */
	private Collection<LinkedObject> getAncestorObjects(OBOObject object) {
		if (reasoner != null) {
			Collection<LinkedObject> pobjs = new HashSet<LinkedObject>();
			for (Link link : reasoner.getParents(object)) {
				if (propertiesToTraverse == null ||
						propertiesToTraverse.size() == 0 ||
						link.getType().equals(OBOProperty.IS_A) ||
						propertiesToTraverse.contains(link.getType())) // TODO
					pobjs.add(link.getParent());
			}
			return pobjs;
		}
		return TermUtil.getAncestors(object);
	}

	/**
	 * 
	 * 
	 * @param id
	 * @param countMode
	 * @return
	 */
	public Collection<OBOObject> mapIdentifierViaFilter(String id, boolean countMode) {
		// note that this method is just a memoization wrapper for mapIdentifierViaFilterUncached
		// TODO: memoize on a per-filter basis
		inputParams = new MapperInputParams(!countMode, filter);
		//if (!mapCacheByFilter.containsKey(filter)) {
	//		mapCacheByFilter.put(filter, new HashMap<String,Collection<OBOObject>>());
		// }
		if (!cacheMap.containsKey(inputParams)) {
			logger.info("creating new cache for "+inputParams);
			cacheMap.put(inputParams, new HashMap<String,Collection<OBOObject>>());
		}
		else {
			logger.info("reusing cache for "+inputParams);
		}
		//Map<String, Collection<OBOObject>> map = mapCacheByFilter.get(filter);
		Map<String, Collection<OBOObject>> map = cacheMap.get(inputParams);
		
		if (map.containsKey(id))
			return map.get(id);
		logger.info(id+" not in cache: "+map);
		Collection<OBOObject> filteredMappedObjs = mapIdentifierViaFilterUncached(id, countMode);
		// cache mapping
		map.put(id, filteredMappedObjs);
		return filteredMappedObjs;
	}
	/**
	 * This class does the bulk of the work for {@link #mapIdentifierViaFilter(String, boolean)
	 * 
	 * @param id
	 * @param countMode
	 * @return
	 */
	private Collection<OBOObject> mapIdentifierViaFilterUncached(String id, boolean countMode) {
		Collection<String> mappedIDs = new HashSet<String>();
		Collection<OBOObject> filteredMappedObjs = new HashSet<OBOObject>();
		OBOObject obj = (OBOObject)session.getObject(id);
		if (obj == null || obj.isObsolete()) {
			IDWarning warning = IDUtil.getWarning(id, false, session, getSecondaryIDMap());
			if (warning != null) {
				addWarning(warning);
				Collection<IDResolution> resols = warning.getResolutions();
				Collection<OBOObject> replacementObjs = new HashSet<OBOObject>();
				for (IDResolution resol : resols) {
					if (resol.requiresUserIntervention() && !isAutoReplaceConsiderTags()) {
						
					}
					else {
						// TODO - this is not really valid. one replacement may subsume another
						replacementObjs.addAll(mapIdentifierViaFilter(resol.getReplacementID(),
								countMode));
					}
				}
				return replacementObjs;
			}
			else {
				// dangling objs
				//logger.info("don't know what to do with: "+id);
				return filteredMappedObjs;
			}
		}

		// we map the objects first, THEN apply the filter
		// this saves repeated applications
		Collection<OBOObject> mappedObjs = new HashSet<OBOObject>();
		for (LinkedObject pobj : getAncestorObjects(obj)) {
			mappedObjs.add((OBOObject)pobj);
		}
		mappedObjs.add(obj); // reflexivity
		for (OBOObject mappedObj : mappedObjs) {
			if (filter.satisfies(mappedObj))
				filteredMappedObjs.add(mappedObj);

		}
		if (!countMode) {
			Collection<OBOObject> generalObjs = new HashSet<OBOObject>();

			for (OBOObject mappedObj : filteredMappedObjs) {
				boolean isSpecific = true;
				for (LinkedObject p : getAncestorObjects(mappedObj)) {
					if (!p.equals(mappedObj) &&
							filteredMappedObjs.contains(p)) {
						logger.info(p+" is more general than "+mappedObj+" [in set], so I am removing");
						generalObjs.add((OBOObject)p);
					}	
				}
			}
			filteredMappedObjs.removeAll(generalObjs);
		}	
		return filteredMappedObjs;
	}
	public Collection<OBOObject> mapIdentifierViaCategories(String id, boolean countMode) {
		Collection<Filter<LinkedObject>> filters = new LinkedList<Filter<LinkedObject>>();
		for (TermSubset cat : categories) {
			ObjectFilter f = new ObjectFilterImpl();
			f.setCriterion(new SubsetSearchCriterion());
			f.setValue(cat.getName());
			filters.add(f);
		}
		filter = FilterUtil.mergeFilters(filters);
		return mapIdentifierViaFilter(id, countMode);	
	}

	public SimpleAnnotation parseAndFilterLine(String line) {
		if (line.substring(0,1).equals(fileMetadata.getCommentCharacter()))
			return null;
		String[] colVals = line.split(fileMetadata.getColumnDelimiter(),-1); // include trailing separators
		if (colVals.length < fileMetadata.getMinCols())
			return null;
		int ENTITY = fileMetadata.getEntityColumn()-1;
		int ID = fileMetadata.getOboIDColumn()-1;
		if (fileMetadata.getQualifierColumn() > 0) {
			String qual = colVals[fileMetadata.getQualifierColumn()-1];
			if (!qual.equals(""))
				return null;
		}
		SimpleAnnotation annot = new SimpleAnnotation(colVals[ENTITY], colVals[ID]);
		annot.setColVals(colVals);
		return annot;
	}


	public void calcEntityCountByOboID(Map<String,Collection<String>> e2ids) {
		// example: GO slim term count by gene product ID
		entityCountByOboIDMap = new HashMap<String,Integer>();

		// iterate through entities first
		for (String entityID : e2ids.keySet()) {

			// what is this entity annotated to?
			// we first build the unique set of mapped IDs, THEN we use this for counting
			HashSet<String> allMappedIDsForEntity = new HashSet<String>();
			for (String oboID : e2ids.get(entityID)) {
				// TODO: other ways beyond categories
				Collection<OBOObject> objs = mapIdentifierViaCategories(oboID,true);
				for (OBOObject obj : objs) {
					allMappedIDsForEntity.add(obj.getID());
				}
			}

			// increment the total# of distinct entities for the mapped IDs
			// mappedIDs is a set, so we cannot have double counting
			for (String mappedID : allMappedIDsForEntity) {
				if (!entityCountByOboIDMap.containsKey(mappedID))
					entityCountByOboIDMap.put(mappedID, 1);
				else
					entityCountByOboIDMap.put(mappedID, entityCountByOboIDMap.get(mappedID)+1);
			}
		}

	}

	public Map<String, Collection<String>> simpleAnnotationFileParse(String inputPath) throws IOException { 
		FileReader fr = new FileReader(inputPath);
		LineNumberReader lnr =  new LineNumberReader(fr);
		Map<String, Collection<String>> e2ids = new HashMap<String,Collection<String>>();
		for (String line=lnr.readLine(); line != null; line = lnr.readLine()) {
			SimpleAnnotation annot = parseAndFilterLine(line);
			if (annot == null)
				continue;
			String[] colVals = annot.getColVals();
			String oboID = annot.getOboID();
			String entityID = annot.getEntityID();
			if (!e2ids.containsKey(entityID))
				e2ids.put(entityID, new HashSet<String>());
			e2ids.get(entityID).add(oboID);
		}
		return e2ids;			
	}



	public void reset() {
		setWarnings(new HashSet<IDWarning>());
		cacheMap =
			new HashMap<MapperInputParams,Map<String,Collection<OBOObject>>>();	
	}


}
