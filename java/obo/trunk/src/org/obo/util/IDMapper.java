package org.obo.util;

import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;

import org.apache.commons.lang.StringUtils;
import org.bbop.util.StringUtil;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.TermCategory;
import org.obo.reasoner.ReasonedLinkDatabase;

public class IDMapper extends SessionWrapper {

	public class IDMapRules {
		boolean useMostSpecific = true;



		public IDMapRules() {
			super();
			// TODO Auto-generated constructor stub
		}

		public boolean isUseMostSpecific() {
			return useMostSpecific;
		}

		public void setUseMostSpecific(boolean useMostSpecific) {
			this.useMostSpecific = useMostSpecific;
		}


	}

	/**
	 * Defaults to GO association file format
	 * @author cjm
	 *
	 */
	public class IDFileMetadata {

		private int entityColumn = 2;
		private int oboIDColumn = 5;
		private int qualifierColumn = 4;
		private String columnDelimiter = "\t";



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
		public String getColumnDelimiter() {
			return columnDelimiter;
		}
		public void setColumnDelimiter(String columnDelimiter) {
			this.columnDelimiter = columnDelimiter;
		}

	}

	private ReasonedLinkDatabase reasoner;
	private IDFileMetadata fileMetadata = new IDFileMetadata();
	private Collection<TermCategory> categories = new HashSet<TermCategory>();
	IDMapRules idmaprules = new IDMapRules();



	public Collection<TermCategory> getCategories() {
		return categories;
	}

	public void setCategories(Collection<TermCategory> categories) {
		this.categories = categories;
	}

	public void addCategory(String cat) {
		categories.add(session.getCategory(cat));
	}

	public IDFileMetadata getFileMetadata() {
		return fileMetadata;
	}

	public void setFileMetadata(IDFileMetadata fileMetadata) {
		this.fileMetadata = fileMetadata;
	}

	public IDMapRules getIdmaprules() {
		return idmaprules;
	}

	public void setIdmaprules(IDMapRules idmaprules) {
		this.idmaprules = idmaprules;
	}

	public ReasonedLinkDatabase getReasoner() {
		return reasoner;
	}

	public void setReasoner(ReasonedLinkDatabase reasoner) {
		this.reasoner = reasoner;
	}

	public IDMapper() {
		super();
	}

	public IDMapper(OBOSession session, ReasonedLinkDatabase linkDatabase) {
		super();
		this.session = session;
		this.reasoner = linkDatabase;
	}


	public Collection<LinkedObject> getParentObjs(OBOObject obj) {
		if (reasoner != null) {
			Collection<LinkedObject> pobjs = new HashSet<LinkedObject>();
			for (Link link : reasoner.getParents(obj)) {
				pobjs.add(link.getParent());
			}
			return pobjs;
		}
		return TermUtil.getAncestors(obj);
	}
	public Collection<OBOObject> mapIdentifierViaCategories(String id) {
		Collection<String> mappedIDs = new HashSet<String>();
		Collection<OBOObject> mappedObjs = new HashSet<OBOObject>();
		Collection<OBOObject> filteredMappedObjs = new HashSet<OBOObject>();
		OBOObject obj = (OBOObject)session.getObject(id);
		for (LinkedObject pobj : getParentObjs(obj)) {
			mappedObjs.add((OBOObject)pobj);
		}
		mappedObjs.add(obj);
		for (OBOObject mappedObj : mappedObjs) {
			for (TermCategory cat : mappedObj.getCategories()) {
				if (categories.contains(cat)) {
					filteredMappedObjs.add(mappedObj);
					continue;
				}
			}
		}
		if (idmaprules.isUseMostSpecific()) {
			Collection<OBOObject> specificObjs = new HashSet<OBOObject>();
			Collection<OBOObject> generalObjs = new HashSet<OBOObject>();
			
			for (OBOObject mappedObj : filteredMappedObjs) {
				boolean isSpecific = true;
				for (LinkedObject p : getParentObjs(mappedObj)) {
					if (!p.equals(mappedObj) &&
							filteredMappedObjs.contains(p)) {
						logger.info(p+" is more general than "+mappedObj+" [in set], so I am removing");
						generalObjs.add((OBOObject)p);
						//isSpecific = false;
						//continue;
					}	
				}
//				if (isSpecific)
//					specificObjs.add(mappedObj);
			}
			filteredMappedObjs.removeAll(generalObjs);
			//filteredMappedObjs = specificObjs;
		}	
		return filteredMappedObjs;
	}

	public void mapIDsInFile(String inputPath) throws IOException {
	
		FileReader fr = new FileReader(inputPath);
		LineNumberReader lnr =  new LineNumberReader(fr);
		int ENTITY = fileMetadata.getEntityColumn()-1;
		int ID = fileMetadata.getOboIDColumn()-1;
		for (String line=lnr.readLine(); line != null; line = lnr.readLine()) {
			String[] colVals = line.split(fileMetadata.getColumnDelimiter(),-1); // include trailing separators

			if (fileMetadata.getQualifierColumn() > 0) {
				String qual = colVals[fileMetadata.getQualifierColumn()-1];
				if (!qual.equals(""))
					continue;
			}
			String oboID = colVals[ID];
			String entityID = colVals[ENTITY];
			Collection<OBOObject> objs = mapIdentifierViaCategories(oboID);
			for (OBOObject obj : objs) {
				colVals[ID] = obj.getID();
				// TODO - name
				printLine(colVals,fileMetadata.getColumnDelimiter());
			}
		}
	}

	private void printLine(String[] colVals, String sep) {
		System.out.println(StringUtils.join(colVals,sep));
		System.out.println("\n");
	}


}
