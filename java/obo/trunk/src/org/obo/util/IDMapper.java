package org.obo.util;

import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.util.Collection;
import java.util.HashSet;

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
		private String columnDelimiter = "\n";
		
	
		
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
	
	public IDMapper() {
		super();
	}

	public IDMapper(OBOSession session, ReasonedLinkDatabase linkDatabase) {
		super();
		this.session = session;
		this.reasoner = linkDatabase;
	}
	
	public Collection<OBOObject> mapIdentifierToCategories(String id, 
			Collection<TermCategory> categories,
			IDMapRules idmaprules) {
		Collection<String> mappedIDs = new HashSet<String>();
		Collection<OBOObject> mappedObjs = new HashSet<OBOObject>();
		Collection<OBOObject> filteredMappedObjs = new HashSet<OBOObject>();
		OBOObject obj = (OBOObject)session.getObject(id);
		for (Link link : reasoner.getParents(obj)) {
			mappedObjs.add((OBOObject)link.getParent());
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
			for (OBOObject mappedObj : filteredMappedObjs) {
				boolean isSpecific = true;
				for (Link link : reasoner.getParents(mappedObj)) {
					LinkedObject p = link.getParent();
					if (!p.equals(mappedObj) &&
							filteredMappedObjs.contains(p)) {
						isSpecific = false;
						continue;
					}	
				}
				if (isSpecific)
					specificObjs.add(mappedObj);
			}
			filteredMappedObjs = specificObjs;
		}	
		return filteredMappedObjs;
	}
	
	public void mapIDsInFile(String inputPath) throws IOException {
		int lineNum = 0;
		String currentLine;

		FileReader fr = new FileReader(inputPath);
		LineNumberReader lnr =  new LineNumberReader(fr);
		for (String line=lnr.readLine(); line != null; line = lnr.readLine()) {
			currentLine = line;
			String[] colVals = line.split(fileMetadata.getColumnDelimiter(),-1); // include trailing separators
			if (fileMetadata.getQualifierColumn() > 0) {
				String qual = colVals[fileMetadata.getQualifierColumn()-1];
				if (!qual.equals(""))
					continue;
			}
		}
	}
	
	
}
