package org.obo.dataadapter;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import org.obo.datamodel.Namespace;

import org.apache.log4j.*;

public class OBOMetaData {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBOMetaData.class);
	
	public static class FileMetaData {
		
		protected String file;
		protected String version;
		protected Collection<Namespace> namespaces = new LinkedList<Namespace>();
		protected Collection<String> imports = new LinkedList<String>();
		
		public FileMetaData(String file, String version) {
			this.file = file;
			this.version = version;
		}

		public FileMetaData(String file) {
			this.file = file;
		}
		
		public void addImport(String importFile) {
			imports.add(importFile);
		}
		
		public Collection<String> getImports() {
			return imports;
		}

		public String getVersion() {
			return version;
		}

		public void setVersion(String version) {
			this.version = version;
		}

		public String getFile() {
			return file;
		}
		
		public Collection<Namespace> getNamespaces() {
			return namespaces;
		}
		
		public void addNamespace(Namespace n) {
			if (!namespaces.contains(n))
				namespaces.add(n);
		}
	}
	
	protected Map<String, FileMetaData> files = new HashMap<String, FileMetaData>();

	public OBOMetaData() {
	}
	
	protected FileMetaData getData(String file) {
		FileMetaData data = (FileMetaData) files.get(file);
		if (data == null) {
			data = new FileMetaData(file);
			files.put(file, data);
		}
		return data;
	}
	
	public Collection<String> getFileNames() {
		return files.keySet();
	}
	
	public String getVersion(String file) {
		FileMetaData data = getData(file);
		if (data != null)
			return data.getVersion();
		return null;
	}
	
	public Collection<Namespace> getNamespaces(String file) {
		FileMetaData data = getData(file);
		if (data != null)
			return data.getNamespaces();
		return null;
	}

	public void mapFileData(String file, String version) {
		FileMetaData data = getData(file);
		data.setVersion(version);
	}
	
	public void addNamespace(String file, Namespace namespace) {
		FileMetaData data = getData(file);
		data.addNamespace(namespace);		
	}
	
	public Collection<String> getImports(String file) {
		FileMetaData data = getData(file);
		if (data != null)
			return data.getImports();
		return null;
	}
	
	public void addImport(String file, String importFile) {
		FileMetaData data = getData(file);
		data.addImport(importFile);
	}
}
