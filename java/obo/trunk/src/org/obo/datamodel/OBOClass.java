package org.obo.datamodel;

import org.obo.datamodel.impl.*;

public interface OBOClass extends OBOObject, Type, Comparable {

	public static final OBOClass OBO_CLASS = new OBOClassImpl("obo:TERM",
			"obo:TERM") {
		/**
				 * 
				 */
				private static final long serialVersionUID = 1L;

		@Override
		public boolean isBuiltIn() {
			return true;
		}
	};
	public static final OBOClass OBO_PROPERTY = new OBOClassImpl("obo:TYPE",
			"obo:TYPE") {
		/**
				 * 
				 */
				private static final long serialVersionUID = 1646247624094874338L;

		@Override
		public boolean isBuiltIn() {
			return true;
		}
	};
	public static final OBOClass OBO_DATATYPE = new OBOClassImpl(
			"obo:datatype", "obo:datatype") {
		/**
				 * 
				 */
				private static final long serialVersionUID = 1340252671132110002L;

		@Override
		public boolean isBuiltIn() {
			return true;
		}
	};

	public static final OBOClass OBO_UNKNOWN = new OBOClassImpl("obo:unknown",
			"obo:unknown") {
		/**
				 * 
				 */
				private static final long serialVersionUID = -211021298957265414L;

		@Override
		public boolean isBuiltIn() {
			return true;
		}
	};

	/**
	 * An untyped instance. Illegal in any context except when loading a file
	 */
	public static final OBOClass OBO_INSTANCE = new OBOClassImpl(
			"obo:instance", "obo:instance") {
		/**
				 * 
				 */
				private static final long serialVersionUID = -6444859861822518514L;

		@Override
		public boolean isBuiltIn() {
			return true;
		}
	};

	public static final OBOClass OBO_OBJECT = new OBOClassImpl("obo:object",
			"obo:object") {
		/**
				 * 
				 */
				private static final long serialVersionUID = -4875684963877846849L;

		@Override
		public boolean isBuiltIn() {
			return true;
		}
	};
	
	public static final OBOClass OBO_LINK = new OBOClassImpl("obo:link",
	"obo:link") {
/**
		 * 
		 */
		private static final long serialVersionUID = 8278049177849877073L;

@Override
public boolean isBuiltIn() {
	return true;
}
};


	public static final OBOClass[] BUILTIN_CLASSES = { OBO_CLASS, OBO_PROPERTY,
			OBO_DATATYPE, OBO_OBJECT, OBO_LINK };

}
