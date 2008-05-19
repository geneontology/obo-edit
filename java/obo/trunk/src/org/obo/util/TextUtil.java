package org.obo.util;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

import org.obo.datamodel.Dbxref;
import org.obo.datamodel.DbxrefedObject;
import org.obo.datamodel.DefinedObject;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.SynonymedObject;

import org.apache.log4j.*;

public class TextUtil {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TextUtil.class);

	private TextUtil() {
	}

	public static Collection<Dbxref> getAllDbxrefs(IdentifiedObject io) {
		Collection<Dbxref> out = new LinkedList<Dbxref>();
		if (io instanceof DbxrefedObject) {
			out.addAll(((DbxrefedObject) io).getDbxrefs());
		}
		if (io instanceof DefinedObject) {
			out.addAll(((DefinedObject) io).getDefDbxrefs());
		}
		if (io instanceof SynonymedObject) {
			Iterator it = ((SynonymedObject) io).getSynonyms().iterator();
			while (it.hasNext()) {
				Synonym s = (Synonym) it.next();
				out.addAll(s.getDbxrefs());
			}
		}
		return out;
	}
}
