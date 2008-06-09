package org.obo.nlp;

import java.util.Collection;
import java.util.LinkedList;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.OBOSession;
import org.obo.history.HistoryItem;
import org.obo.history.NameChangeHistoryItem;
import org.obo.nlp.impl.LogicalDefinitionNamer;
import org.obo.util.TermUtil;

/**
 * This is useful when loading ontologies that contain unnamed anonymous
 * classes; for example, .obo files converted from OWL such as OBI
 * <p>
 * <a href="http://www.berkeleybop.org/ontologies/obo-all/obi/obi.obo">obi.obo</a>
 * <p>
 * Or some of the cross-product files, in which nested xps are used.
 * <p>
 * Ideally OE would just display the anonymous classes intuitively; but this is
 * a lot of engineering. So the hack is top allow the user to auto-name
 * anonymous class using a simple algorithm that implements a manchester syntax
 * style.
 * <p>
 * For example, OBI has the axiom [written here in Manchester Syntax]:
 * <code>
 * xenograft has_role (specimen_role or reagent_role)
 * </code>
 * The bracketed expression gets converted to obo as the ugly:
 * <code>
 * [Term] id:
 * __file:///data/cluster/cjm/obo/website/utils/obo-all/obi/obi.owl#__Description6
 * is_anonymous: true union_of: obi:OBI_0000086 ! reagent role union_of:
 * obi:OBI_0000254 ! specimen_role
 * </code>
 * Which by default shows up as empty in the GUI, very confusing
 * <p>
 * Using NamerUtil, this gets named as "specimen_role -or- reagent_role"
 * <p>
 * It's not ideal but it helps bridge the gap between the ontology modeling
 * paradigms.
 * <p>
 * @author cjm
 * 
 */
public class NamerUtil {
	
	public static Namer getDefaultNamer() {
		return new LogicalDefinitionNamer();
	}
	public static void nameUnnamedObjects(OBOSession session) {
		nameUnnamedObjects(session, getDefaultNamer());
	}
	public static void nameUnnamedObjects(OBOSession session, Namer namer) {
		for (OBOObject io : TermUtil.getOBOObjects(session)) {
			if (io.getName() == null) {
				Collection<String> names = namer.constructNames(io);
				io.setName(names.iterator().next());
			}
			if (io.getName() == null) {
				io.setName(io.getID());
			}
		}
	}
	public static Collection<HistoryItem> getNameUnnamedObjectsAction(OBOSession session) {
		return getNameUnnamedObjectsAction(session, getDefaultNamer());
	}
	public static Collection<HistoryItem> getNameUnnamedObjectsAction(OBOSession session, Namer namer) {
		Collection<HistoryItem> items = new LinkedList<HistoryItem>();
		for (OBOObject io : TermUtil.getOBOObjects(session)) {
			String newName = null;
			if (io.getName() == null) {
				Collection<String> names = namer.constructNames(io);
				for (String name : names) {
					newName = name;
					break;
				}
				if (newName == null) {
					newName = io.getID();
				}
			}
			if (newName != null) {
				items.add(new NameChangeHistoryItem(io,newName));
			}
		}
		return items;
	}

}
