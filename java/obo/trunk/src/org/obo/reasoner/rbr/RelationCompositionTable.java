package org.obo.reasoner.rbr;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.util.TermUtil;

/**
 * Table showing relation compositions A.B->C
 * 
 * @see http://wiki.geneontology.org/index.php/Relation_composition
 * @author cjm
 *
 */
public class RelationCompositionTable {
	protected final static Logger logger = Logger.getLogger(RelationCompositionTable.class);
	private Map<OBOProperty,Map<OBOProperty,Set<OBOProperty>>> propertyMap;
	final private HashSet<OBOProperty> emptySet = new HashSet<OBOProperty>();
	private Collection<OBOProperty> properties;

	public RelationCompositionTable(LinkDatabase ldb) {
		super();
		initialize(ldb);
	}

	public void initialize(LinkDatabase ldb) {
		propertyMap = new HashMap<OBOProperty,Map<OBOProperty,Set<OBOProperty>>>();
		OBOProperty is_a = OBOProperty.IS_A;
		properties = new HashSet<OBOProperty>();
		properties.addAll(ldb.getProperties());
		properties.add(is_a);
		for (OBOProperty a : properties) {
			if (a.isNonInheritable()) {
				continue;
			}
			if (a.getDisjointOver() != null)
				continue;
			if (a.isTransitive()) {
				add(a,a,a);
			}
			if (!a.equals(is_a)) {
				add(a,is_a,a);
				add(is_a,a,a);
			}
			if (a.getTransitiveOver() != null) {
				add(a,a.getTransitiveOver(),a);
			}
			if (a.getHoldsOverChains() != null) {
				for (List<OBOProperty> chain : a.getHoldsOverChains()) {
					if (chain.size() != 2) {
						logger.error("only chains of size 2 allowed. Illegal: "+chain);
					}
					else {
						add(chain.get(0), chain.get(1), a);
					}
				}
			}
		}

	}

	public boolean hasComposition(OBOProperty a) {
		return propertyMap.containsKey(a);
	}

	public Set<OBOProperty> lookup(OBOProperty a, OBOProperty b) {
		if (propertyMap.containsKey(a)) {
			if (propertyMap.get(a).containsKey(b)) {
				return propertyMap.get(a).get(b);
			}
		}
		return emptySet;
	}

	public void add(OBOProperty a, OBOProperty b, OBOProperty c) {
		//add(a,b,c);
		Collection<OBOProperty> acs = TermUtil.getSubProperties(a);
		Collection<OBOProperty> bcs = TermUtil.getSubProperties(b);
		acs.add(a);
		bcs.add(b);
		for (OBOProperty ac : acs) {
			for (OBOProperty bc : bcs) {
				directAdd(ac,bc,c);
			}
		}
	}

	public void directAdd(OBOProperty a, OBOProperty b, OBOProperty c) {
		if (!propertyMap.containsKey(a))
			propertyMap.put(a, new HashMap<OBOProperty,Set<OBOProperty>>());

		if (!propertyMap.get(a).containsKey(b))
			propertyMap.get(a).put(b, new HashSet<OBOProperty>());

		propertyMap.get(a).get(b).add(c);
	}
	
	private String relationLabel(Collection<OBOProperty> ps) {
		StringBuffer sb = new StringBuffer("[");
		for (OBOProperty p : ps) {
			sb.append(" "+relationLabel(p));
		}
		return sb.toString()+" ]";
	}

	private String relationLabel(OBOProperty p) {
		if (p.getName() != null)
			return p.getName();
		return p.getID();
	}

	public String toTable() {
		StringBuffer sb = new StringBuffer();
		for (OBOProperty b : properties) {
			sb.append("\t"+relationLabel(b));
		}
		sb.append("\n");

		for (OBOProperty a : properties) {
			sb.append(relationLabel(a));
			for (OBOProperty b : properties) {
				sb.append("\t"+relationLabel(lookup(a,b)));
			}	
			sb.append("\n");
		}
		return sb.toString();
	}


}
