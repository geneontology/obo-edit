package org.obo.util;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import org.apache.commons.codec.language.Soundex;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.OBOSession;

/**
 * Engine to perform lexical checks; e.g. univocity checks
 * @author cjm
 *
 */
public class LexUtil extends SessionWrapper {

	protected TextComparisonAlgorithm algorithm = TextComparisonAlgorithm.EXACT;

	public enum TextComparisonAlgorithm { EXACT, SOUNDEX, STEM; }

	public LexUtil() {
		super();
	}
	public LexUtil(OBOSession session) {
		super();
		this.session = session;
	}

	public LexUtil(TextComparisonAlgorithm algorithm) {
		super();
		this.algorithm = algorithm;
	}

	public  Map<String,Collection<OBOObject>> getDefinitionToObjectMap() {
		Map<String,Collection<OBOObject>> map = new HashMap<String,Collection<OBOObject>>();
		Soundex soundex = new Soundex();
		for (OBOObject obj : TermUtil.getOBOObjects(getSession())) {
			String def = obj.getDefinition();
			if (def != null && !def.equals("")) {
				String key = def;
				// TODO: we have to stem first!
				if (algorithm.equals(TextComparisonAlgorithm.SOUNDEX))
					key = soundex.soundex(def);
				if (!map.containsKey(key))
					map.put(key, new LinkedList<OBOObject>());

				map.get(key).add(obj);
			}
		}
		return map;
	}

	public void findTermSetsWithSimilarDefinitions() {
		Map<String, Collection<OBOObject>> map = 
			getDefinitionToObjectMap();
		for (String key : map.keySet()) {
			Collection<OBOObject> objs = map.get(key);
			if (objs.size() > 1) {
				System.out.println("TermSet: "+key);
				for (OBOObject obj : objs) {
					System.out.println("  Term: "+obj+" "+obj.getDefinition());
				}
			}
		}
	}
}
