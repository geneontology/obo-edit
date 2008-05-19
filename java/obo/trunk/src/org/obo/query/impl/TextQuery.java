package org.obo.query.impl;

import java.awt.font.TextHitInfo;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.LinkedList;

import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.Synonym;
import org.obo.query.Query;
import org.obo.query.StringQuery;

import org.apache.log4j.*;

public class TextQuery implements
	StringQuery<OBOClass, TextSearchHit> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TextQuery.class);

	public static enum HitType {
		NAME, SYNONYM, DEFINITION, ID, COMMENT;
	};

	protected Comparator<TextSearchHit> comparator = new Comparator<TextSearchHit>() {
		public int compare(TextSearchHit o1,
				TextSearchHit o2) {
			if ((o1 instanceof TextSearchHit) && (o2 instanceof TextSearchHit)) {
				TextSearchHit s1 = (TextSearchHit) o1;
				TextSearchHit s2 = (TextSearchHit) o2;
				int rank1 = getRank(s1.getHitType());
				int rank2 = getRank(s2.getHitType());

				// put lower ranked hits first
				if (rank1 - rank2 != 0)
					return rank1 - rank2;

				// if ranks are the same, put beginning-of-string hits first
				if (s1.getHitPosition() != s2.getHitPosition()
						&& (s1.getHitPosition() == 0 || s2.getHitPosition() == 0)) {
					if (s1.getHitPosition() == 0)
						return -1;
					else
						return 1;
				}
			}

			// otherwise, sort by term name
			if (ignoreCase)
				return o1.getHit().getName().compareToIgnoreCase(
						o2.getHit().getName());
			else
				return o1.getHit().getName().compareTo(o2.getHit().getName());
		}

	};

	protected String searchString;

	protected String lowercaseString;

	protected Collection<HitType> hitTypes = new LinkedList<HitType>();

	protected boolean ignoreCase = true;

	public TextQuery() {
	}

	protected int getRank(HitType type) {
		if (type == HitType.NAME)
			return 1;
		if (type == HitType.ID)
			return 2;
		if (type == HitType.SYNONYM)
			return 3;
		if (type == HitType.DEFINITION)
			return 4;
		if (type == HitType.COMMENT)
			return 5;
		throw new IllegalArgumentException("Unexpected hittype");
	}

	public void setSearchTypes(Collection<HitType> types) {
		this.hitTypes = types;
	}

	public void setIgnoreCase(boolean ignoreCase) {
		this.ignoreCase = ignoreCase;
	}

	/**
	 * Sets the search fields. Note that this uses Java 1.5 varargs, so you can
	 * specify as many arguments as you want, and Java will automatically box
	 * them into an array. If you'd rather pass a pre-built collection, call
	 * {@link #setSearchTypes(Collection)}
	 * 
	 * @param types
	 */
	public void setSearchTypes(HitType... types) {
		hitTypes.clear();
		for (HitType t : types)
			hitTypes.add(t);
	}

	public void setSearchString(String searchString) {
		this.searchString = searchString;
		this.lowercaseString = searchString.toLowerCase();
	}

	public OBOClass convertToInputType(TextSearchHit original) {
		return original.result;
	}

	public Collection<TextSearchHit> createResultHolder() {
		return new ArrayList<TextSearchHit>();
	}

	public Comparator<TextSearchHit> getComparator() {
		return comparator;
	}

	public Class<IdentifiedObject> getInputType() {
		return IdentifiedObject.class;
	}

	protected String getString(String s) {
		if (ignoreCase)
			return s.toLowerCase();
		else
			return s;
	}

	public String getSearchString() {
		if (ignoreCase)
			return lowercaseString;
		else
			return searchString;
	}

	public TextSearchHit matches(OBOClass a) {
		if (hitTypes.contains(HitType.NAME)) {
			int pos = getString(a.getName()).indexOf(getSearchString());
			if (pos >= 0)
				return new TextSearchHit(a, a.getName(), HitType.NAME, pos);
		}

		if (hitTypes.contains(HitType.DEFINITION)) {
			int pos = getString(a.getDefinition()).indexOf(getSearchString());
			if (pos >= 0)
				return new TextSearchHit(a, a.getDefinition(),
						HitType.DEFINITION, pos);
		}

		if (hitTypes.contains(HitType.COMMENT)) {
			int pos = getString(a.getComment()).indexOf(getSearchString());
			if (pos >= 0)
				return new TextSearchHit(a, a.getComment(), HitType.COMMENT,
						pos);
		}

		if (hitTypes.contains(HitType.ID)) {
			int pos = getString(a.getID()).indexOf(getSearchString());
			if (pos >= 0)
				return new TextSearchHit(a, a.getID(), HitType.ID, pos);
		}

		if (hitTypes.contains(HitType.SYNONYM)) {
			for (Synonym s : a.getSynonyms()) {
				int pos = getString(s.getText()).indexOf(getSearchString());
				if (pos >= 0)
					return new TextSearchHit(a, s.getText(), HitType.SYNONYM,
							pos);
			}
		}

		return null;
	}

	public IdentifiedObject convertToInputType(SearchHit<IdentifiedObject> original) {
		return original.getHit();
	}

	public TextSearchHit convertToOutputType(OBOClass a) {
		return new TextSearchHit(a, a.getName(), HitType.NAME, 0);
	}

	public Collection<FieldPathSpec> getInputPaths() {
		return null;
	}

	public void setFieldPath(FieldPath path) {
	}

}
