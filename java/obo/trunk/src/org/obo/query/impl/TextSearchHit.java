package org.obo.query.impl;

import org.obo.datamodel.FieldPath;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOClass;

import org.apache.log4j.*;

public class TextSearchHit implements SearchHit<OBOClass> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TextSearchHit.class);

	protected OBOClass result;

	protected String hitText;

	protected int hitPosition;

	protected TextQuery.HitType hitType;
	
	protected FieldPath path;
	
	public OBOClass getHit() {
		return result;
	}

	public TextSearchHit() {
		
	}
	
	public void setPath(FieldPath path) {
		this.path = path;
	}
	
	public FieldPath getPath() {
		return path;
	}
	
	public TextSearchHit(OBOClass result, String hitText,
			TextQuery.HitType hitType, int hitPosition) {
		this.result = result;
		this.hitText = hitText;
		this.hitPosition = hitPosition;
		this.hitType = hitType;
	}

	public int getHitPosition() {
		return hitPosition;
	}
	
	public OBOClass getResult() {
		return (OBOClass) getHit();
	}

	public String getHitText() {
		return hitText;
	}

	public TextQuery.HitType getHitType() {
		return hitType;
	}

	public String toString() {
		return result.getName();
	}

	public void populateHit(OBOClass obj) {
		this.result = obj;
		this.hitText = obj.getName();
		this.hitPosition = 0;
		this.hitType = TextQuery.HitType.NAME;
	}
}
