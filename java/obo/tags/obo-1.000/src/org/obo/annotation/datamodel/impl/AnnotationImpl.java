package org.obo.annotation.datamodel.impl;

import java.util.Collection;
import java.util.LinkedList;

import org.obo.annotation.datamodel.Annotation;
import org.obo.annotation.datamodel.AnnotationOntology;
import org.obo.datamodel.Datatype;
import org.obo.datamodel.DatatypeValue;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.Instance;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkLinkedObject;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.PropertyValue;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.impl.DatatypeValueImpl;
import org.obo.datamodel.impl.InstanceImpl;
import org.obo.datamodel.impl.InstancePropertyValue;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.history.AddPropertyValueHistoryItem;
import org.obo.history.CompletesHistoryItem;
import org.obo.history.CreateLinkHistoryItem;
import org.obo.history.DeleteLinkHistoryItem;
import org.obo.history.DeletePropertyValueHistoryItem;
import org.obo.history.HistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.util.IDUtil;
import org.obo.util.TermUtil;

public class AnnotationImpl extends InstanceImpl implements Annotation {
	
	public AnnotationImpl(String id) {
		super(id, AnnotationOntology.ANNOTATION());
	}
	
	public AnnotationImpl(Instance instance) {
		super(instance.getID());
		setName(instance.getName());
		setDefinition(instance.getDefinition());
		setComment(instance.getComment());
		for(Dbxref ref : instance.getDbxrefs())
			addDbxref(ref);
		for(Synonym synonym : instance.getSynonyms())
			addSynonym(synonym);
		for(Dbxref ref : instance.getDefDbxrefs())
			addDefDbxref(ref);
		for(PropertyValue pv : instance.getPropertyValues())
			addPropertyValue(pv);
		for(Link link : instance.getParents()) {
			Link newLink = (Link) link.clone();
			newLink.setChild(this);
			addParent(newLink);
		}
	}

	public String getAssignedBy() {
		return (String) TermUtil.getPropValue(this, AnnotationOntology
				.ASSIGNED_BY_REL(), String.class, true);
	}

	public Collection<LinkedObject> getEvidence() {
		return TermUtil.getPropValues(this, AnnotationOntology.EVIDENCE_REL());
	}

	public LinkedObject getObject() {
		LinkLinkedObject lo = (LinkLinkedObject) TermUtil.getPropValue(this,
				AnnotationOntology.POSITS_REL(), LinkedObject.class, true);
		if (lo == null)
			return null;
		Link link = lo.getLink();
		return (OBOClass) link.getParent();
	}

	public OBOProperty getRelationship() {
		LinkLinkedObject lo = (LinkLinkedObject) TermUtil.getPropValue(this,
				AnnotationOntology.POSITS_REL(), LinkedObject.class, true);
		if (lo == null)
			return null;
		Link link = lo.getLink();
		return (OBOProperty) link.getType();
	}

	public Collection<String> getSources() {
		return TermUtil.getPropValues(this, AnnotationOntology.SOURCE_REL());
	}

	public LinkedObject getSubject() {
		LinkLinkedObject lo = (LinkLinkedObject) TermUtil.getPropValue(this,
				AnnotationOntology.POSITS_REL(), LinkedObject.class, true);
		if (lo == null)
			return null;
		Link link = lo.getLink();
		return (LinkedObject) link.getChild();
	}
	
	public boolean getIsNegated() {
		String isNegatedStr = 
			((String)TermUtil.getPropValue(this, AnnotationOntology.IS_NEGATED()));
		if (isNegatedStr != null) {
			return isNegatedStr.equals("true");
		}
		return false;
	}

	public HistoryItem getAssignedByChangeItem(String assignedBy) {
		TermMacroHistoryItem item = new TermMacroHistoryItem();
		String oldAssignedBy = getAssignedBy();
		if (oldAssignedBy != null) {
			HistoryItem delitem = new DeletePropertyValueHistoryItem(getID(),
					AnnotationOntology.ASSIGNED_BY_REL().getID(),
					Datatype.STRING.getID(), oldAssignedBy);
			item.addItem(delitem);
		}
		if (assignedBy != null) {
			HistoryItem additem = new AddPropertyValueHistoryItem(getID(),
					AnnotationOntology.ASSIGNED_BY_REL().getID(),
					Datatype.STRING.getID(), assignedBy);
			item.addItem(additem);
		}
		return item;
	}

	public HistoryItem addEvidenceHistoryItem(LinkedObject evidence) {
		return new CreateLinkHistoryItem(this, AnnotationOntology
				.EVIDENCE_REL(), evidence);
	}

	public HistoryItem removeEvidenceHistoryItem(LinkedObject evidence) {
		return new DeleteLinkHistoryItem(evidence.getID(), getID(),
				AnnotationOntology.EVIDENCE_REL().getID());
	}

	public HistoryItem getObjectChangeItem(LinkedObject object) {
		LinkLinkedObject lo = (LinkLinkedObject) TermUtil.getPropValue(this,
				AnnotationOntology.POSITS_REL(), LinkedObject.class, true);
		Link link = lo.getLink();
		TermMacroHistoryItem item = new TermMacroHistoryItem();
		HistoryItem delitem = new DeletePropertyValueHistoryItem(getID(),
				AnnotationOntology.POSITS_REL().getID(), null, link.getID());
		HistoryItem delLinkItem = new DeleteLinkHistoryItem(link);
		Link newLink = (Link) link.clone();
		newLink.setParent(object);
		HistoryItem copyItem = new CreateLinkHistoryItem(newLink.getChild().getID(), newLink.getType().getID(), newLink.getParent()
						.getID());
		item.addItem(delitem);
		item.addItem(delLinkItem);
		item.addItem(copyItem);
		if (TermUtil.isIntersection(link)) {
			item.addItem(new CompletesHistoryItem(
					(OBORestriction) newLink));
		}
		HistoryItem positsItem = new CreateLinkHistoryItem(getID(),
				AnnotationOntology.POSITS_REL().getID(), newLink.getID());
		item.addItem(positsItem);
		return item;
	}

	public HistoryItem getSubjectChangeItem(LinkedObject subject) {
		LinkLinkedObject lo = (LinkLinkedObject) TermUtil.getPropValue(this,
				AnnotationOntology.POSITS_REL(), LinkedObject.class, true);
		Link link = lo.getLink();
		TermMacroHistoryItem item = new TermMacroHistoryItem();
		HistoryItem delitem = new DeletePropertyValueHistoryItem(getID(),
				AnnotationOntology.POSITS_REL().getID(), null, link.getID());
		HistoryItem delLinkItem = new DeleteLinkHistoryItem(link);
		Link newLink = (Link) link.clone();
		newLink.setChild(subject);
		HistoryItem copyItem = new CreateLinkHistoryItem(newLink.getChild().getID(), newLink.getType().getID(), newLink.getParent()
						.getID());
		item.addItem(delitem);
		item.addItem(delLinkItem);
		item.addItem(copyItem);
		if (TermUtil.isIntersection(link)) {
			item.addItem(new CompletesHistoryItem(
					(OBORestriction) newLink));
		}
		HistoryItem positsItem = new CreateLinkHistoryItem(getID(),
				AnnotationOntology.POSITS_REL().getID(), newLink.getID());
		item.addItem(positsItem);
		return item;
	}

	public void setObject(LinkedObject object) {
		LinkLinkedObject lo = (LinkLinkedObject) TermUtil.getPropValue(this,
				AnnotationOntology.POSITS_REL(), LinkedObject.class, true);

		Link newLink = null;
		if (lo != null) {
			Link link = lo.getLink();
			newLink = (Link) link.clone();
			newLink.setParent(object);
			removePropertyValue(AnnotationOntology.POSITS_REL(), lo);
			if (link.getChild() != null)
				link.getChild().removeParent(link);
		} else {
			newLink = new OBORestrictionImpl((LinkedObject) null,
					(OBOProperty) null, (LinkedObject) object);
		}
		lo = new LinkLinkedObject(newLink);
		addPropertyValue(AnnotationOntology.POSITS_REL(), lo);
		if (newLink.getChild() != null)
			newLink.getChild().addParent(newLink);
	}

	public HistoryItem getRelationshipChangeItem(OBOProperty relationship) {
		LinkLinkedObject lo = (LinkLinkedObject) TermUtil.getPropValue(this,
				AnnotationOntology.POSITS_REL(), LinkedObject.class, true);
		Link link = lo.getLink();
		TermMacroHistoryItem item = new TermMacroHistoryItem();
		HistoryItem delitem = new DeletePropertyValueHistoryItem(getID(),
				AnnotationOntology.POSITS_REL().getID(), null, link.getID());
		HistoryItem delLinkItem = new DeleteLinkHistoryItem(link);
		Link newLink = (Link) link.clone();
		newLink.setType(relationship);
		HistoryItem copyItem = new CreateLinkHistoryItem(newLink.getChild().getID(), newLink.getType().getID(), newLink.getParent()
						.getID());
		item.addItem(delitem);
		item.addItem(delLinkItem);
		item.addItem(copyItem);
		if (TermUtil.isIntersection(link)) {
			item.addItem(new CompletesHistoryItem(
					(OBORestriction) newLink));
		}
		HistoryItem positsItem = new CreateLinkHistoryItem(getID(),
				AnnotationOntology.POSITS_REL().getID(), newLink.getID());
		item.addItem(positsItem);
		return item;
	}

	public HistoryItem addSourceChangeItem(String source) {
		return new AddPropertyValueHistoryItem(getID(), AnnotationOntology
				.SOURCE_REL().getID(), Datatype.STRING.getID(), source);
	}

	public HistoryItem removeSourceChangeItem(String source) {
		return new DeletePropertyValueHistoryItem(getID(), AnnotationOntology
				.SOURCE_REL().getID(), Datatype.STRING.getID(), source);
	}

	public void setAssignedBy(String assignedBy) {
		for (Link link : getParents()) {
			if (link.getType().equals(AnnotationOntology.ASSIGNED_BY_REL())) {
				removeParent(link);
				break;
			}
		}
		if (assignedBy != null) {
			Link link = new InstancePropertyValue(this, AnnotationOntology
					.ASSIGNED_BY_REL(), new DatatypeValueImpl(Datatype.STRING,
					assignedBy));
			addParent(link);
		}
	}

	public void addEvidence(LinkedObject evidence) {
		Link link = new InstancePropertyValue(this, AnnotationOntology
				.EVIDENCE_REL(), evidence);
		addParent(link);
	}

	public void removeEvidence(LinkedObject evidence) {
		Link link = new InstancePropertyValue(this, AnnotationOntology
				.EVIDENCE_REL(), evidence);
		removeParent(link);
	}

	public void addSource(String source) {
		addPropertyValue(AnnotationOntology.SOURCE_REL(),
				new DatatypeValueImpl(Datatype.STRING, source));
	}

	public void removeSource(String source) {
		removePropertyValue(AnnotationOntology.SOURCE_REL(),
				new DatatypeValueImpl(Datatype.STRING, source));
	}

	public void setRelationship(OBOProperty relationship) {
		LinkLinkedObject lo = (LinkLinkedObject) TermUtil.getPropValue(this,
				AnnotationOntology.POSITS_REL(), LinkedObject.class, true);
		Link newLink = null;
		if (lo != null) {
			Link link = lo.getLink();
			newLink = (Link) link.clone();
			newLink.setType(relationship);
			removePropertyValue(AnnotationOntology.POSITS_REL(), lo);
			if (link.getChild() != null)
				link.getChild().removeParent(link);
		} else {
			newLink = new OBORestrictionImpl((LinkedObject) null,
					(OBOProperty) relationship, (LinkedObject) null);
		}
		lo = new LinkLinkedObject(newLink);
		addPropertyValue(AnnotationOntology.POSITS_REL(), lo);
		if (newLink.getChild() != null) {
			newLink.getChild().addParent(newLink);
		}
	}

	public void setSubject(LinkedObject subject) {
		LinkLinkedObject lo = (LinkLinkedObject) TermUtil.getPropValue(this,
				AnnotationOntology.POSITS_REL(), LinkedObject.class, true);
		Link newLink = null;
		if (lo != null) {
			Link link = lo.getLink();
			newLink = (Link) link.clone();
			newLink.setChild(subject);
			removePropertyValue(AnnotationOntology.POSITS_REL(), lo);
			if (link.getChild() != null)
				link.getChild().removeParent(link);
		} else {
			newLink = new OBORestrictionImpl((LinkedObject) subject,
					(OBOProperty) null, (LinkedObject) null);
		}
		lo = new LinkLinkedObject(newLink);
		addPropertyValue(AnnotationOntology.POSITS_REL(), lo);
		if (newLink.getChild() != null) {
			newLink.getChild().addParent(newLink);
		}
	}

	public void setIsNegated(boolean isNegated) {
		for (Link link : getParents()) {
			if (link.getType().equals(AnnotationOntology.IS_NEGATED())) {
				removeParent(link);
				break;
			}
		}
		if (isNegated) {
			Link link = new InstancePropertyValue(this, AnnotationOntology
					.IS_NEGATED(), new DatatypeValueImpl(Datatype.BOOLEAN,
					"true"));

			addParent(link);
		}
	}

}
