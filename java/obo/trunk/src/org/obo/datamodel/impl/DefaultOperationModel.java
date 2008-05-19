package org.obo.datamodel.impl;

import java.util.*;

import org.obo.datamodel.*;
import org.obo.history.*;
import org.obo.util.HistoryUtil;

import org.apache.log4j.*;

public class DefaultOperationModel implements OperationModel {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefaultOperationModel.class);

	protected OBOSession session;
	protected Collection<OperationModel> lockstepModels = new LinkedList<OperationModel>();
	protected String currentUser;

	public void setSession(OBOSession session) {
		this.session = session;
	}

	public OperationWarning apply(HistoryItem item) {
		OperationWarning warning;
		if (item instanceof CreateObjectHistoryItem)
			warning = apply((CreateObjectHistoryItem) item);
		else if (item instanceof CreateLinkHistoryItem)
			warning = apply((CreateLinkHistoryItem) item);
		else if (item instanceof DeleteLinkHistoryItem)
			warning = apply((DeleteLinkHistoryItem) item);
		else if (item instanceof DestroyObjectHistoryItem)
			warning = apply((DestroyObjectHistoryItem) item);
		else if (item instanceof ObsoleteObjectHistoryItem)
			warning = apply((ObsoleteObjectHistoryItem) item);
		else if (item instanceof TermMacroHistoryItem)
			warning = apply((TermMacroHistoryItem) item);
		else if (item instanceof LinkTypeHistoryItem)
			warning = apply((LinkTypeHistoryItem) item);
		else if (item instanceof NamespaceHistoryItem)
			warning = apply((NamespaceHistoryItem) item);
		else if (item instanceof NameChangeHistoryItem)
			warning = apply((NameChangeHistoryItem) item);
		else if (item instanceof DefinitionChangeHistoryItem)
			warning = apply((DefinitionChangeHistoryItem) item);
		else if (item instanceof TermCategoryHistoryItem)
			warning = apply((TermCategoryHistoryItem) item);
		else if (item instanceof SynonymCategoryHistoryItem)
			warning = apply((SynonymCategoryHistoryItem) item);
		else if (item instanceof ChangeSynCategoryHistoryItem)
			warning = apply((ChangeSynCategoryHistoryItem) item);
		else if (item instanceof ChangeSynScopeHistoryItem)
			warning = apply((ChangeSynScopeHistoryItem) item);
		else if (item instanceof TermNamespaceHistoryItem)
			warning = apply((TermNamespaceHistoryItem) item);
		else if (item instanceof AddDbxrefHistoryItem)
			warning = apply((AddDbxrefHistoryItem) item);
		else if (item instanceof DelDbxrefHistoryItem)
			warning = apply((DelDbxrefHistoryItem) item);
		else if (item instanceof AddSynonymHistoryItem)
			warning = apply((AddSynonymHistoryItem) item);
		else if (item instanceof DelSynonymHistoryItem)
			warning = apply((DelSynonymHistoryItem) item);
		else if (item instanceof CommentChangeHistoryItem)
			warning = apply((CommentChangeHistoryItem) item);
		else if (item instanceof CategoryChangeHistoryItem)
			warning = apply((CategoryChangeHistoryItem) item);
		else if (item instanceof SecondaryIDHistoryItem)
			warning = apply((SecondaryIDHistoryItem) item);
		else if (item instanceof NecessarilyTrueHistoryItem)
			warning = apply((NecessarilyTrueHistoryItem) item);
		else if (item instanceof InverseNecHistoryItem)
			warning = apply((InverseNecHistoryItem) item);
		else if (item instanceof CompletesHistoryItem)
			warning = apply((CompletesHistoryItem) item);
		else if (item instanceof RangeHistoryItem)
			warning = apply((RangeHistoryItem) item);
		else if (item instanceof DomainHistoryItem)
			warning = apply((DomainHistoryItem) item);
		else if (item instanceof AddReplacementHistoryItem)
			warning = apply((AddReplacementHistoryItem) item);
		else if (item instanceof RemoveReplacementHistoryItem)
			warning = apply((RemoveReplacementHistoryItem) item);
		else if (item instanceof CyclicHistoryItem)
			warning = apply((CyclicHistoryItem) item);
		else if (item instanceof CardinalityHistoryItem)
			warning = apply((CardinalityHistoryItem) item);
		else if (item instanceof SymmetricHistoryItem)
			warning = apply((SymmetricHistoryItem) item);
		else if (item instanceof TransitiveHistoryItem)
			warning = apply((TransitiveHistoryItem) item);
		else if (item instanceof TRNamespaceHistoryItem)
			warning = apply((TRNamespaceHistoryItem) item);
		else if (item instanceof CardinalityHistoryItem)
			warning = apply((CardinalityHistoryItem) item);
		else if (item instanceof MinCardinalityHistoryItem)
			warning = apply((MinCardinalityHistoryItem) item);
		else if (item instanceof MaxCardinalityHistoryItem)
			warning = apply((MaxCardinalityHistoryItem) item);
		else if (item instanceof AddConsiderHistoryItem)
			warning = apply((AddConsiderHistoryItem) item);
		else if (item instanceof RemoveConsiderHistoryItem)
			warning = apply((RemoveConsiderHistoryItem) item);
		else if (item instanceof AddPropertyValueHistoryItem)
			warning = apply((AddPropertyValueHistoryItem) item);
		else if (item instanceof DeletePropertyValueHistoryItem)
			warning = apply((DeletePropertyValueHistoryItem) item);
		else
			warning = new OperationWarning("Unknown history item " + item
					+ " found of type " + item.getClass() + "!");
		for (OperationModel lockstepModel : lockstepModels) {
			OperationWarning lwarning = lockstepModel.apply(item);
			if (lwarning != null)
				warning.addWarning(lwarning);
		}
		if (item.getTarget() != null) {
			IdentifiedObject io = getRealIDObject(item.getTarget());
			if (io instanceof ModificationMetadataObject) {
				((ModificationMetadataObject) io)
						.setModificationDate(new Date());
				if (session.getCurrentUser() != null) {
					((ModificationMetadataObject) io).setModifiedBy(session
							.getCurrentUser());
				}
			}
		}
		return warning;
	}

	public OperationWarning apply(AddPropertyValueHistoryItem item) {
		IdentifiedObject target = getRealObject(item.getTarget());
		if (target == null)
			return new OperationWarning("Could not add property value to non-"
					+ "existant object " + item.getTarget());

		if (!(target instanceof Instance))
			return new OperationWarning("Cannot add property value to "
					+ "non-instance " + target);

		Instance instance = (Instance) target;

		IdentifiedObject property = getRealObject(item.getPropertyID());
		if (property == null)
			return new OperationWarning("Cannot add property value with "
					+ "non-existant property " + item.getPropertyID());
		if (!(property instanceof OBOProperty))
			return new OperationWarning("Cannot add property value with "
					+ "non-property " + item.getPropertyID());

		OBOProperty oboProperty = (OBOProperty) property;

		Value value = null;
		if (item.getDatatypeID() == null) {
			value = getRealObject(item.getValue());
		} else {
			Datatype type = null;
			for (int i = 0; i < Datatype.DATATYPES.length; i++)
				if (Datatype.DATATYPES[i].getID().equals(item.getDatatypeID())) {
					type = Datatype.DATATYPES[i];
					break;
				}
			if (type == null)
				return new OperationWarning("Cannot use unrecognized datatype"
						+ " " + item.getDatatypeID());
			try {
				value = new DatatypeValueImpl(type, item.getValue());
			} catch (Exception ex) {
				return new OperationWarning("Value " + value
						+ " is not a legal " + "value for datatype " + type);
			}
		}
		instance.addPropertyValue(oboProperty, value);
		return null;
	}

	public OperationWarning reverse(AddPropertyValueHistoryItem item) {
		IdentifiedObject target = getRealObject(item.getTarget());
		if (target == null)
			return new OperationWarning(
					"Could not undo add property value with non-"
							+ "existant object " + item.getTarget());

		if (!(target instanceof Instance))
			return new OperationWarning("Cannot not undo add property value "
					+ "to " + "non-instance " + target);

		Instance instance = (Instance) target;

		IdentifiedObject property = getRealObject(item.getPropertyID());
		if (property == null)
			return new OperationWarning("Cannot undo add property value to "
					+ "non-existant property " + item.getPropertyID());
		if (!(property instanceof OBOProperty))
			return new OperationWarning("Cannot undo add property value to "
					+ "non-property " + item.getPropertyID());

		OBOProperty oboProperty = (OBOProperty) property;

		Value value = null;
		if (item.getDatatypeID() == null) {
			value = getRealObject(item.getValue());
		} else {
			Datatype type = null;
			for (int i = 0; i < Datatype.DATATYPES.length; i++)
				if (Datatype.DATATYPES[i].getID().equals(item.getDatatypeID())) {
					type = Datatype.DATATYPES[i];
					break;
				}
			if (type == null)
				return new OperationWarning("Cannot use unrecognized datatype"
						+ " " + item.getDatatypeID());
			try {
				value = new DatatypeValueImpl(type, item.getValue());
			} catch (Exception ex) {
				return new OperationWarning("Value " + value
						+ " is not a legal " + "value for datatype " + type);
			}
		}
		instance.removePropertyValue(oboProperty, value);
		return null;
	}

	public OperationWarning apply(DeletePropertyValueHistoryItem item) {
		IdentifiedObject target = getRealObject(item.getTarget());
		if (target == null)
			return new OperationWarning(
					"Could not delete property value from non-"
							+ "existant object " + item.getTarget());

		if (!(target instanceof Instance))
			return new OperationWarning("Cannot delete property value from "
					+ "non-instance " + target);

		Instance instance = (Instance) target;

		IdentifiedObject property = getRealObject(item.getPropertyID());
		if (property == null)
			return new OperationWarning("Cannot delete property value from "
					+ "non-existant property " + item.getPropertyID());
		if (!(property instanceof OBOProperty))
			return new OperationWarning("Cannot delete property value with "
					+ "non-property " + item.getPropertyID());

		OBOProperty oboProperty = (OBOProperty) property;

		Value value = null;
		if (item.getDatatypeID() == null) {
			value = getRealObject(item.getValue());
		} else {
			Datatype type = null;
			for (int i = 0; i < Datatype.DATATYPES.length; i++)
				if (Datatype.DATATYPES[i].getID().equals(item.getDatatypeID())) {
					type = Datatype.DATATYPES[i];
					break;
				}
			if (type == null)
				return new OperationWarning("Cannot use unrecognized datatype"
						+ " " + item.getDatatypeID());
			try {
				value = new DatatypeValueImpl(type, item.getValue());
			} catch (Exception ex) {
				return new OperationWarning("Value " + value
						+ " is not a legal " + "value for datatype " + type);
			}
		}
		instance.removePropertyValue(oboProperty, value);
		return null;
	}

	public OperationWarning reverse(DeletePropertyValueHistoryItem item) {
		IdentifiedObject target = getRealObject(item.getTarget());
		if (target == null)
			return new OperationWarning(
					"Could not undo delete property value from "
							+ "non-existant object " + item.getTarget());

		if (!(target instanceof Instance))
			return new OperationWarning("Cannot undo delete property value "
					+ "from " + "non-instance " + target);

		Instance instance = (Instance) target;

		IdentifiedObject property = getRealObject(item.getPropertyID());
		if (property == null)
			return new OperationWarning("Cannot undo delete property value "
					+ "from " + "non-existant property " + item.getPropertyID());
		if (!(property instanceof OBOProperty))
			return new OperationWarning("Cannot undo delete property value "
					+ "from " + "non-property " + item.getPropertyID());

		OBOProperty oboProperty = (OBOProperty) property;

		Value value = null;
		if (item.getDatatypeID() == null) {
			value = getRealObject(item.getValue());
		} else {
			Datatype type = null;
			for (int i = 0; i < Datatype.DATATYPES.length; i++)
				if (Datatype.DATATYPES[i].getID().equals(item.getDatatypeID())) {
					type = Datatype.DATATYPES[i];
					break;
				}
			if (type == null)
				return new OperationWarning("Cannot use unrecognized datatype"
						+ " " + item.getDatatypeID());
			try {
				value = new DatatypeValueImpl(type, item.getValue());
			} catch (Exception ex) {
				return new OperationWarning("Value " + value
						+ " is not a legal " + "value for datatype " + type);
			}
		}
		instance.addPropertyValue(oboProperty, value);
		return null;
	}

	public OperationWarning apply(AddDbxrefHistoryItem item) {
		IdentifiedObject target = getRealObject(item.getTarget());
		if (target == null)
			return new OperationWarning("Could not add dbxref to non-"
					+ "existant object " + item.getTarget());
		if (item.isDef()) {
			if (!(target instanceof DefinedObject))
				return new OperationWarning("Could not add def dbxref to non-"
						+ "defined object " + item.getTarget());
			((DefinedObject) target).addDefDbxref(item.getDbxref());
		} else if (item.getSynonym() != null) {
			if (!(target instanceof SynonymedObject))
				return new OperationWarning("Could not add synonym dbxref "
						+ "to non-synonymed object " + item.getTarget());
			Synonym s = HistoryUtil.findSynonym((SynonymedObject) target, item
					.getSynonym());
			if (s == null)
				return new OperationWarning("Could not modify non-existant "
						+ "synonym " + s + " of " + target);
			s.addDbxref(item.getDbxref());
		} else {
			if (!(target instanceof DbxrefedObject))
				return new OperationWarning("Could not add dbxref to "
						+ "non-dbxrefed object " + item.getTarget());
			((DbxrefedObject) target).addDbxref(item.getDbxref());
		}
		return null;
	}

	public OperationWarning reverse(AddDbxrefHistoryItem item) {
		IdentifiedObject target = getRealObject(item.getTarget());
		if (target == null)
			return new OperationWarning("Could not remove dbxref from non-"
					+ "existant object " + item.getTarget());
		if (item.isDef()) {
			if (!(target instanceof DefinedObject))
				return new OperationWarning("Could not remove def dbxref "
						+ "from non-" + "defined object " + item.getTarget());
			((DefinedObject) target).removeDefDbxref(item.getDbxref());
		} else if (item.getSynonym() != null) {
			if (!(target instanceof SynonymedObject))
				return new OperationWarning("Could not remove synonym dbxref "
						+ "from non-synonymed object " + item.getTarget());
			Synonym s = HistoryUtil.findSynonym((SynonymedObject) target, item
					.getSynonym());
			s.removeDbxref(item.getDbxref());
		} else {
			if (!(target instanceof DbxrefedObject))
				return new OperationWarning("Could not remove dbxref from "
						+ "non-dbxrefed object " + item.getTarget());
			((DbxrefedObject) target).removeDbxref(item.getDbxref());
		}
		return null;
	}

	public OperationWarning reverse(DelDbxrefHistoryItem item) {
		IdentifiedObject target = getRealObject(item.getTarget());
		if (target == null)
			return new OperationWarning("Could not add dbxref to non-"
					+ "existant object " + item.getTarget());
		if (item.isDef()) {
			if (!(target instanceof DefinedObject))
				return new OperationWarning("Could not add def dbxref to non-"
						+ "defined object " + item.getTarget());
			((DefinedObject) target).addDefDbxref(item.getDbxref());
		} else if (item.getSynonym() != null) {
			if (!(target instanceof SynonymedObject))
				return new OperationWarning("Could not add synonym dbxref "
						+ "to non-synonymed object " + item.getTarget());
			Synonym s = HistoryUtil.findSynonym((SynonymedObject) target, item
					.getSynonym());
			s.addDbxref(item.getDbxref());
		} else {
			if (!(target instanceof DbxrefedObject))
				return new OperationWarning("Could not add dbxref to "
						+ "non-dbxrefed object " + item.getTarget());
			((DbxrefedObject) target).addDbxref(item.getDbxref());
		}
		return null;
	}

	public OperationWarning apply(DelDbxrefHistoryItem item) {
		IdentifiedObject target = getRealObject(item.getTarget());
		if (target == null)
			return new OperationWarning("Could not remove dbxref from non-"
					+ "existant object " + item.getTarget());
		if (item.isDef()) {
			if (!(target instanceof DefinedObject))
				return new OperationWarning("Could not remove def dbxref "
						+ "from non-" + "defined object " + item.getTarget());
			((DefinedObject) target).removeDefDbxref(item.getDbxref());
		} else if (item.getSynonym() != null) {
			if (!(target instanceof SynonymedObject))
				return new OperationWarning("Could not remove synonym dbxref "
						+ "from non-synonymed object " + item.getTarget());
			Synonym s = HistoryUtil.findSynonym((SynonymedObject) target, item
					.getSynonym());
			s.removeDbxref(item.getDbxref());
		} else {
			if (!(target instanceof DbxrefedObject))
				return new OperationWarning("Could not remove dbxref from "
						+ "non-dbxrefed object " + item.getTarget());
			((DbxrefedObject) target).removeDbxref(item.getDbxref());
		}
		return null;
	}

	public OperationWarning apply(TRNamespaceHistoryItem item) {
		StringRelationship sr = item.getRel();

		Link tr = getRealRel(sr);
		LinkedObject parent = tr.getParent();

		if (parent == null) {
			return new OperationWarning("Couldn't do "
					+ "relationship namespace " + "change with missing "
					+ "parent " + sr.getParent());
		}

		if (tr.getChild() == null) {
			return new OperationWarning("Couldn't do "
					+ "relationship namespace " + "change with missing "
					+ "child " + sr.getChild());
		}

		if (tr.getType() == null) {
			return new OperationWarning("Couldn't complete "
					+ "relationship namespace " + "change with missing "
					+ "relationship type " + sr.getType());
		}
		tr = HistoryUtil.findChildRel(tr, tr.getParent());
		tr.setNamespace(session.getNamespace(item.getNewNamespace()));
		return null;
	}

	public OperationWarning reverse(TRNamespaceHistoryItem item) {
		StringRelationship sr = item.getRel();

		Link tr = getRealRel(sr);
		LinkedObject parent = tr.getParent();

		if (parent == null) {
			return new OperationWarning("Couldn't reverse "
					+ "relationship namespace " + "change with missing "
					+ "parent " + sr.getParent());
		}

		if (tr.getChild() == null) {
			return new OperationWarning("Couldn't reverse "
					+ "relationship namespace " + "change with missing "
					+ "child " + sr.getChild());
		}

		if (tr.getType() == null) {
			return new OperationWarning("Couldn't reverse "
					+ "relationship namespace " + "change with missing "
					+ "relationship type " + sr.getType());
		}

		tr = HistoryUtil.findChildRel(tr, tr.getParent());
		tr.setNamespace(session.getNamespace(item.getOldNamespace()));

		return null;
	}

	public OperationWarning apply(CyclicHistoryItem item) {
		LinkedObject target = getRealObject(item.getTarget());
		if (target == null)
			return new OperationWarning(
					"Could not change cyclic quality of non-"
							+ "existant term " + item.getTarget());
		else if (!(target instanceof OBOProperty))
			return new OperationWarning(
					"Could not change cyclic quality of non-" + "type term "
							+ item.getTarget());
		OBOProperty type = (OBOProperty) target;
		type.setCyclic(!item.getOldCyclic());
		return null;
	}

	public OperationWarning reverse(CyclicHistoryItem item) {
		LinkedObject target = getRealObject(item.getTarget());
		if (target == null)
			return new OperationWarning(
					"Could not change cyclic quality of non-"
							+ "existant term " + item.getTarget());
		else if (!(target instanceof OBOProperty))
			return new OperationWarning(
					"Could not change cyclic quality of non-" + "type term "
							+ item.getTarget());
		OBOProperty type = (OBOProperty) target;
		type.setCyclic(item.getOldCyclic());
		return null;
	}

	public OperationWarning apply(SymmetricHistoryItem item) {
		LinkedObject target = getRealObject(item.getTarget());
		if (target == null)
			return new OperationWarning(
					"Could not change symmetric quality of non-"
							+ "existant term " + item.getTarget());
		else if (!(target instanceof OBOProperty))
			return new OperationWarning(
					"Could not change symmetric quality of non-" + "type term "
							+ item.getTarget());
		OBOProperty type = (OBOProperty) target;
		type.setSymmetric(!item.getOldSymmetric());
		return null;
	}

	public OperationWarning reverse(SymmetricHistoryItem item) {
		LinkedObject target = getRealObject(item.getTarget());
		if (target == null)
			return new OperationWarning(
					"Could not change symmetric quality of non-"
							+ "existant term " + item.getTarget());
		else if (!(target instanceof OBOProperty))
			return new OperationWarning(
					"Could not change symmetric quality of non-" + "type term "
							+ item.getTarget());
		OBOProperty type = (OBOProperty) target;
		type.setSymmetric(item.getOldSymmetric());
		return null;
	}

	public OperationWarning apply(TransitiveHistoryItem item) {
		LinkedObject target = getRealObject(item.getTarget());
		if (target == null)
			return new OperationWarning(
					"Could not change transitive quality of non-"
							+ "existant term " + item.getTarget());
		else if (!(target instanceof OBOProperty))
			return new OperationWarning(
					"Could not change transitive quality of non-"
							+ "type term " + item.getTarget());
		OBOProperty type = (OBOProperty) target;
		type.setTransitive(!item.getOldTransitive());
		return null;
	}

	public OperationWarning reverse(TransitiveHistoryItem item) {
		LinkedObject target = getRealObject(item.getTarget());
		if (target == null)
			return new OperationWarning(
					"Could not change transitive quality of non-"
							+ "existant term " + item.getTarget());
		else if (!(target instanceof OBOProperty))
			return new OperationWarning(
					"Could not change transitive quality of non-"
							+ "type term " + item.getTarget());
		OBOProperty type = (OBOProperty) target;
		type.setTransitive(item.getOldTransitive());
		return null;
	}

	public OperationWarning apply(TermCategoryHistoryItem item) {
		if (item.isAdd())
			session.addCategory(item.getNewCategory());
		else if (item.isDel()) {
			session.removeCategory(item.getOldCategory());
		} else {
			Iterator it = session.getCategories().iterator();
			while (it.hasNext()) {
				TermCategory cat = (TermCategory) it.next();
				if (cat.equals(item.getOldCategory())) {
					cat.setName(item.getNewCategory().getName());
					cat.setDesc(item.getNewCategory().getDesc());
				}
			}
		}
		return null;
	}

	public OperationWarning reverse(TermCategoryHistoryItem item) {
		if (item.isAdd())
			session.removeCategory(item.getNewCategory());
		else if (item.isDel()) {
			session.addCategory(item.getOldCategory());
		} else {
			Iterator it = session.getCategories().iterator();
			while (it.hasNext()) {
				TermCategory cat = (TermCategory) it.next();
				if (cat.equals(item.getNewCategory())) {
					cat.setName(item.getOldCategory().getName());
					cat.setDesc(item.getOldCategory().getDesc());
				}
			}
		}
		return null;
	}

	public OperationWarning apply(SynonymCategoryHistoryItem item) {
		if (item.isAdd())
			session.addSynonymCategory(item.getNewCategory());
		else if (item.isDel()) {
			session.removeSynonymCategory(item.getOldCategory());
		} else {
			Iterator it = session.getSynonymCategories().iterator();
			while (it.hasNext()) {
				SynonymCategory cat = (SynonymCategory) it.next();
				if (cat.equals(item.getOldCategory())) {
					cat.setID(item.getNewCategory().getID());
					cat.setName(item.getNewCategory().getName());
					cat.setScope(item.getNewCategory().getScope());
				}
			}
		}
		return null;
	}

	public OperationWarning reverse(SynonymCategoryHistoryItem item) {
		if (item.isAdd())
			session.removeSynonymCategory(item.getNewCategory());
		else if (item.isDel()) {
			session.addSynonymCategory(item.getOldCategory());
		} else {
			Iterator it = session.getSynonymCategories().iterator();
			while (it.hasNext()) {
				SynonymCategory cat = (SynonymCategory) it.next();
				if (cat.equals(item.getNewCategory())) {
					cat.setID(item.getOldCategory().getID());
					cat.setName(item.getOldCategory().getName());
					cat.setScope(item.getOldCategory().getScope());
				}
			}
		}
		return null;
	}

	public OperationWarning apply(TermNamespaceHistoryItem item) {
		if (item.isAdd())
			session.addNamespace(session.getObjectFactory().createNamespace(
					item.getNewID(), null));
		else if (item.isDel()) {
			session.removeNamespace(session.getNamespace(item.getOldID()));
		} else {
			Namespace ns = session.getNamespace(item.getOldID());
			ns.setID(item.getNewID());
		}
		return null;
	}

	public OperationWarning reverse(TermNamespaceHistoryItem item) {
		if (item.isAdd())
			session.removeNamespace(session.getNamespace(item.getNewID()));
		else if (item.isDel()) {
			session.addNamespace(session.getNamespace(item.getOldID()));
		} else {
			Namespace ns = session.getNamespace(item.getNewID());
			ns.setID(item.getOldID());
		}
		return null;
	}

	protected LinkedObject getRealObject(String term) {
		return (LinkedObject) getRealIDObject(term);
	}

	protected IdentifiedObject getRealIDObject(String term) {
		if (term == null)
			return null;
		IdentifiedObject out = session.getObject(term);
		if (out == null) {
			for (IdentifiedObject io : session.getObjects()) {
				if (io instanceof LinkedObject) {
					for (Link link : ((LinkedObject) io).getParents()) {
						/*
						 * if (link.getParent() instanceof DanglingObject) {
						 * logger.info("*** "+term+" doesn't match
						 * "+link.getParent()); }
						 */
						if (link.getParent() != null
								&& link.getParent().getID().equals(term)) {
							// (new Exception("*** found match for "+term+" in
							// link "+link)).printStackTrace();
							return link.getParent();
						}
					}
				}
			}
		}
		return out;
	}

	protected Link getRealRel(StringRelationship tr) {
		if (tr == null)
			return null;
		LinkedObject parent = getRealObject(tr.getParent());
		LinkedObject child = getRealObject(tr.getChild());
		OBOProperty type = (OBOProperty) getRealObject(tr.getType());

		if (parent == null && allowDangling) {
			parent = new DanglingObjectImpl(tr.getParent());
		}
		if (type == null && allowDangling) {
			type = new DanglingPropertyImpl(tr.getType());
		}

		Namespace ns = session.getNamespace(tr.getNamespace());
		OBORestriction realtr = new OBORestrictionImpl(child, parent, type);
		realtr.setNamespace(ns);
		realtr.setNecessarilyTrue(tr.isNecessary());
		realtr.setInverseNecessarilyTrue(tr.isInverseNecessary());
		realtr.setCompletes(tr.completes());
		realtr.setMinCardinality(tr.getMinCardinality());
		realtr.setMaxCardinality(tr.getMaxCardinality());
		realtr.setCardinality(tr.getCardinality());
		return realtr;
	}

	public OperationWarning apply(CreateObjectHistoryItem item) {
		IdentifiedObject type = session.getObject(item.getObjectType());
		if (type == null)
			return new OperationWarning("Could not find type "
					+ item.getObjectType() + ".");
		if (!(type instanceof OBOClass))
			return new OperationWarning(item.getObjectType() + " is not a "
					+ "type.");
		if (session.getObject(item.getObjectID()) != null)
			return new OperationWarning("Could not create object with id "
					+ item.getObjectID()
					+ " because an object with that id already exists.");
		IdentifiedObject io = session.getObjectFactory().createObject(
				item.getObjectID(), (OBOClass) type, item.isAnonymous());
		if (io instanceof ModificationMetadataObject) {
			((ModificationMetadataObject) io).setCreationDate(new Date());
			((ModificationMetadataObject) io).setCreatedBy(session
					.getCurrentUser());
		}
		session.addObject(io);

		return null;
	}

	public OperationWarning reverse(CreateObjectHistoryItem item) {
		IdentifiedObject newObj = session.getObject(item.getObjectID());
		session.removeObject(newObj);
		return null;
	}

	public OperationWarning apply(SecondaryIDHistoryItem item) {
		LinkedObject t = getRealObject(item.getTarget());

		if (t == null)
			return new OperationWarning("Could not assign secondary id to "
					+ "unknown target " + item.getTarget());
		if (!(t instanceof MultiIDObject))
			return new OperationWarning("Could not assign secondary id to "
					+ item.getTarget() + " because it does "
					+ "not allow secondary ids");
		MultiIDObject target = (MultiIDObject) t;

		boolean hasSecondary = target.getSecondaryIDs().contains(
				item.getSecondaryID());
		if (item.isDelete()) {
			if (!hasSecondary)
				return new OperationWarning("Could not remove secondary id "
						+ item.getSecondaryID() + " from target "
						+ item.getTarget() + ", because the "
						+ "term does not have that id.");
			else
				target.removeSecondaryID(item.getSecondaryID());
		} else {
			if (hasSecondary)
				return new OperationWarning("Could not add secondary id "
						+ item.getSecondaryID() + " to target "
						+ item.getTarget() + ", because the "
						+ "term already has that id.");
			else
				target.addSecondaryID(item.getSecondaryID());
		}

		return null;
	}

	public OperationWarning reverse(SecondaryIDHistoryItem item) {
		LinkedObject t = getRealObject(item.getTarget());

		if (t == null)
			return new OperationWarning("Could not reverse assignment of "
					+ "secondary id to " + "unknown target " + item.getTarget());
		if (!(t instanceof MultiIDObject))
			return new OperationWarning("Could not reverse assignment of "
					+ "secondary id to " + item.getTarget()
					+ " because it does " + "not allow secondary ids");

		MultiIDObject target = (MultiIDObject) t;

		boolean hasSecondary = target.getSecondaryIDs().contains(
				item.getSecondaryID());

		if (item.isDelete()) {
			if (hasSecondary)
				return new OperationWarning("Could not reverse deletion "
						+ "of secondary id " + item.getSecondaryID()
						+ " to target " + item.getTarget() + ", because the "
						+ "term already has that id.");
			else
				target.addSecondaryID(item.getSecondaryID());
		} else {
			if (!hasSecondary)
				return new OperationWarning("Could not reverse addition of "
						+ "secondary id " + item.getSecondaryID()
						+ " from target " + item.getTarget() + ", because the "
						+ "term does not have that id.");
			else
				target.removeSecondaryID(item.getSecondaryID());
		}
		return null;
	}

	public OperationWarning apply(InverseNecHistoryItem item) {
		OBORestriction tr = (OBORestriction) getRealRel(item.getRel());
		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change inverse necessary "
							+ "status of unknown term relationship "
							+ item.getRel() + " to "
							+ !item.getOldInverseNecessary());
			return cwarning;
		}
		tr = (OBORestriction) HistoryUtil.findChildRel(tr, tr.getParent());

		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change inverse necessary "
							+ "status of unknown term relationship " + tr
							+ " because it doesn't exist in " + "the ontology");
			return cwarning;
		}
		tr.setInverseNecessarilyTrue(!item.getOldInverseNecessary());
		return null;
	}

	public OperationWarning reverse(InverseNecHistoryItem item) {
		OBORestriction tr = (OBORestriction) getRealRel(item.getRel());
		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change inverse necessary "
							+ "status of unknown term relationship "
							+ item.getRel() + " back to "
							+ item.getOldInverseNecessary());
			return cwarning;
		}
		tr = (OBORestriction) HistoryUtil.findChildRel(tr, tr.getParent());

		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change inverse necessary "
							+ "status of unknown term relationship " + tr
							+ " because it doesn't exist in " + "the ontology");
			return cwarning;
		}
		tr.setInverseNecessarilyTrue(item.getOldInverseNecessary());
		return null;
	}

	public OperationWarning apply(RangeHistoryItem item) {
		LinkedObject target = getRealObject(item.getTarget());
		IdentifiedObject range = getRealIDObject(item.getRange());
		if (target == null) {
			OperationWarning warning = new OperationWarning(
					"Couldn't change range of " + item.getTarget()
							+ " because it doesn't exist in the ontology"
							+ item.getRange());
			return warning;
		}
		if (range == null) {
			OperationWarning warning = new OperationWarning(
					"Could not apply change range of " + item.getTarget()
							+ " to " + item.getRange() + ", because "
							+ item.getRange() + " does not "
							+ "exist in the ontology");
			return warning;
		}
		if (!(target instanceof OBOProperty)) {
			OperationWarning warning = new OperationWarning(
					"Could not apply range change to " + "non-property "
							+ target);
			return warning;
		}
		if (range != null && !(range instanceof Type)) {
			OperationWarning warning = new OperationWarning(
					"Could not apply range " + "change to " + target
							+ " with non-type object " + item.getRange());
			return warning;
		}
		OBOProperty type = (OBOProperty) target;
		type.setRange((Type) range);
		return null;
	}

	public OperationWarning reverse(RangeHistoryItem item) {
		LinkedObject target = getRealObject(item.getTarget());
		IdentifiedObject range = getRealIDObject(item.getOldRange());
		if (target == null) {
			OperationWarning warning = new OperationWarning(
					"Couldn't reverse range change of " + item.getTarget()
							+ " because it doesn't exist in the ontology"
							+ item.getRange());
			return warning;
		}
		if (range == null) {
			OperationWarning warning = new OperationWarning(
					"Could not reverse change range of " + item.getTarget()
							+ " to " + item.getOldRange() + ", because "
							+ item.getOldRange() + " does not "
							+ "exist in the ontology");
			return warning;
		}
		if (!(target instanceof OBOProperty)) {
			OperationWarning warning = new OperationWarning(
					"Could not reverse range change of non-type " + target);
			return warning;
		}
		if (range != null && !(range instanceof Type)) {
			OperationWarning warning = new OperationWarning(
					"Could not reverse range " + "change to " + target
							+ " with non-type object " + item.getRange());
			return warning;
		}
		OBOProperty type = (OBOProperty) target;
		type.setRange((Type) range);
		return null;
	}

	public OperationWarning apply(AddReplacementHistoryItem item) {
		LinkedObject target = getRealObject(item.getTarget());
		LinkedObject replacedBy = getRealObject(item.getReplace());

		if (target == null) {
			OperationWarning warning = new OperationWarning(
					"Couldn't change replacement for " + item.getTarget()
							+ " because it doesn't exist in the ontology");
			return warning;
		}
		if (replacedBy == null && item.getReplace() != null) {
			OperationWarning warning = new OperationWarning(
					"Could not change replacement of " + item.getTarget()
							+ " to " + item.getReplace() + ", because "
							+ item.getReplace() + " does not "
							+ "exist in the ontology");
			return warning;
		}

		if (!(target instanceof ObsoletableObject)) {
			OperationWarning warning = new OperationWarning(
					"Could not change replacement to "
							+ "non-obsoletable object " + item.getTarget());
			return warning;
		}

		if (replacedBy != null && !(replacedBy instanceof ObsoletableObject)) {
			OperationWarning warning = new OperationWarning(
					"Could not change replacement of " + target
							+ " to non-obsoletable object " + item.getTarget());
			return warning;
		}

		((ObsoletableObject) target)
				.addReplacedBy((ObsoletableObject) replacedBy);
		return null;
	}

	public OperationWarning reverse(RemoveReplacementHistoryItem item) {
		LinkedObject target = getRealObject(item.getTarget());
		LinkedObject replacedBy = getRealObject(item.getReplace());

		if (target == null) {
			OperationWarning warning = new OperationWarning(
					"Couldn't undo replacement for " + item.getTarget()
							+ " because it doesn't exist in the ontology");
			return warning;
		}
		if (replacedBy == null && item.getReplace() != null) {
			OperationWarning warning = new OperationWarning(
					"Could not undo replacement of " + item.getTarget()
							+ " to " + item.getReplace() + ", because "
							+ item.getReplace() + " does not "
							+ "exist in the ontology");
			return warning;
		}

		if (!(target instanceof ObsoletableObject)) {
			OperationWarning warning = new OperationWarning(
					"Could not undo replacement to "
							+ "non-obsoletable object " + item.getTarget());
			return warning;
		}

		if (replacedBy != null && !(replacedBy instanceof ObsoletableObject)) {
			OperationWarning warning = new OperationWarning(
					"Could not undo replacement of " + target
							+ " to non-obsoletable object " + item.getTarget());
			return warning;
		}

		((ObsoletableObject) target)
				.addReplacedBy((ObsoletableObject) replacedBy);
		return null;
	}

	public OperationWarning apply(RemoveReplacementHistoryItem item) {
		LinkedObject target = getRealObject(item.getTarget());
		LinkedObject replacedBy = getRealObject(item.getReplace());

		if (target == null) {
			OperationWarning warning = new OperationWarning(
					"Couldn't remove replacement for " + item.getTarget()
							+ " because it doesn't exist in the ontology");
			return warning;
		}

		if (replacedBy == null && item.getReplace() != null) {
			OperationWarning warning = new OperationWarning(
					"Could not remove replacement of " + item.getTarget()
							+ " to " + item.getReplace() + ", because "
							+ item.getReplace()
							+ " does not exist in the ontology");
			return warning;
		}

		if (!(target instanceof ObsoletableObject)) {
			OperationWarning warning = new OperationWarning(
					"Could not remove replacement for "
							+ "non-obsoletable object " + item.getTarget());
			return warning;
		}

		if (replacedBy != null && !(replacedBy instanceof ObsoletableObject)) {
			OperationWarning warning = new OperationWarning(
					"Could not remove replacement for " + target
							+ " of non-obsoletable object " + replacedBy);
			return warning;
		}

		((ObsoletableObject) target)
				.removeReplacedBy((ObsoletableObject) replacedBy);
		return null;
	}

	public OperationWarning reverse(AddReplacementHistoryItem item) {
		LinkedObject target = getRealObject(item.getTarget());
		LinkedObject replacedBy = getRealObject(item.getReplace());

		if (target == null) {
			OperationWarning warning = new OperationWarning(
					"Couldn't reverse replacement change for "
							+ item.getTarget()
							+ " because it doesn't exist in the ontology");
			return warning;
		}

		if (replacedBy == null && item.getReplace() != null) {
			OperationWarning warning = new OperationWarning(
					"Could not reverse replacement change of "
							+ item.getTarget() + " to " + item.getReplace()
							+ ", because " + item.getReplace()
							+ " does not exist in the ontology");
			return warning;
		}

		if (!(target instanceof ObsoletableObject)) {
			OperationWarning warning = new OperationWarning(
					"Could not reverse replacement change to "
							+ "non-obsoletable object " + item.getTarget());
			return warning;
		}

		if (replacedBy != null && !(replacedBy instanceof ObsoletableObject)) {
			OperationWarning warning = new OperationWarning(
					"Could not reverse replacement change of " + target
							+ " to non-obsoletable object " + replacedBy);
			return warning;
		}

		((ObsoletableObject) target)
				.removeReplacedBy((ObsoletableObject) replacedBy);
		return null;
	}

	public OperationWarning apply(DomainHistoryItem item) {
		LinkedObject target = getRealObject(item.getTarget());
		IdentifiedObject domain = getRealIDObject(item.getDomain());

		if (target == null) {
			OperationWarning warning = new OperationWarning(
					"Couldn't change domain of " + item.getTarget()
							+ " because it doesn't exist in the ontology"
							+ item.getDomain());
			return warning;
		}
		if (domain == null && item.getDomain() != null) {
			OperationWarning warning = new OperationWarning(
					"Could not apply change domain of " + item.getTarget()
							+ " to " + item.getDomain() + ", because "
							+ item.getDomain() + " does not "
							+ "exist in the ontology");
			return warning;
		}
		if (!(target instanceof OBOProperty)) {
			OperationWarning warning = new OperationWarning(
					"Could not apply domain change to non-type "
							+ item.getTarget());
			return warning;
		}
		if (domain != null && !(domain instanceof OBOClass)) {
			OperationWarning warning = new OperationWarning(
					"Could not apply domain " + "change to " + target
							+ " with non-type object " + item.getDomain());
			return warning;
		}
		OBOProperty type = (OBOProperty) target;
		type.setDomain(domain);
		return null;
	}

	public OperationWarning reverse(DomainHistoryItem item) {
		LinkedObject target = getRealObject(item.getTarget());
		IdentifiedObject domain = getRealIDObject(item.getOldDomain());
		if (target == null) {
			OperationWarning warning = new OperationWarning(
					"Couldn't reverse domain change of " + item.getTarget()
							+ " because it doesn't exist in the ontology.");
			return warning;
		}
		if (domain == null && item.getOldDomain() != null) {
			OperationWarning warning = new OperationWarning(
					"Could not reverse change domain of " + item.getTarget()
							+ " back to " + item.getOldDomain() + ", because "
							+ item.getOldDomain() + " does not "
							+ "exist in the ontology");
			return warning;
		}
		if (!(target instanceof OBOProperty)) {
			OperationWarning warning = new OperationWarning(
					"Could not reverse domain change to " + "non-type "
							+ item.getTarget());
			return warning;
		}

		if (domain != null && !(domain instanceof OBOClass)) {
			OperationWarning warning = new OperationWarning(
					"Could not reverse domain" + " change to " + target
							+ " with non-type object " + item.getDomain());
			return warning;
		}
		OBOProperty type = (OBOProperty) target;
		type.setDomain(domain);
		return null;
	}

	public OperationWarning apply(CardinalityHistoryItem item) {
		OBORestriction tr = (OBORestriction) getRealRel(item.getRel());
		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change cardinality "
							+ "of unknown term relationship " + item.getRel()
							+ " to " + item.getOldValue());
			return cwarning;
		}
		tr = (OBORestriction) HistoryUtil.findChildRel(tr, tr.getParent());

		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change cardinality "
							+ "of unknown term relationship " + tr
							+ " because it doesn't exist in " + "the ontology");
			return cwarning;
		}
		tr.setCardinality(item.getNewValue());
		return null;
	}

	public OperationWarning reverse(CardinalityHistoryItem item) {
		OBORestriction tr = (OBORestriction) getRealRel(item.getRel());
		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change cardinality "
							+ "of unknown term relationship " + item.getRel()
							+ " to " + item.getOldValue());
			return cwarning;
		}
		tr = (OBORestriction) HistoryUtil.findChildRel(tr, tr.getParent());

		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change cardinality "
							+ "of unknown term relationship " + tr
							+ " because it doesn't exist in " + "the ontology");
			return cwarning;
		}
		tr.setCardinality(item.getOldValue());
		return null;
	}

	public OperationWarning apply(MinCardinalityHistoryItem item) {
		OBORestriction tr = (OBORestriction) getRealRel(item.getRel());
		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change min cardinality "
							+ "of unknown term relationship " + item.getRel()
							+ " to " + item.getNewValue());
			return cwarning;
		}
		tr = (OBORestriction) HistoryUtil.findChildRel(tr, tr.getParent());

		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change min cardinality "
							+ "of unknown term relationship " + tr
							+ " because it doesn't exist in " + "the ontology");
			return cwarning;
		}
		tr.setMinCardinality(item.getNewValue());
		return null;
	}

	public OperationWarning reverse(MinCardinalityHistoryItem item) {
		OBORestriction tr = (OBORestriction) getRealRel(item.getRel());
		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change min cardinality "
							+ "of unknown term relationship " + item.getRel()
							+ " to " + item.getOldValue());
			return cwarning;
		}
		tr = (OBORestriction) HistoryUtil.findChildRel(tr, tr.getParent());

		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change min cardinality "
							+ "of unknown term relationship " + tr
							+ " because it doesn't exist in " + "the ontology");
			return cwarning;
		}
		tr.setMinCardinality(item.getOldValue());
		return null;
	}

	public OperationWarning apply(MaxCardinalityHistoryItem item) {
		OBORestriction tr = (OBORestriction) getRealRel(item.getRel());
		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change min cardinality "
							+ "of unknown term relationship " + item.getRel()
							+ " to " + item.getNewValue());
			return cwarning;
		}
		tr = (OBORestriction) HistoryUtil.findChildRel(tr, tr.getParent());

		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change min cardinality "
							+ "of unknown term relationship " + tr
							+ " because it doesn't exist in " + "the ontology");
			return cwarning;
		}
		tr.setMaxCardinality(item.getNewValue());
		return null;
	}

	public OperationWarning reverse(MaxCardinalityHistoryItem item) {
		OBORestriction tr = (OBORestriction) getRealRel(item.getRel());
		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change min cardinality "
							+ "of unknown term relationship " + item.getRel()
							+ " to " + item.getOldValue());
			return cwarning;
		}
		tr = (OBORestriction) HistoryUtil.findChildRel(tr, tr.getParent());

		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change min cardinality "
							+ "of unknown term relationship " + tr
							+ " because it doesn't exist in " + "the ontology");
			return cwarning;
		}
		tr.setMaxCardinality(item.getOldValue());
		return null;
	}

	public OperationWarning apply(CompletesHistoryItem item) {
		OBORestriction tr = (OBORestriction) getRealRel(item.getRel());
		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change completes status "
							+ "of unknown term relationship " + item.getRel()
							+ " to " + !item.getOldCompletes());
			return cwarning;
		}
		tr = (OBORestriction) HistoryUtil.findChildRel(tr, tr.getParent());

		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change completes status "
							+ "of unknown term relationship " + item.getRel()
							+ " because it doesn't exist in " + "the ontology");
			return cwarning;
		}
		// the relationship MUST be removed and readded because a
		// change in completeness changes the relationship hash code.
		// Removing and re-adding the relationship keeps the internal
		// term hashes consistent
		tr.getChild().removeParent(tr);
		tr.setCompletes(!item.getOldCompletes());
		tr.getChild().addParent(tr);
		return null;
	}

	public OperationWarning reverse(CompletesHistoryItem item) {
		StringRelationship sr = (StringRelationship) item.getRel().clone();
		sr.setCompletes(!item.getOldCompletes());

		OBORestriction tr = (OBORestriction) getRealRel(sr);
		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change completes "
							+ "status of unknown term relationship "
							+ item.getRel() + " back to "
							+ item.getOldCompletes());
			return cwarning;
		}
		tr = (OBORestriction) HistoryUtil.findChildRel(tr, tr.getParent());

		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change completes status "
							+ "of unknown term relationship " + tr
							+ " because it doesn't exist in " + "the ontology");
			return cwarning;
		}
		tr.getParent().removeChild(tr);
		tr.setCompletes(item.getOldCompletes());
		tr.getParent().addChild(tr);
		return null;
	}

	public OperationWarning apply(NecessarilyTrueHistoryItem item) {

		OBORestriction tr = (OBORestriction) getRealRel(item.getRel());
		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change necessary status "
							+ "of unknown term relationship " + item.getRel()
							+ " to " + !item.getOldNecessary());
			return cwarning;
		}
		Link temp = tr;
		tr = (OBORestriction) HistoryUtil.findChildRel(tr, tr.getParent());

		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change necessary status "
							+ "of unknown term relationship " + item.getRel()
							+ " because it doesn't exist in "
							+ "the ontology. realRel = " + temp
							+ ", tr.getChild().getParents() = "
							+ temp.getChild().getParents());
			return cwarning;
		}
		tr.setNecessarilyTrue(!item.getOldNecessary());
		return null;
	}

	public OperationWarning reverse(NecessarilyTrueHistoryItem item) {
		OBORestriction tr = (OBORestriction) getRealRel(item.getRel());
		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change necessary "
							+ "status of unknown term relationship "
							+ item.getRel() + " back to "
							+ item.getOldNecessary());
			return cwarning;
		}
		tr = (OBORestriction) HistoryUtil.findChildRel(tr, tr.getParent());

		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change necessary status "
							+ "of unknown term relationship " + tr
							+ " because it doesn't exist in " + "the ontology");
			return cwarning;
		}
		tr.setNecessarilyTrue(item.getOldNecessary());
		return null;
	}

	protected boolean allowDangling = true;

	public void setAllowDangling(boolean allowDangling) {
		this.allowDangling = allowDangling;
	}

	public OperationWarning apply(CreateLinkHistoryItem item) {
		LinkedObject target = getRealObject(item.getTarget());
		if (target == null) {
			return new OperationWarning("Could not add parents to unknown "
					+ "term " + item.getTarget());
		}

		StringRelationship sr = new StringRelationship(item.getTarget(), item
				.getTypeID(), item.getParentID());

		Link tr = getRealRel(sr);

		if (tr.getParent() == null)
			if (allowDangling) {
				tr.setParent(session.getObjectFactory().createDanglingObject(
						sr.getParent(), false));
			} else
				return new OperationWarning(
						"Couldn't do copy of unknown parent " + sr.getParent()
								+ " to " + target + " with type "
								+ sr.getType() + ".");
		if (tr.getType() == null)
			if (allowDangling) {
				tr.setType((OBOProperty) session.getObjectFactory()
						.createDanglingObject(sr.getType(), true));
			} else
				return new OperationWarning("Couldn't do copy of "
						+ sr.getChild() + " to " + target
						+ " with unknown type " + sr.getType() + ".");

		if (HistoryUtil.findParentRel(tr, target) != null) {
			OperationWarning cwarning = new OperationWarning(
					"Could not create link " + tr
							+ " because it already exists");
			return cwarning;
		} else {
			tr.getChild().addParent(tr);
		}

		return null;
	}

	public OperationWarning reverse(CreateLinkHistoryItem item) {
		LinkedObject target = getRealObject(item.getTarget());
		if (target == null) {
			return new OperationWarning("Could not remove parents to unknown "
					+ "term " + item.getTarget());
		}

		StringRelationship sr = new StringRelationship(item.getTarget(), item
				.getTypeID(), item.getParentID());

		Link tr = getRealRel(sr);

		if (tr.getParent() == null)
			if (allowDangling) {
				tr.setParent(session.getObjectFactory().createDanglingObject(
						sr.getParent(), false));
			} else
				return new OperationWarning(
						"Couldn't undo copy of unknown parent "
								+ sr.getParent() + " to " + target
								+ " with type " + sr.getType() + ".");
		if (tr.getType() == null)
			if (allowDangling) {
				tr.setType((OBOProperty) session.getObjectFactory()
						.createDanglingObject(sr.getType(), false));
			} else
				return new OperationWarning("Couldn't undo copy of "
						+ sr.getChild() + " to " + target
						+ " with unknown type " + sr.getType() + ".");

		if (HistoryUtil.findParentRel(tr, target) == null) {
			OperationWarning cwarning = new OperationWarning(
					"Could not remove link " + tr + " because it doesn't exist");
			return cwarning;
		} else {
			tr.getChild().removeParent(tr);
		}

		return null;
	}

	public OperationWarning apply(ObsoleteObjectHistoryItem item) {
		IdentifiedObject io = session.getObject(item.getTarget());
		if (io == null)
			return new OperationWarning("Cannot obsolete unknown object "
					+ item.getTarget());
		if (!(io instanceof ObsoletableObject))
			return new OperationWarning("Cannot obsolete non-obsoletable "
					+ "object " + item.getTarget());
		if (((ObsoletableObject) io).isObsolete())
			return new OperationWarning("Cannot obsolete already obsolete "
					+ "object " + io);
		((ObsoletableObject) io).setObsolete(true);
		return null;
	}

	public OperationWarning reverse(ObsoleteObjectHistoryItem item) {
		IdentifiedObject io = session.getObject(item.getTarget());
		if (io == null)
			return new OperationWarning("Cannot unobsolete unknown object "
					+ item.getTarget());
		if (!(io instanceof ObsoletableObject))
			return new OperationWarning("Cannot unobsolete non-obsoletable "
					+ "object " + item.getTarget());
		if (!((ObsoletableObject) io).isObsolete())
			return new OperationWarning("Cannot unobsolete non-obsolete "
					+ "object " + io);
		((ObsoletableObject) io).setObsolete(false);
		return null;
	}

	public OperationWarning apply(DestroyObjectHistoryItem item) {
		IdentifiedObject io = session.getObject(item.getTarget());
		if (io == null)
			return new OperationWarning("Cannot destroy unknown object " + io);

		session.removeObject(io);
		return null;
	}

	public OperationWarning reverse(DestroyObjectHistoryItem item) {
		IdentifiedObject io = item.getObject();
		if (io == null)
			return new OperationWarning("Cannot undestroy unknown object " + io);

		session.addObject(io);
		return null;
	}

	public OperationWarning apply(DeleteLinkHistoryItem item) {
		StringRelationship sr = item.getRel();

		Link tr = getRealRel(sr);
		tr = HistoryUtil.findChildRelNoIntersection(tr, tr.getParent());

		if (tr == null) {
			return new OperationWarning("Couldn't find link " + sr
					+ " in ontology");
		}

		if (tr.getChild() == null) {
			return new OperationWarning("Can't delete "
					+ "link with unknown child " + sr.getChild());
		}

		if (sr.getParent() != null && tr.getParent() == null) {
			return new OperationWarning("Couldn't delete " + "relationship "
					+ "with unknown " + "parent " + sr.getParent(),
					OperationWarning.DANGEROUS);
		}
		if (tr.getParent() != null) {
			if (tr.getType() == null) {
				return new OperationWarning("Can't delete " + "relationship "
						+ "with unknown " + "type " + sr.getType());
			} else if (!HistoryUtil.hasChild(tr.getParent(), tr)) {
				return new OperationWarning("Couldn't delete "
						+ "non-existant " + "relationship " + tr);
			}
			tr.getChild().removeParent(tr);
		} else
			return new OperationWarning("Can't delete link " + tr
					+ " with NULL parent!");

		return null;
	}

	public OperationWarning reverse(DeleteLinkHistoryItem item) {
		StringRelationship sr = item.getRel();

		Link tr = getRealRel(sr);

		if (tr.getChild() == null) {
			return new OperationWarning("Can't undelete "
					+ "link with unknown child " + sr.getChild());
		}

		if (sr.getParent() != null && tr.getParent() == null) {
			return new OperationWarning("Couldn't undelete " + "relationship "
					+ "with unknown " + "parent " + sr.getParent(),
					OperationWarning.DANGEROUS);
		}
		if (tr.getParent() != null) {
			if (tr.getType() == null) {
				return new OperationWarning("Can't undelete " + "relationship "
						+ "with unknown " + "type " + sr.getType());
			}
			tr.getParent().addChild(tr);
		}

		return null;
	}

	public OperationWarning apply(TermMacroHistoryItem item) {

		item.lock(session);

		OperationWarning warning = new OperationWarning("Couldn't apply "
				+ "macro history item");
		boolean failure = false;
		for (int i = 0; i < item.size(); i++) {
			try {
				OperationWarning w = apply(item.getItemAt(i));
				if (w != null) {
					logger.info("got warning " + w + " from "
							+ item.getItemAt(i));
					failure = true;
					warning.addWarning(w);
				}
			} catch (Throwable t) {
				t.printStackTrace();
			}
		}

		if (failure)
			return warning;
		return null;
	}

	public OperationWarning reverse(TermMacroHistoryItem item) {

		OperationWarning warning = new OperationWarning("Couldn't apply "
				+ "macro history item");
		boolean failure = false;
		for (int i = item.size() - 1; i >= 0; i--) {
			OperationWarning w = reverse(item.getItemAt(i));
			if (w != null) {
				failure = true;
				warning.addWarning(w);
			}
		}

		if (failure)
			return warning;
		return null;
	}

	public OperationWarning apply(LinkTypeHistoryItem item) {
		OBOProperty type = (OBOProperty) getRealObject(item
				.getRelationshipType());

		if (type == null) {
			return new OperationWarning("Couldn't do relationship "
					+ "type change with missing " + "type "
					+ item.getRelationshipType());
		}

		StringRelationship sr = item.getRel();

		Link tr = getRealRel(sr);
		if (tr.getParent() == null) {
			return new OperationWarning("Couldn't do " + "relationship type "
					+ "change with missing " + "parent " + sr.getParent());
		}

		if (tr.getChild() == null) {
			return new OperationWarning("Couldn't do " + "relationship type "
					+ "change with missing " + "child " + sr.getChild());
		}
		if (tr.getType() == null) {
			return new OperationWarning("Couldn't complete "
					+ "relationship type " + "change with missing "
					+ "relationship type " + sr.getType());
		}
		tr = HistoryUtil.findChildRelNoIntersection(tr, tr.getParent());

		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't change type status "
							+ "of unknown term relationship " + tr
							+ " because it doesn't exist in " + "the ontology");
			return cwarning;
		}

		tr.setType(type);

		return null;
	}

	public OperationWarning reverse(LinkTypeHistoryItem item) {

		StringRelationship sr = item.getRel();

		OBOProperty type = (OBOProperty) getRealObject(sr.getType());

		if (type == null) {
			return new OperationWarning("Couldn't do relationship "
					+ "type change with missing " + "type "
					+ item.getRelationshipType());
		}

		sr = new StringRelationship(sr.getChild(), item.getRelationshipType(),
				sr.getParent());

		Link tr = getRealRel(sr);

		if (tr.getParent() == null) {
			return new OperationWarning("Couldn't undo " + "relationship type "
					+ "change with missing " + "parent " + sr.getParent());
		}

		if (tr.getChild() == null) {
			return new OperationWarning("Couldn't undo " + "relationship type "
					+ "change with missing " + "child " + sr.getChild());
		}
		if (tr.getType() == null) {
			return new OperationWarning("Couldn't undo " + "relationship type "
					+ "change with missing " + "relationship type "
					+ sr.getType());
		}

		tr = HistoryUtil.findChildRel(tr, tr.getParent());

		if (tr == null) {
			OperationWarning cwarning = new OperationWarning(
					"Couldn't undo type change "
							+ "of unknown term relationship " + tr
							+ " because it doesn't exist in " + "the ontology");
			return cwarning;
		}

		tr.setType(type);
		return null;
	}

	public OperationWarning apply(NameChangeHistoryItem item) {
		LinkedObject target = getRealObject(item.getTarget());
		if (target == null) {
			return new OperationWarning("no term for id " + item.getTarget());
		}
		target.setName(item.getNewText());
		return null;
	}

	public OperationWarning reverse(NameChangeHistoryItem item) {
		LinkedObject target = getRealObject(item.getTarget());
		target.setName(item.getText());
		return null;
	}

	public OperationWarning apply(NamespaceHistoryItem item) {
		LinkedObject target = getRealObject(item.getTarget());
		if (target == null) {
			return new OperationWarning("Could not change namespace "
					+ "of unrecognized term " + item.getTarget());
		}
		Namespace ns = null;
		if (item.getNewNamespace() != null) {
			String nsid = item.getNewNamespace().getID();
			ns = session.getNamespace(nsid);
			if (ns == null) {
				return new OperationWarning("Could not change namespace "
						+ "to unrecognized namespace " + nsid);
			}
		}
		target.setNamespace(ns);
		return null;
	}

	public OperationWarning reverse(NamespaceHistoryItem item) {
		LinkedObject target = getRealObject(item.getTarget());
		Namespace ns = null;
		if (item.getOldNamespace() != null) {
			String nsid = item.getOldNamespace().getID();
			ns = session.getNamespace(nsid);
			if (ns == null) {
				return new OperationWarning("Could not undo change of "
						+ "namespace to unrecognized " + "namespace " + nsid);
			}
		}
		target.setNamespace(ns);
		return null;
	}

	public OperationWarning apply(DefinitionChangeHistoryItem item) {
		IdentifiedObject o = getRealIDObject(item.getTarget());
		if (!(o instanceof DefinedObject)) {
			return new OperationWarning("Could not apply definition change "
					+ "to non definable object " + item.getTarget());
		}
		DefinedObject target = (DefinedObject) o;
		target.setDefinition(item.getNewText());
		return null;
	}

	public OperationWarning reverse(DefinitionChangeHistoryItem item) {
		IdentifiedObject o = getRealIDObject(item.getTarget());
		if (!(o instanceof DefinedObject)) {
			return new OperationWarning("Could not reverse definition change "
					+ "to non-definable object " + item.getTarget());
		}
		DefinedObject target = (DefinedObject) o;
		target.setDefinition(item.getText());
		return null;
	}

	public OperationWarning apply(CommentChangeHistoryItem item) {
		IdentifiedObject o = getRealIDObject(item.getTarget());
		if (!(o instanceof CommentedObject))
			return new OperationWarning("Could not apply comment change to "
					+ "non-commentable object " + item.getTarget());
		CommentedObject target = (CommentedObject) o;
		target.setComment(item.getNewText());
		return null;
	}

	public OperationWarning reverse(CommentChangeHistoryItem item) {
		IdentifiedObject o = getRealIDObject(item.getTarget());
		if (!(o instanceof CommentedObject))
			return new OperationWarning("Could not reverse comment change to "
					+ "non-commentable object " + item.getTarget());
		CommentedObject target = (CommentedObject) o;
		target.setComment(item.getText());
		return null;
	}

	public OperationWarning apply(CategoryChangeHistoryItem item) {
		IdentifiedObject o = getRealIDObject(item.getTarget());
		if (!(o instanceof CategorizedObject))
			return new OperationWarning("Could not apply category change to "
					+ "non-categorizable object " + item.getTarget());
		CategorizedObject target = (CategorizedObject) o;
		TermCategory cat = session.getCategory(item.getCategory());
		if (item.isDel())
			target.removeCategory(cat);
		else
			target.addCategory(cat);
		return null;
	}

	public OperationWarning reverse(CategoryChangeHistoryItem item) {
		IdentifiedObject o = getRealIDObject(item.getTarget());
		if (!(o instanceof CategorizedObject))
			return new OperationWarning("Could not reverse category change to "
					+ "non-categorizable object " + item.getTarget());
		CategorizedObject target = (CategorizedObject) o;
		TermCategory cat = session.getCategory(item.getCategory());

		if (item.isDel())
			target.addCategory(cat);
		else
			target.removeCategory(cat);
		return null;
	}

	public OperationWarning apply(ChangeSynScopeHistoryItem item) {
		IdentifiedObject o = getRealIDObject(item.getTarget());
		if (!(o instanceof SynonymedObject))
			return new OperationWarning("Could not change synonym scope "
					+ "in non-synonymable object " + item.getTarget());
		SynonymedObject target = (SynonymedObject) o;
		Synonym s = HistoryUtil.findSynonym(target, item.getSynonym());
		if (s == null)
			return new OperationWarning("Couldn't modify non-existant "
					+ "synonym " + item.getSynonym());
		s.setScope(item.getNewScope());
		return null;
	}

	public OperationWarning reverse(ChangeSynScopeHistoryItem item) {
		IdentifiedObject o = getRealIDObject(item.getTarget());
		if (!(o instanceof SynonymedObject))
			return new OperationWarning("Could not change synonym scope "
					+ "in non-synonymable object " + item.getTarget());
		SynonymedObject target = (SynonymedObject) o;
		Synonym s = HistoryUtil.findSynonym(target, item.getSynonym());
		if (s == null)
			return new OperationWarning("Couldn't modify non-existant "
					+ "synonym " + item.getSynonym());
		s.setScope(item.getOldScope());
		return null;
	}

	public OperationWarning apply(ChangeSynCategoryHistoryItem item) {
		IdentifiedObject o = getRealIDObject(item.getTarget());
		if (!(o instanceof SynonymedObject))
			return new OperationWarning("Could not change synonym category "
					+ "in non-synonymable object " + item.getTarget());
		SynonymedObject target = (SynonymedObject) o;
		Synonym s = HistoryUtil.findSynonym(target, item.getSynonym());
		if (s == null)
			return new OperationWarning("Couldn't modify non-existant "
					+ "synonym " + item.getSynonym());
		SynonymCategory cat = null;
		if (item.getNewCategory() != null) {
			cat = session.getSynonymCategory(item.getNewCategory());
			if (cat == null)
				return new OperationWarning("Couldn't find category "
						+ item.getNewCategory());
		}
		s.setSynonymCategory(cat);
		return null;
	}

	public OperationWarning reverse(ChangeSynCategoryHistoryItem item) {
		IdentifiedObject o = getRealIDObject(item.getTarget());
		if (!(o instanceof SynonymedObject))
			return new OperationWarning("Could not change synonym category "
					+ "in non-synonymable object " + item.getTarget());
		SynonymedObject target = (SynonymedObject) o;
		Synonym s = HistoryUtil.findSynonym(target, item.getSynonym());
		if (s == null)
			return new OperationWarning("Couldn't modify non-existant "
					+ "synonym " + item.getSynonym());

		SynonymCategory cat = null;
		if (item.getOldCategory() != null) {
			cat = session.getSynonymCategory(item.getOldCategory());
			if (cat == null)
				return new OperationWarning("Couldn't find category "
						+ item.getOldCategory());
		}
		s.setSynonymCategory(cat);
		return null;
	}

	public OperationWarning apply(AddConsiderHistoryItem item) {
		IdentifiedObject o = getRealObject(item.getTarget());
		if (o == null)
			return new OperationWarning("Could not add consider term to "
					+ "unknown target " + item.getTarget());

		if (!(o instanceof ObsoletableObject)) {
			return new OperationWarning("Could not add consider term to "
					+ "non-obsoletable term " + o);
		}

		ObsoletableObject target = (ObsoletableObject) o;

		IdentifiedObject consider = getRealObject(item.getConsider());
		if (consider == null) {
			return new OperationWarning("Could not add unknown consider term "
					+ item.getConsider() + " to " + target);

		}
		if (!(consider instanceof ObsoletableObject)) {
			return new OperationWarning("Could not add non-obsoletable term "
					+ consider + " as consider term for " + target);
		}

		target.addConsiderReplacement((ObsoletableObject) consider);
		return null;
	}

	public OperationWarning reverse(AddConsiderHistoryItem item) {
		IdentifiedObject o = getRealObject(item.getTarget());
		if (o == null)
			return new OperationWarning("Could not reverse addition of "
					+ "consider term to " + "unknown target "
					+ item.getTarget());

		if (!(o instanceof ObsoletableObject)) {
			return new OperationWarning("Could not reverse addition of "
					+ "consider term to " + "non-obsoletable term " + o);
		}

		ObsoletableObject target = (ObsoletableObject) o;

		IdentifiedObject consider = getRealObject(item.getConsider());
		if (consider == null) {
			return new OperationWarning("Could not reverse addition of "
					+ "unknown consider term " + item.getConsider() + " to "
					+ target);

		}
		if (!(consider instanceof ObsoletableObject)) {
			return new OperationWarning("Could not reverse addition of "
					+ "non-obsoletable term " + consider
					+ " as consider term for " + target);
		}

		target.removeConsiderReplacement((ObsoletableObject) consider);
		return null;
	}

	public OperationWarning apply(RemoveConsiderHistoryItem item) {
		IdentifiedObject o = getRealObject(item.getTarget());
		if (o == null)
			return new OperationWarning("Could not rremove "
					+ "consider term from " + "unknown target "
					+ item.getTarget());

		if (!(o instanceof ObsoletableObject)) {
			return new OperationWarning("Could not remove "
					+ "consider term from " + "non-obsoletable term " + o);
		}

		ObsoletableObject target = (ObsoletableObject) o;

		IdentifiedObject consider = getRealObject(item.getConsider());
		if (consider == null) {
			return new OperationWarning("Could not remove "
					+ "unknown consider term " + item.getConsider() + " from "
					+ target);

		}
		if (!(consider instanceof ObsoletableObject)) {
			return new OperationWarning("Could not remove "
					+ "non-obsoletable term " + consider
					+ " as consider term for " + target);
		}

		target.removeConsiderReplacement((ObsoletableObject) consider);
		return null;
	}

	public OperationWarning reverse(RemoveConsiderHistoryItem item) {
		IdentifiedObject o = getRealObject(item.getTarget());
		if (o == null)
			return new OperationWarning("Could not remove consider term "
					+ "from unknown target " + item.getTarget());

		if (!(o instanceof ObsoletableObject)) {
			return new OperationWarning("Could not remove consider term from "
					+ "non-obsoletable term " + o);
		}

		ObsoletableObject target = (ObsoletableObject) o;

		IdentifiedObject consider = getRealObject(item.getConsider());
		if (consider == null) {
			return new OperationWarning("Could not remove unknown consider "
					+ "term " + item.getConsider() + " from " + target);

		}
		if (!(consider instanceof ObsoletableObject)) {
			return new OperationWarning("Could not remove non-obsoletable "
					+ "term " + consider + " as consider term for " + target);
		}

		target.addConsiderReplacement((ObsoletableObject) consider);
		return null;
	}

	public OperationWarning apply(AddSynonymHistoryItem item) {
		IdentifiedObject o = getRealIDObject(item.getTarget());
		if (!(o instanceof SynonymedObject))
			return new OperationWarning("Could not add synonym to "
					+ "non-synonymable object " + item.getTarget());

		SynonymedObject target = (SynonymedObject) o;
		Synonym s = session.getObjectFactory().createSynonym(item.getSynonym(),
				Synonym.RELATED_SYNONYM);
		if (HistoryUtil.findSynonym(target, s) == null)
			target.addSynonym(s);
		else
			return new OperationWarning("Could not add synonym " + s + " to "
					+ item.getTarget()
					+ " because that synonym has already been added");
		return null;
	}

	public OperationWarning reverse(AddSynonymHistoryItem item) {
		IdentifiedObject o = getRealIDObject(item.getTarget());

		if (!(o instanceof SynonymedObject))
			return new OperationWarning("Could not remove synonym from "
					+ "non-synonymable object " + item.getTarget());

		SynonymedObject target = (SynonymedObject) o;
		Synonym s = HistoryUtil.findSynonym(target, item.getSynonym());

		if (s == null)
			return new OperationWarning("Could not delete non-existant "
					+ "synonym " + s + " of " + target);

		target.removeSynonym(s);
		return null;
	}

	public OperationWarning reverse(DelSynonymHistoryItem item) {
		IdentifiedObject o = getRealIDObject(item.getTarget());
		if (!(o instanceof SynonymedObject))
			return new OperationWarning("Could not add synonym to "
					+ "non-synonymable object " + item.getTarget());

		SynonymedObject target = (SynonymedObject) o;
		target.addSynonym(session.getObjectFactory().createSynonym(
				item.getSynonym(), Synonym.RELATED_SYNONYM));
		return null;
	}

	public OperationWarning apply(DelSynonymHistoryItem item) {
		IdentifiedObject o = getRealIDObject(item.getTarget());
		if (!(o instanceof SynonymedObject))
			return new OperationWarning("Could not reverse synonym change to "
					+ "non-synonymable object " + item.getTarget());

		SynonymedObject target = (SynonymedObject) o;
		Synonym s = HistoryUtil.findSynonym(target, item.getSynonym());
		if (s == null)
			return new OperationWarning("Could not delete non-existant "
					+ "synonym " + s + " of " + target);

		target.removeSynonym(s);
		return null;
	}

	/*
	 * public OperationWarning apply(DbxrefChangeHistoryItem item) { boolean
	 * isAdd = item.isAdd(); boolean isDel = item.isDel(); Synonym synonym =
	 * item.getSynonym(); Dbxref newDbxref = (Dbxref) item.getNewDbxref();
	 * Dbxref oldDbxref = (Dbxref) item.getOldDbxref(); IdentifiedObject o =
	 * getRealIDObject(item.getTarget());
	 * 
	 * if (synonym != null) { if (!(o instanceof SynonymedObject)) return new
	 * OperationWarning("Cannot apply synonym dbxref "+ "edit to non-synonymable
	 * object "+ item.getTarget()); SynonymedObject target = (SynonymedObject)
	 * o; Synonym s = TermUtil.findSynonym(target, synonym.privateid); if
	 * (isAdd) { s.addDbxref(newDbxref); } else if (isDel) {
	 * s.removeDbxref(oldDbxref); } else { Dbxref ref =
	 * s.findDbxref(newDbxref.privateid); ref.setID(newDbxref.getID());
	 * ref.setDesc(newDbxref.getDesc());
	 * ref.setDatabase(newDbxref.getDatabase());
	 * ref.setType(newDbxref.getType()); } } else { boolean isDefEdit =
	 * (newDbxref != null && newDbxref.isDefRef()) || (oldDbxref != null &&
	 * oldDbxref.isDefRef()); if (isDefEdit) { if (!(o instanceof
	 * DefinedObject)) return new OperationWarning("Can't apply definition "+
	 * "dbxref edit to "+ "non-definable object "+ item.getTarget());
	 * DefinedObject target = (DefinedObject) o; if (isAdd)
	 * target.addDefDbxref(newDbxref); else if (isDel)
	 * target.removeDefDbxref(oldDbxref); else { Dbxref ref =
	 * TermUtil.findDefDbxref(target, oldDbxref); if (ref == null) return new
	 * OperationWarning("Can't find dbxref "+ "to edit!");
	 * 
	 * ref.setDesc(newDbxref.getDesc());
	 * ref.setDatabase(newDbxref.getDatabase());
	 * ref.setType(newDbxref.getType()); } } else { if (!(o instanceof
	 * DbxrefedObject)) return new OperationWarning("Can't apply "+ "dbxref edit
	 * to "+ "non-dbxrefable object "+ item.getTarget()); DbxrefedObject target =
	 * (DbxrefedObject) o; if (isAdd) target.addDbxref(newDbxref); else if
	 * (isDel) target.removeDbxref(oldDbxref); else { Dbxref ref =
	 * TermUtil.findGeneralDbxref(target, oldDbxref); if (ref == null) return
	 * new OperationWarning("Can't find dbxref "+ "to edit!");
	 * 
	 * ref.setDesc(newDbxref.getDesc());
	 * ref.setDatabase(newDbxref.getDatabase());
	 * ref.setType(newDbxref.getType()); } } } return null; }
	 * 
	 * public OperationWarning reverse(DbxrefChangeHistoryItem item) { boolean
	 * isAdd = item.isAdd(); boolean isDel = item.isDel(); Synonym synonym =
	 * item.getSynonym(); Dbxref newDbxref = (Dbxref) item.getNewDbxref();
	 * Dbxref oldDbxref = (Dbxref) item.getOldDbxref(); IdentifiedObject o =
	 * getRealIDObject(item.getTarget());
	 * 
	 * if (synonym != null) { if (!(o instanceof SynonymedObject)) return new
	 * OperationWarning("Cannot apply synonym dbxref "+ "edit to non-synonymable
	 * object "+ item.getTarget()); SynonymedObject target = (SynonymedObject)
	 * o; Synonym s = TermUtil.findSynonym(target, synonym.privateid); if
	 * (isAdd) { s.removeDbxref(newDbxref); } else if (isDel) {
	 * s.addDbxref(oldDbxref); } else { Dbxref ref =
	 * s.findDbxref(oldDbxref.privateid); ref.setID(oldDbxref.getID());
	 * ref.setDesc(oldDbxref.getDesc());
	 * ref.setDatabase(oldDbxref.getDatabase());
	 * ref.setType(oldDbxref.getType()); } } else { boolean isDefEdit =
	 * (newDbxref != null && newDbxref.isDefRef()) || (oldDbxref != null &&
	 * oldDbxref.isDefRef()); if (isDefEdit) { if (!(o instanceof
	 * DefinedObject)) return new OperationWarning("Can't apply definition "+
	 * "dbxref edit to "+ "non-definable object "+ item.getTarget());
	 * DefinedObject target = (DefinedObject) o; if (isAdd)
	 * target.removeDefDbxref(newDbxref); else if (isDel)
	 * target.addDefDbxref(oldDbxref); else { Dbxref ref =
	 * TermUtil.findDefDbxref(target, oldDbxref); if (ref == null) return new
	 * OperationWarning("Can't find dbxref "+ "to edit!");
	 * 
	 * ref.setDesc(oldDbxref.getDesc());
	 * ref.setDatabase(oldDbxref.getDatabase());
	 * ref.setType(oldDbxref.getType()); } } else { if (!(o instanceof
	 * DbxrefedObject)) return new OperationWarning("Can't apply "+ "dbxref edit
	 * to "+ "non-dbxrefable object "+ item.getTarget()); DbxrefedObject target =
	 * (DbxrefedObject) o; if (isAdd) target.removeDbxref(newDbxref); else if
	 * (isDel) target.addDbxref(oldDbxref); else { Dbxref ref =
	 * TermUtil.findGeneralDbxref(target, oldDbxref); if (ref == null) return
	 * new OperationWarning("Can't find dbxref "+ "to edit!");
	 * 
	 * ref.setDesc(oldDbxref.getDesc());
	 * ref.setDatabase(oldDbxref.getDatabase());
	 * ref.setType(oldDbxref.getType()); } } } return null; }
	 */

	public OperationWarning reverse(HistoryItem item) {
		OperationWarning warning;

		if (item instanceof CreateObjectHistoryItem)
			warning = reverse((CreateObjectHistoryItem) item);
		else if (item instanceof CreateLinkHistoryItem)
			warning = reverse((CreateLinkHistoryItem) item);
		else if (item instanceof DeleteLinkHistoryItem)
			warning = reverse((DeleteLinkHistoryItem) item);
		else if (item instanceof DestroyObjectHistoryItem)
			warning = reverse((DestroyObjectHistoryItem) item);
		else if (item instanceof ObsoleteObjectHistoryItem)
			warning = reverse((ObsoleteObjectHistoryItem) item);
		else if (item instanceof TermMacroHistoryItem)
			warning = reverse((TermMacroHistoryItem) item);
		else if (item instanceof LinkTypeHistoryItem)
			warning = reverse((LinkTypeHistoryItem) item);
		else if (item instanceof NamespaceHistoryItem)
			warning = reverse((NamespaceHistoryItem) item);
		else if (item instanceof NameChangeHistoryItem)
			warning = reverse((NameChangeHistoryItem) item);
		else if (item instanceof DefinitionChangeHistoryItem)
			warning = reverse((DefinitionChangeHistoryItem) item);
		else if (item instanceof AddSynonymHistoryItem)
			warning = reverse((AddSynonymHistoryItem) item);
		else if (item instanceof DelSynonymHistoryItem)
			warning = reverse((DelSynonymHistoryItem) item);
		else if (item instanceof TermCategoryHistoryItem)
			warning = reverse((TermCategoryHistoryItem) item);
		else if (item instanceof SynonymCategoryHistoryItem)
			warning = reverse((SynonymCategoryHistoryItem) item);
		else if (item instanceof ChangeSynCategoryHistoryItem)
			warning = reverse((ChangeSynCategoryHistoryItem) item);
		else if (item instanceof ChangeSynScopeHistoryItem)
			warning = reverse((ChangeSynScopeHistoryItem) item);
		else if (item instanceof TermNamespaceHistoryItem)
			warning = reverse((TermNamespaceHistoryItem) item);
		else if (item instanceof AddDbxrefHistoryItem)
			warning = reverse((AddDbxrefHistoryItem) item);
		else if (item instanceof DelDbxrefHistoryItem)
			warning = reverse((DelDbxrefHistoryItem) item);
		else if (item instanceof CommentChangeHistoryItem)
			warning = reverse((CommentChangeHistoryItem) item);
		else if (item instanceof CategoryChangeHistoryItem)
			warning = reverse((CategoryChangeHistoryItem) item);
		else if (item instanceof SecondaryIDHistoryItem)
			warning = reverse((SecondaryIDHistoryItem) item);
		else if (item instanceof NecessarilyTrueHistoryItem)
			warning = reverse((NecessarilyTrueHistoryItem) item);
		else if (item instanceof InverseNecHistoryItem)
			warning = reverse((InverseNecHistoryItem) item);
		else if (item instanceof CardinalityHistoryItem)
			warning = reverse((CardinalityHistoryItem) item);
		else if (item instanceof CompletesHistoryItem)
			warning = reverse((CompletesHistoryItem) item);
		else if (item instanceof TRNamespaceHistoryItem)
			warning = reverse((TRNamespaceHistoryItem) item);
		else if (item instanceof RangeHistoryItem)
			warning = reverse((RangeHistoryItem) item);
		else if (item instanceof DomainHistoryItem)
			warning = reverse((DomainHistoryItem) item);
		else if (item instanceof AddReplacementHistoryItem)
			warning = reverse((AddReplacementHistoryItem) item);
		else if (item instanceof RemoveReplacementHistoryItem)
			warning = reverse((RemoveReplacementHistoryItem) item);
		else if (item instanceof CyclicHistoryItem)
			warning = reverse((CyclicHistoryItem) item);
		else if (item instanceof SymmetricHistoryItem)
			warning = reverse((SymmetricHistoryItem) item);
		else if (item instanceof TransitiveHistoryItem)
			warning = reverse((TransitiveHistoryItem) item);
		else if (item instanceof CardinalityHistoryItem)
			warning = reverse((CardinalityHistoryItem) item);
		else if (item instanceof MinCardinalityHistoryItem)
			warning = reverse((MinCardinalityHistoryItem) item);
		else if (item instanceof MaxCardinalityHistoryItem)
			warning = reverse((MaxCardinalityHistoryItem) item);
		else if (item instanceof AddConsiderHistoryItem)
			warning = reverse((AddConsiderHistoryItem) item);
		else if (item instanceof RemoveConsiderHistoryItem)
			warning = reverse((RemoveConsiderHistoryItem) item);
		else if (item instanceof AddPropertyValueHistoryItem)
			warning = reverse((AddPropertyValueHistoryItem) item);
		else if (item instanceof DeletePropertyValueHistoryItem)
			warning = reverse((DeletePropertyValueHistoryItem) item);
		else
			warning = new OperationWarning("Unknown history item found!");
		for (OperationModel lockstepModel : lockstepModels) {
			OperationWarning lwarning = lockstepModel.reverse(item);
			if (lwarning != null)
				warning.addWarning(lwarning);
		}
		return warning;
	}

	public void addLockstepModel(OperationModel model) {
		lockstepModels.add(model);
	}

	public void removeLockstepModel(OperationModel model) {
		lockstepModels.remove(model);
	}

}
