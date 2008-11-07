package org.obo.history;

import java.util.*;
import java.io.Serializable;

import org.bbop.util.*;
import org.obo.datamodel.SubsetObject;
import org.obo.datamodel.CommentedObject;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.DbxrefedObject;
import org.obo.datamodel.DefinedObject;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.MultiIDObject;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.ObsoletableObject;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.SynonymType;
import org.obo.datamodel.SynonymedObject;
import org.obo.datamodel.TermSubset;
import org.obo.datamodel.impl.*;
import org.obo.history.*;
import org.obo.util.HistoryUtil;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class HistoryGenerator implements Serializable {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(HistoryGenerator.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected static boolean checkInterface(IdentifiedObject io,
			HistoryList history, Class checkme, Collection<String> warnings) {
		if (!checkme.isInstance(io)) {
			if (warnings != null)
				warnings.add(io + " should be an instance of " + checkme
						+ ", but it is not.");
			return false;
		}
		return true;
	}

	public static SessionHistoryList getHistory(OBOSession oldHistory,
			OBOSession newHistory) {
		return getHistory(oldHistory, newHistory, null);
	}

	public static SessionHistoryList getHistory(OBOSession oldHistory,
			OBOSession newHistory,
			Collection<String> warnings) {
		Iterator it;

		SessionHistoryList history = new DefaultHistoryList();

		// find namespace collection changes
		it = oldHistory.getNamespaces().iterator();
		while (it.hasNext()) {
			Namespace n = (Namespace) it.next();
			Namespace newn = HistoryUtil.findNamespace(n, newHistory);
			if (newn == null) {
				TermNamespaceHistoryItem item = new TermNamespaceHistoryItem(n
						.getID(), null, false, true);
				history.addItem(item);
			}
		}
		it = newHistory.getNamespaces().iterator();
		while (it.hasNext()) {
			Namespace n = (Namespace) it.next();
			Namespace oldn = HistoryUtil.findNamespace(n, oldHistory);
			if (oldn == null) {
				TermNamespaceHistoryItem item = new TermNamespaceHistoryItem(
						null, n.getID(), true, false);
				history.addItem(item);
			}
		}

		// find category collection changes
		it = oldHistory.getSubsets().iterator();
		while (it.hasNext()) {
			TermSubset cat = (TermSubset) it.next();
			if (!newHistory.getSubsets().contains(cat)) {
				TermSubsetHistoryItem item = new TermSubsetHistoryItem(cat,
						null, false, true);
				history.addItem(item);
			} else {
				TermSubset cat2 = HistoryUtil.findCategory(cat, newHistory);
				if (!cat.getDesc().equals(cat2.getDesc())) {
					TermSubsetHistoryItem item = new TermSubsetHistoryItem(
							cat, cat2, false, false);
					history.addItem(item);
				}
			}
		}
		it = newHistory.getSubsets().iterator();
		while (it.hasNext()) {
			TermSubset cat = (TermSubset) it.next();
			if (!oldHistory.getSubsets().contains(cat)) {
				TermSubsetHistoryItem item = new TermSubsetHistoryItem(
						null, cat, true, false);
				history.addItem(item);
			}
		}

		// find synonym category collection changes
		it = oldHistory.getSynonymTypes().iterator();
		while (it.hasNext()) {
			SynonymType cat = (SynonymType) it.next();
			if (!newHistory.getSynonymTypes().contains(cat)) {
				SynonymTypeHistoryItem item = new SynonymTypeHistoryItem(
						cat, null, false, true);
				history.addItem(item);
			} else {
				SynonymType cat2 = HistoryUtil.findSynonymCategory(cat,
						newHistory);
				if (!cat.getName().equals(cat2.getName())
						|| cat.getScope() != cat2.getScope()) {
					SynonymTypeHistoryItem item = new SynonymTypeHistoryItem(
							cat, cat2, false, false);
					history.addItem(item);
				}
			}
		}
		it = newHistory.getSynonymTypes().iterator();
		while (it.hasNext()) {
			SynonymType cat = (SynonymType) it.next();
			if (!oldHistory.getSynonymTypes().contains(cat)) {
				SynonymTypeHistoryItem item = new SynonymTypeHistoryItem(
						null, cat, true, false);
				history.addItem(item);
			}
		}

		List newObjects = new LinkedList();

		// if an object is in the new history, but not in the old history,
		// it has been added
		it = newHistory.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			IdentifiedObject oldio = oldHistory.getObject(io.getID());
			if (oldio == null) {
				history.addItem(new CreateObjectHistoryItem(io.getID(), io
						.getType().getID()));
				newObjects.add(io);
			}
		}

		it = newObjects.iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			IdentifiedObject oldio = oldHistory.getObject(io.getID());
			HistoryList creationList = new DefaultHistoryList();
			if (io.getType() instanceof OBOClass) {
				IdentifiedObject blankTerm = oldHistory.getObjectFactory()
						.createObject(io.getID(), (OBOClass) io.getType(),
								io.isAnonymous());

				getTermTextChanges(blankTerm, io, creationList, warnings);
				getParentageChanges(blankTerm, io, creationList, warnings);
				getObsoleteChanges(blankTerm, io, creationList, warnings);
				getNamespaceChanges(blankTerm, io, creationList, warnings);
			}

			if (creationList.size() > 1) {
				TermMacroHistoryItem item = new TermMacroHistoryItem(
						"Populated new object " + io.getID());
				item.setHistoryList(creationList);
				history.addItem(item);
			} else
				history.addItem(creationList.getItemAt(0));
		}

		it = oldHistory.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			IdentifiedObject newio = newHistory.getObject(io.getID());

			// if an object is in the old history, but not in the new history,
			// it has been destroyed
			if (newio == null) {
				if (io instanceof LinkedObject) {
					LinkedObject lo = (LinkedObject) io;
					Iterator it2 = lo.getParents().iterator();
					while (it2.hasNext()) {
						Link link = (Link) it2.next();
						history.addItem(new DeleteLinkHistoryItem(link));
					}
//					it2 = lo.getChildren().iterator();
//					while (it2.hasNext()) {
//						Link link = (Link) it2.next();
//						history.addItem(new DeleteLinkHistoryItem(link));
//					}
				}
				history.addItem(new DestroyObjectHistoryItem(io));
				continue;
			}

			if (!io.getType().equals(newio.getType()) && warnings != null)
				warnings.add(io
						+ " changed type between versions of this file!");

			getTermTextChanges(io, newio, history, warnings);

			getParentageChanges(io, newio, history, warnings);

			getObsoleteChanges(io, newio, history, warnings);

		}

		return history;
	}

	public static void getChanges(IdentifiedObject io, IdentifiedObject newio,
			HistoryList changes) {
		getChanges(io, newio, changes);
	}

	public static void getChanges(IdentifiedObject io, IdentifiedObject newio,
			HistoryList changes, Collection<String> warnings) {
		getParentageChanges(io, newio, changes, warnings);
		getObsoleteChanges(io, newio, changes, warnings);
		getTermTextChanges(io, newio, changes, warnings);
		getNamespaceChanges(io, newio, changes, warnings);
	}

	public static HistoryList getChanges(IdentifiedObject io,
			IdentifiedObject newio) {
		return getChanges(io, newio, (Collection<String>) null);
	}

	public static HistoryList getChanges(IdentifiedObject io,
			IdentifiedObject newio, Collection<String> warnings) {
		HistoryList changes = new DefaultHistoryList();
		getChanges(io, newio, changes, warnings);
		return changes;
	}

	public static void getParentageChanges(IdentifiedObject io,
			IdentifiedObject newio, HistoryList history,
			Collection<String> warnings) {
		// check parents
		if (checkInterface(io, history, LinkedObject.class, warnings)
				&& checkInterface(newio, history, LinkedObject.class, warnings)) {
			LinkedObject lio = (LinkedObject) io;
			LinkedObject lnewio = (LinkedObject) newio;

			// checked for removed & changed parents
			Iterator it2 = lio.getParents().iterator();
			while (it2.hasNext()) {
				Link link = (Link) it2.next();
				//Link foundLink = HistoryUtil.findParentRelNoIntersection(link,lnewio);
				Link foundLink = HistoryUtil.findParentRel(link,lnewio);
				if (foundLink == null) {
					history.addItem(new DeleteLinkHistoryItem(link));
				} else {
					if (!ObjectUtil.equals(link.getNamespace(), foundLink
							.getNamespace())) {
						history.addItem(new TRNamespaceHistoryItem(link,
								foundLink.getNamespace()));
					}

					if ((link instanceof OBORestriction) != (foundLink instanceof OBORestriction)
							&& warnings != null) {
						warnings.add(link + " changed interface types!");
					}

					if (link instanceof OBORestriction) {
						OBORestriction rlink = (OBORestriction) link;
						OBORestriction rfoundLink = (OBORestriction) foundLink;

						if (!ObjectUtil.equals(rlink.getCardinality(),
								rfoundLink.getCardinality())) {
							history.addItem(new CardinalityHistoryItem(rlink,
									rfoundLink.getCardinality()));
						}
						if (!ObjectUtil.equals(rlink.getMaxCardinality(),
								rfoundLink.getMaxCardinality())) {
							history.addItem(new MaxCardinalityHistoryItem(
									rlink, rfoundLink.getMaxCardinality()));
						}
						if (!ObjectUtil.equals(rlink.getMinCardinality(),
								rfoundLink.getMinCardinality())) {
							history.addItem(new MinCardinalityHistoryItem(
									rlink, rfoundLink.getMinCardinality()));
						}
						if (rlink.isNecessarilyTrue() != rfoundLink
								.isNecessarilyTrue()) {
							history.addItem(new NecessarilyTrueHistoryItem(
									rlink));
						}
						if (rlink.isInverseNecessarilyTrue() != rfoundLink
								.isInverseNecessarilyTrue()) {
							history.addItem(new InverseNecHistoryItem(rlink));
						}
						if (rlink.completes() != rfoundLink.completes()) {
							history.addItem(new CompletesHistoryItem(rlink));
						}
					}
				}
			}

			// check for added parents
			it2 = lnewio.getParents().iterator();
			while (it2.hasNext()) {
				Link link = (Link) it2.next();
				Link foundLink = HistoryUtil.findParentRelNoIntersection(link,
						lio);
				// if a parent is in the new term, but not in the old,
				// it has been added
				if (foundLink == null) {
					history.addItem(new CreateLinkHistoryItem(link.getChild(),
							link.getType(), link.getParent()));
					OBORestriction newLink = new OBORestrictionImpl(link
							.getChild(), link.getType(), link.getParent());
					if (link.getNamespace() != null)
						history.addItem(new TRNamespaceHistoryItem(
								new StringRelationship(link), null,
								link.getNamespace().getID()));

					if (link instanceof OBORestriction) {
						OBORestriction rlink = (OBORestriction) link;
						if (rlink.getCardinality() != null)
							history.addItem(new CardinalityHistoryItem(newLink,
									rlink.getCardinality()));
						if (rlink.getMaxCardinality() != null)
							history.addItem(new MaxCardinalityHistoryItem(
									newLink, rlink.getMaxCardinality()));
						if (rlink.getMinCardinality() != null)
							history.addItem(new MinCardinalityHistoryItem(
									newLink, rlink.getMinCardinality()));
						if (!rlink.isNecessarilyTrue())
							history
									.addItem(new NecessarilyTrueHistoryItem(
											new StringRelationship(
													newLink), true));
						if (rlink.isInverseNecessarilyTrue())
							history
									.addItem(new InverseNecHistoryItem(
											new StringRelationship(
													newLink), false));
						if (rlink.completes())
							history
									.addItem(new CompletesHistoryItem(
											new StringRelationship(
													newLink), false));
					}
				}
			}
		}
	}

	public static void getObsoleteChanges(IdentifiedObject io,
			IdentifiedObject newio, HistoryList history,
			Collection<String> warnings) {
		if (TermUtil.isObsolete(newio) && !TermUtil.isObsolete(io)) {
			if (!(newio instanceof ObsoletableObject) && warnings != null)
				warnings.add(newio + " is not an ObsoletableObject");
			else
				history.addItem(new ObsoleteObjectHistoryItem(io));
		} else if (!TermUtil.isObsolete(newio) && TermUtil.isObsolete(io)
				&& warnings != null) {
			warnings.add(newio + " was somehow unobsoleted!");
		} else {
			if (newio instanceof ObsoletableObject
					&& io instanceof ObsoletableObject) {
				ObsoletableObject oio = (ObsoletableObject) io;
				ObsoletableObject onewio = (ObsoletableObject) newio;
				CollectionChanges<ObsoletableObject> considerchanges = CollectionUtil
						.getChanges(oio.getConsiderReplacements(), onewio
								.getConsiderReplacements());

				for(ObsoletableObject oo : considerchanges.getAddedItems()) {
					history.addItem(new AddConsiderHistoryItem(oio, oo));
				}
				for(ObsoletableObject oo : considerchanges.getDeletedItems()) {
					history.addItem(new RemoveConsiderHistoryItem(oio, oo));
				}

				CollectionChanges<ObsoletableObject> replacementchanges = CollectionUtil
						.getChanges(oio.getReplacedBy(), onewio.getReplacedBy());
				for(ObsoletableObject oo : replacementchanges.getAddedItems()) {
					history.addItem(new AddReplacementHistoryItem(oio, oo));
				}
				for(ObsoletableObject oo : replacementchanges.getDeletedItems()) {
					history.addItem(new RemoveReplacementHistoryItem(oio, oo));
				}

			}
		}
	}

	public static void getNamespaceChanges(IdentifiedObject io,
			IdentifiedObject newio, HistoryList history,
			Collection<String> warnings) {
		if (!ObjectUtil.equals(io.getNamespace(), newio.getNamespace())) {
			history.addItem(new NamespaceHistoryItem(io, newio.getNamespace()));
		}
	}

	public static void getTermTextChanges(IdentifiedObject io,
			IdentifiedObject newio, HistoryList history,
			Collection<String> warnings) {
		getTermTextChanges(io, newio, history, false, warnings);
	}

	public static void getTermTextChanges(IdentifiedObject io,
			IdentifiedObject newio, HistoryList history,
			boolean ignoreSecondaryIDs, Collection<String> warnings) {

		// check for text edits
		if (io.getName() == null || newio.getName() == null ||
		    !io.getName().equals(newio.getName()))
			history.addItem(new NameChangeHistoryItem(io, newio.getName()));

		if (checkInterface(io, history, CommentedObject.class, warnings) != checkInterface(
				newio, history, CommentedObject.class, warnings)
				&& warnings != null) {
			warnings.add(io + " changed whether it is " + "a CommentedObject!");
			return;
		} else if (checkInterface(io, history, CommentedObject.class, warnings)) {
			CommentedObject cio = (CommentedObject) io;
			CommentedObject cnewio = (CommentedObject) newio;
			if (!ObjectUtil.equals(cio.getComment(), cnewio.getComment()))
				history.addItem(new CommentChangeHistoryItem(cio, cnewio
						.getComment()));
		}

		if (!ignoreSecondaryIDs) {
			if (checkInterface(io, history, MultiIDObject.class, warnings) != checkInterface(
					newio, history, MultiIDObject.class, warnings)
					&& warnings != null) {
				warnings.add(io + " changed whether it is "
						+ "a MultiIDObject!");
				return;
			} else if (checkInterface(io, history, MultiIDObject.class,
					warnings)) {
				MultiIDObject mio = (MultiIDObject) io;
				MultiIDObject cmio = (MultiIDObject) newio;

				Iterator it = mio.getSecondaryIDs().iterator();
				while (it.hasNext()) {
					String id = (String) it.next();
					if (!cmio.getSecondaryIDs().contains(id)) {
						history.addItem(new SecondaryIDHistoryItem(mio, id,
								true));
					}
				}
				it = cmio.getSecondaryIDs().iterator();
				while (it.hasNext()) {
					String id = (String) it.next();
					if (!mio.getSecondaryIDs().contains(id)) {
						history.addItem(new SecondaryIDHistoryItem(mio, id,
								false));
						// history.addItem(new DestroyObjectHistoryItem(mio));
					}
				}
			}
		}

		if (checkInterface(io, history, DefinedObject.class, warnings) != checkInterface(
				newio, history, DefinedObject.class, warnings)
				&& warnings != null) {
			warnings.add(io + " changed whether it is " + "a DefinedObject!");
			return;
		} else if (checkInterface(io, history, DefinedObject.class, warnings)) {
			DefinedObject dio = (DefinedObject) io;
			DefinedObject dnewio = (DefinedObject) newio;
			if (!ObjectUtil.equals(dio.getDefinition(), dnewio.getDefinition()))
				history.addItem(new DefinitionChangeHistoryItem(dio, dnewio
						.getDefinition()));
			Iterator it = dio.getDefDbxrefs().iterator();
			while (it.hasNext()) {
				Dbxref ref = (Dbxref) it.next();
				if (!dnewio.getDefDbxrefs().contains(ref))
					history.addItem(new DelDbxrefHistoryItem(io.getID(), ref,
							true, null));
			}
			it = dnewio.getDefDbxrefs().iterator();
			while (it.hasNext()) {
				Dbxref ref = (Dbxref) it.next();
				if (!dio.getDefDbxrefs().contains(ref)) {
					history.addItem(new AddDbxrefHistoryItem(io.getID(), ref,
							true, null));
				}
			}
		}

		if (checkInterface(io, history, DbxrefedObject.class, warnings) != checkInterface(
				newio, history, DbxrefedObject.class, warnings)
				&& warnings != null) {
			warnings.add(io + " changed whether it is a " + "DbxrefedObject");
		} else if (checkInterface(io, history, DbxrefedObject.class, warnings)) {
			DbxrefedObject dio = (DbxrefedObject) io;
			DbxrefedObject dnewio = (DbxrefedObject) newio;

			Iterator it = dio.getDbxrefs().iterator();
			while (it.hasNext()) {
				Dbxref ref = (Dbxref) it.next();
				if (!dnewio.getDbxrefs().contains(ref))
					history.addItem(new DelDbxrefHistoryItem(io.getID(), ref,
							false, null));
			}
			it = dnewio.getDbxrefs().iterator();
			while (it.hasNext()) {
				Dbxref ref = (Dbxref) it.next();
				if (!dio.getDbxrefs().contains(ref)) {
					history.addItem(new AddDbxrefHistoryItem(io.getID(), ref,
							false, null));
				}
			}
		}

		if (checkInterface(io, history, SubsetObject.class, warnings) != checkInterface(
				newio, history, SubsetObject.class, warnings)
				&& warnings != null) {
			warnings
					.add(io + " changed whether it is a " + "CategorizedObject");
		} else if (checkInterface(io, history, SubsetObject.class,
				warnings)) {
			SubsetObject cio = (SubsetObject) io;
			SubsetObject cnewio = (SubsetObject) newio;

			Iterator it = cio.getSubsets().iterator();
			while (it.hasNext()) {
				TermSubset termCategory = (TermSubset) it.next();

				if (!cnewio.getSubsets().contains(termCategory)) {
					SubsetChangeHistoryItem item = new SubsetChangeHistoryItem(
							termCategory.getName(), true, io.getID());
					history.addItem(item);
				}
			}
			it = cnewio.getSubsets().iterator();
			while (it.hasNext()) {
				TermSubset termCategory = (TermSubset) it.next();
				if (!cio.getSubsets().contains(termCategory)) {
					SubsetChangeHistoryItem item = new SubsetChangeHistoryItem(
							termCategory.getName(), false, io.getID());
					history.addItem(item);
				}
			}
		}

		if (checkInterface(io, history, SynonymedObject.class, warnings) != checkInterface(
				newio, history, SynonymedObject.class, warnings)
				&& warnings != null) {
			warnings.add(io + " changed whether it is a " + "SynonymedObject");
		} else if (checkInterface(io, history, SynonymedObject.class, warnings)) {
			SynonymedObject sio = (SynonymedObject) io;
			SynonymedObject snewio = (SynonymedObject) newio;

			Iterator it = sio.getSynonyms().iterator();
			while (it.hasNext()) {
				Synonym s = (Synonym) it.next();
				boolean found = false;
				Iterator it2 = snewio.getSynonyms().iterator();
				while (it2.hasNext()) {
					Synonym news = (Synonym) it2.next();
					if (news.getText().equals(s.getText())) {
						found = true;

						Iterator it3 = s.getDbxrefs().iterator();
						while (it3.hasNext()) {
							Dbxref oldref = (Dbxref) it3.next();
							if (!news.getDbxrefs().contains(oldref))
								history.addItem(new DelDbxrefHistoryItem(sio
										.getID(), oldref, false, s.getText()));
						}

						it3 = news.getDbxrefs().iterator();
						while (it3.hasNext()) {
							Dbxref newref = (Dbxref) it3.next();
							if (!s.getDbxrefs().contains(newref))
								history.addItem(new AddDbxrefHistoryItem(sio
										.getID(), newref, false, s.getText()));
						}

						if (!ObjectUtil.equals(news.getSynonymType(), s
								.getSynonymType())) {
							history.addItem(new ChangeSynTypeHistoryItem(
									sio, s, news.getSynonymType()));
						}

						if (news.getScope() != s.getScope()) {
							history.addItem(new ChangeSynScopeHistoryItem(io
									.getID(), s.getText(), s.getScope(), news
									.getScope()));
						}
						break;
					}
				}
				if (!found) {
					history.addItem(new DelSynonymHistoryItem(io.getID(), s
							.getText()));
				}
			}

			it = snewio.getSynonyms().iterator();
			while (it.hasNext()) {
				Synonym s = (Synonym) it.next();
				boolean found = false;
				Iterator it2 = sio.getSynonyms().iterator();
				while (it2.hasNext()) {
					Synonym olds = (Synonym) it2.next();
					if (olds.getText().equals(s.getText())) {
						found = true;

						break;
					}
				}
				if (!found) {
					history.addItem(new AddSynonymHistoryItem(io.getID(), s
							.getText()));
					it2 = s.getDbxrefs().iterator();
					while (it2.hasNext()) {
						Dbxref ref = (Dbxref) it2.next();
						history.addItem(new AddDbxrefHistoryItem(sio.getID(),
								ref, false, s.getText()));
					}
					if (s.getSynonymType() != null)
						history.addItem(new ChangeSynTypeHistoryItem(sio
								.getID(), s.getText(), null, s
								.getSynonymType().getID()));
					if (s.getScope() != Synonym.RELATED_SYNONYM)
						history.addItem(new ChangeSynScopeHistoryItem(io
								.getID(), s.getText(), Synonym.RELATED_SYNONYM,
								s.getScope()));

				}
			}
		}

		if (checkInterface(io, history, OBOProperty.class, warnings) != checkInterface(
				newio, history, OBOProperty.class, warnings)) {
			warnings.add(io + " changed whether it is " + "a OBOProperty!");
			return;
		} else if (checkInterface(io, history, OBOProperty.class, warnings)) {
			OBOProperty prop = (OBOProperty) io;
			OBOProperty newprop = (OBOProperty) newio;
			if (!ObjectUtil.equals(prop.getDomain(), newprop.getDomain()))
				history
						.addItem(new DomainHistoryItem(prop, newprop
								.getDomain()));

			if (!ObjectUtil.equals(prop.getRange(), newprop.getRange()))
				history.addItem(new RangeHistoryItem(prop, newprop.getRange()));

			if (prop.isTransitive() != newprop.isTransitive())
				history.addItem(new TransitiveHistoryItem(prop));

			if (prop.isSymmetric() != newprop.isSymmetric())
				history.addItem(new SymmetricHistoryItem(prop));

			if (prop.isCyclic() != newprop.isCyclic())
				history.addItem(new CyclicHistoryItem(prop));
		}
	}
}
