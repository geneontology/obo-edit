package org.obo.dataadapter;

import java.util.*;

import org.bbop.dataadapter.*;
import org.bbop.io.ProgressableInputStream;
import org.bbop.util.*;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.DefaultObjectFactory;
import org.obo.history.*;

import java.io.*;
import java.awt.Color;
import org.xml.sax.*;
import javax.xml.parsers.*;

public class XMLHistoryAdapter implements OBOEditAdapter {

	protected String path;

	protected AdapterConfiguration config;

	protected ProgressableInputStream pfis;

	protected boolean cancelled = false;

	protected List listeners = new Vector();

	protected static class BasicLinkedList {
		Object head;

		BasicLinkedList tail;

		static int idgen = 0;

		int id = idgen++;

		public Object getHead() {
			return head;
		}

		public BasicLinkedList getTail() {
			return tail;
		}

		public void setHead(Object head) {
			this.head = head;
		}

		public void setTail(BasicLinkedList tail) {
			this.tail = tail;
		}

		@Override
		public String toString() {
			return "BasicLinkedList id " + id;
		}
	}

	public DataAdapterUI getPreferredUI() {
		FileAdapterUI ui = new FileAdapterUI();
		ui.setReadOperation(OBOEditAdapter.READ_HISTORY);
		ui.setWriteOperation(OBOEditAdapter.WRITE_HISTORY);
		ui.setMultiSelectEnabled(true);
		return ui;
	}

	public void cancel() {
		try {
			cancelled = true;
			if (pfis != null)
				pfis.close();
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}

	public AdapterConfiguration getConfiguration() {
		return config;
	}

	protected class HistoryContentHandler implements ContentHandler {
		protected List histories;

		protected SessionHistoryList historyList;

		StringBuffer buffer;

		Stack tagStack;

		boolean errors = false;

		Stack objectStack;

		protected ObjectFactory objectFactory = new DefaultObjectFactory();

		protected Locator locator;

		public void setObjectFactory(ObjectFactory factory) {
			this.objectFactory = factory;
		}

		public HistoryContentHandler() {
		}

		public List getHistories() {
			return histories;
		}

		public HistoryList getHistoryList() {
			return historyList;
		}

		public void characters(char[] ch, int start, int length) {
			buffer.append(ch, start, length);
		}

		public void startDocument() {
			buffer = new StringBuffer();
			tagStack = new Stack();
			objectStack = new Stack();
			// objectStack.push(historyList);
		}

		public void startElement(String namespaceURI, String localName,
				String qName, Attributes atts) {
			// System.err.println("startElement: "+qName);
			buffer.delete(0, buffer.length());
			tagStack.push(qName);
			if (qName.equalsIgnoreCase("list")) {
				objectStack.push(new BasicLinkedList());
			} else if (qName.equalsIgnoreCase("histories")) {
				histories = new Vector();
				objectStack.push(histories);
			} else if (qName.equalsIgnoreCase("edithistory")) {
				historyList = new DefaultHistoryList();
				objectStack.push(historyList);
			} else if (qName.equalsIgnoreCase("TRNamespaceHistoryItem")) {
				objectStack.push(new TRNamespaceHistoryItem());
			} else if (qName.equalsIgnoreCase("CompletesHistoryItem")) {
				objectStack.push(new CompletesHistoryItem());
			} else if (qName.equalsIgnoreCase("AddDbxrefHistoryItem")) {
				objectStack.push(new AddDbxrefHistoryItem());
			} else if (qName.equalsIgnoreCase("AddConsiderHistoryItem")) {
				objectStack.push(new AddConsiderHistoryItem());
			} else if (qName.equalsIgnoreCase("RemoveConsiderHistoryItem")) {
				objectStack.push(new RemoveConsiderHistoryItem());
			} else if (qName.equalsIgnoreCase("AddReplacementHistoryItem")) {
				objectStack.push(new AddReplacementHistoryItem());
			} else if (qName.equalsIgnoreCase("RemoveReplacementHistoryItem")) {
				objectStack.push(new RemoveReplacementHistoryItem());
			} else if (qName.equalsIgnoreCase("DelDbxrefHistoryItem")) {
				objectStack.push(new DelDbxrefHistoryItem());
			} else if (qName.equalsIgnoreCase("AddSynonymHistoryItem")) {
				objectStack.push(new AddSynonymHistoryItem());
			} else if (qName.equalsIgnoreCase("DelSynonymHistoryItem")) {
				objectStack.push(new DelSynonymHistoryItem());
			} else if (qName.equalsIgnoreCase("DomainHistoryItem")) {
				objectStack.push(new DomainHistoryItem());
			} else if (qName.equalsIgnoreCase("RangeHistoryItem")) {
				objectStack.push(new RangeHistoryItem());
			} else if (qName.equalsIgnoreCase("SymmetricHistoryItem")) {
				objectStack.push(new SymmetricHistoryItem());
			} else if (qName.equalsIgnoreCase("TransitiveHistoryItem")) {
				objectStack.push(new TransitiveHistoryItem());
			} else if (qName.equalsIgnoreCase("CyclicHistoryItem")) {
				objectStack.push(new CyclicHistoryItem());
			} else if (qName.equalsIgnoreCase("CardinalityHistoryItem")) {
				objectStack.push(new CardinalityHistoryItem());
			} else if (qName.equalsIgnoreCase("MaxCardinalityHistoryItem")) {
				objectStack.push(new MaxCardinalityHistoryItem());
			} else if (qName.equalsIgnoreCase("MinCardinalityHistoryItem")) {
				objectStack.push(new MinCardinalityHistoryItem());
			} else if (qName.equalsIgnoreCase("InverseNecHistoryItem")) {
				objectStack.push(new InverseNecHistoryItem());
			} else if (qName.equalsIgnoreCase("SecondaryIDHistoryItem")) {
				objectStack.push(new SecondaryIDHistoryItem());
			} else if (qName.equalsIgnoreCase("NecessarilyTrueHistoryItem")) {
				objectStack.push(new NecessarilyTrueHistoryItem());
			} else if (qName.equalsIgnoreCase("CreateObjectHistoryItem")) {
				objectStack.push(new CreateObjectHistoryItem());
			} else if (qName.equalsIgnoreCase("DeleteLinkHistoryItem")) {
				objectStack.push(new DeleteLinkHistoryItem());
			} else if (qName.equalsIgnoreCase("DestroyObjectHistoryItem")) {
				objectStack.push(new DestroyObjectHistoryItem());
			} else if (qName.equalsIgnoreCase("ObsoleteObjectHistoryItem")) {
				objectStack.push(new ObsoleteObjectHistoryItem());
			} else if (qName.equalsIgnoreCase("TermCategoryHistoryItem")) {
				objectStack.push(new TermCategoryHistoryItem());
			} else if (qName.equalsIgnoreCase("SynonymCategoryHistoryItem")) {
				objectStack.push(new SynonymCategoryHistoryItem());
			} else if (qName.equalsIgnoreCase("TermCopyHistoryItem")) {
				objectStack.push(new CreateLinkHistoryItem());
			} else if (qName.equalsIgnoreCase("TermMoveHistoryItem")) {
				try {
					objectStack.push(new TermMoveHistoryItem());
				} catch (Throwable ex) {
					ex.printStackTrace();
				}
			} else if (qName.equalsIgnoreCase("TermNamespaceHistoryItem")) {
				objectStack.push(new TermNamespaceHistoryItem());
			} else if (qName.equalsIgnoreCase("Link")) {
				objectStack.push(new StringRelationship());
			} else if (qName.equalsIgnoreCase("LinkTypeHistoryItem")) {
				objectStack.push(new LinkTypeHistoryItem());
			} else if (qName.equalsIgnoreCase("TermMergeHistoryItem")) {
				objectStack.push(new TermMergeHistoryItem());
			} else if (qName.equalsIgnoreCase("TermSplitHistoryItem")) {
				objectStack.push(new TermSplitHistoryItem());
			} else if (qName.equalsIgnoreCase("NameChangeHistoryItem")) {
				objectStack.push(new NameChangeHistoryItem());
			} else if (qName.equalsIgnoreCase("NamespaceHistoryItem")) {
				objectStack.push(new NamespaceHistoryItem());
			} else if (qName.equalsIgnoreCase("DefinitionChangeHistoryItem")) {
				objectStack.push(new DefinitionChangeHistoryItem());
			} else if (qName.equalsIgnoreCase("CommentChangeHistoryItem")) {
				objectStack.push(new CommentChangeHistoryItem());
			} else if (qName.equalsIgnoreCase("CategoryChangeHistoryItem")) {
				objectStack.push(new CategoryChangeHistoryItem());
			} else if (qName.equalsIgnoreCase("ChangeSynCategoryHistoryItem")) {
				objectStack.push(new ChangeSynCategoryHistoryItem());
			} else if (qName.equalsIgnoreCase("ChangeSynScopeHistoryItem")) {
				objectStack.push(new ChangeSynScopeHistoryItem());
			} else if (qName.equalsIgnoreCase("TermMacroHistoryItem")) {
				objectStack.push(new TermMacroHistoryItem());
			} else if (qName.equalsIgnoreCase("Namespace")) {
				objectStack.push(new Namespace());
			} else if (qName.equalsIgnoreCase("Dbxref")) {
				objectStack.push(objectFactory.createDbxref("", "", "",
						Dbxref.UNKNOWN, null));
			} else if (qName.equalsIgnoreCase("Synonym")) {
				objectStack.push(objectFactory.createSynonym("",
						Synonym.RELATED_SYNONYM));
			} else if (qName.equalsIgnoreCase("TermCategory")) {
				objectStack.push(objectFactory.createCategory("", ""));
			} else if (qName.equalsIgnoreCase("SynonymCategory")) {
				objectStack.push(objectFactory.createSynonymCategory("", "",
						Synonym.UNKNOWN_SCOPE));
			}

		}

		public String getString() {
			return buffer.toString().trim();
		}

		public void endElement(String namespaceURI, String localName,
				String qName) {
			try {
				String tagName = (String) tagStack.pop();

				if (!tagName.equalsIgnoreCase(qName))
					System.err.println("Something is very wrong, " + tagName
							+ " does not match tag " + qName);

				if (tagName.equalsIgnoreCase("user")) {
					historyList.setUser(getString());
				} else if (tagName.equalsIgnoreCase("date")) {
					historyList.setDate(new Date(getString()));
				} else if (tagName.equalsIgnoreCase("version")) {
					historyList.setVersion(getString());
				} else if (tagName.equalsIgnoreCase("comment")) {
					historyList.setComment(getString());
				} else if (tagName.equalsIgnoreCase("edithistory")) {
					HistoryList hl = (HistoryList) objectStack.pop();
					List parentList = null;
					if (objectStack.size() > 0
							&& objectStack.peek() instanceof List) {
						parentList = (List) objectStack.peek();
					} else if (histories == null) {
						histories = new Vector();
						parentList = histories;
					} else {
						(new Exception("Unexpected condition"))
								.printStackTrace();
					}
					System.err.println("HISTORIES = " + histories);
					parentList.add(hl);
				} else if (tagName.equalsIgnoreCase("head")) {
					Object oldObj = objectStack.pop();

					System.err.println("peeked at object of type "
							+ objectStack.peek().getClass());
					BasicLinkedList listNode = (BasicLinkedList) objectStack
							.peek();

					listNode.setHead(oldObj);
				} else if (tagName.equalsIgnoreCase("tail")) {
					Object oldObj = objectStack.pop();
					Object peekObj = objectStack.peek();

					System.err.println("peekObj = "
							+ peekObj
							+ ", type = "
							+ peekObj.getClass()
							+ ", lineNum = "
							+ (locator == null ? "?" : locator.getLineNumber()
									+ ""));

					BasicLinkedList listNode = (BasicLinkedList) peekObj;
					listNode.setTail((BasicLinkedList) oldObj);
				} else if (tagName.equalsIgnoreCase("historyList")) {
					Object oldObj = objectStack.pop();
					Object parentObj = objectStack.peek();

					HistoryList parentList = (HistoryList) parentObj;
					BasicLinkedList list = (BasicLinkedList) oldObj;
					while (list != null) {
						System.err.println("head = "
								+ list.getHead().getClass());
						parentList.addItem((HistoryItem) list.getHead());
						list = list.getTail();
					}
				} else if (tagName.equalsIgnoreCase("editlist")) {
					Object oldObj = objectStack.pop();
					Object parentObj = objectStack.peek();

					TermMacroHistoryItem item = (TermMacroHistoryItem) parentObj;
					BasicLinkedList list = (BasicLinkedList) oldObj;
					while (list != null) {
						item.addItem((HistoryItem) list.getHead());
						list = list.getTail();
					}
				} else if (tagName.equalsIgnoreCase("desc")) {
					Object oldObj = objectStack.peek();
					TermMacroHistoryItem item = (TermMacroHistoryItem) oldObj;
					item.setDescription(getString());
				} else if (tagName.equalsIgnoreCase("macroresult")) {
					Object oldObj = objectStack.peek();
					TermMacroHistoryItem item = (TermMacroHistoryItem) oldObj;
					item.setResult(getString());
				} else if (tagName.equalsIgnoreCase("splitaddtype")) {
					Object oldObj = objectStack.peek();
					TermSplitHistoryItem item = (TermSplitHistoryItem) oldObj;
					item.setAddType(getString().equalsIgnoreCase("true"));
				} else if (tagName.equalsIgnoreCase("splitresult")) {
					Object oldObj = objectStack.peek();
					TermSplitHistoryItem item = (TermSplitHistoryItem) oldObj;
					item.setResult(getString());

				} else if (tagName.equalsIgnoreCase("syncatid")) {
					SynonymCategory cat = (SynonymCategory) objectStack.peek();
					cat.setID(getString());
				} else if (tagName.equalsIgnoreCase("syncatname")) {
					SynonymCategory cat = (SynonymCategory) objectStack.peek();
					cat.setName(getString());
				} else if (tagName.equalsIgnoreCase("syncatscope")) {
					SynonymCategory cat = (SynonymCategory) objectStack.peek();
					try {
						int scope = Integer.parseInt(getString());
						cat.setScope(scope);
					} catch (NumberFormatException ex) {
						System.err.println("couldn't parse scope "
								+ getString());
					}
				} else if (tagName.equalsIgnoreCase("oldSynCategory")) {
					ChangeSynCategoryHistoryItem item = (ChangeSynCategoryHistoryItem) objectStack
							.peek();
					item.setOldCategory(getString());
				} else if (tagName.equalsIgnoreCase("newSynCategory")) {
					ChangeSynCategoryHistoryItem item = (ChangeSynCategoryHistoryItem) objectStack
							.peek();
					item.setNewCategory(getString());
				} else if (tagName.equalsIgnoreCase("oldScope")) {
					ChangeSynScopeHistoryItem item = (ChangeSynScopeHistoryItem) objectStack
							.peek();
					try {
						int scope = Integer.parseInt(getString());
						item.setOldScope(scope);
					} catch (Exception ex) {
						System.err.println("bad scope " + getString());
					}
				} else if (tagName.equalsIgnoreCase("newScope")) {
					ChangeSynScopeHistoryItem item = (ChangeSynScopeHistoryItem) objectStack
							.peek();
					try {
						int scope = Integer.parseInt(getString());
						item.setNewScope(scope);
					} catch (Exception ex) {
						System.err.println("bad scope " + getString());
					}
				} else if (tagName.equalsIgnoreCase("oldcomment")) {
					Object oldObj = objectStack.peek();
					CommentChangeHistoryItem item = (CommentChangeHistoryItem) oldObj;
					item.setOldText(getString());
				} else if (tagName.equalsIgnoreCase("consider")) {
					Object oldObj = objectStack.peek();
					if (oldObj instanceof AddConsiderHistoryItem) {
						AddConsiderHistoryItem item = (AddConsiderHistoryItem) oldObj;
						item.setConsider(getString());
					} else if (oldObj instanceof RemoveConsiderHistoryItem) {
						RemoveConsiderHistoryItem item = (RemoveConsiderHistoryItem) oldObj;
						item.setConsider(getString());
					}
				} else if (tagName.equalsIgnoreCase("replacedby")) {
					Object oldObj = objectStack.peek();
					if (oldObj instanceof AddReplacementHistoryItem) {
						AddReplacementHistoryItem item = (AddReplacementHistoryItem) oldObj;
						item.setReplace(getString());
					} else {
						RemoveReplacementHistoryItem item = (RemoveReplacementHistoryItem) oldObj;
						item.setReplace(getString());
					}
					/*
					 * SetReplacementHistoryItem item =
					 * (SetReplacementHistoryItem) oldObj;
					 * item.setReplacedBy(getString());
					 */
					/*
					 * } else if (tagName.equalsIgnoreCase("oldreplacedby")) {
					 * Object oldObj = objectStack.peek();
					 * SetReplacementHistoryItem item =
					 * (SetReplacementHistoryItem) oldObj;
					 * item.setOldReplacedBy(getString());
					 */
				} else if (tagName.equalsIgnoreCase("newcomment")) {
					Object oldObj = objectStack.peek();
					CommentChangeHistoryItem item = (CommentChangeHistoryItem) oldObj;
					item.setNewText(getString());
				} else if (tagName.equalsIgnoreCase("oldtext")) {
					Object oldObj = objectStack.peek();
					NameChangeHistoryItem item = (NameChangeHistoryItem) oldObj;
					item.setOldText(getString());
				} else if (tagName.equalsIgnoreCase("newtext")) {
					Object oldObj = objectStack.peek();
					NameChangeHistoryItem item = (NameChangeHistoryItem) oldObj;
					item.setNewText(getString());
				} else if (tagName.equalsIgnoreCase("objectid")) {
					CreateObjectHistoryItem item = (CreateObjectHistoryItem) objectStack
							.peek();
					item.setObjectID(getString());
				} else if (tagName.equalsIgnoreCase("typeid")) {
					CreateObjectHistoryItem item = (CreateObjectHistoryItem) objectStack
							.peek();
					item.setTypeID(getString());
				} else if (tagName.equalsIgnoreCase("olddef")) {
					Object oldObj = objectStack.peek();
					DefinitionChangeHistoryItem item = (DefinitionChangeHistoryItem) oldObj;
					item.setOldText(getString());
				} else if (tagName.equalsIgnoreCase("newdef")) {
					Object oldObj = objectStack.peek();
					DefinitionChangeHistoryItem item = (DefinitionChangeHistoryItem) oldObj;
					item.setNewText(getString());
				} else if (tagName.equalsIgnoreCase("oldcat")) {
					TermCategory cat = (TermCategory) objectStack.pop();
					TermCategoryHistoryItem item = (TermCategoryHistoryItem) objectStack
							.peek();
					item.setOldCat(cat);
				} else if (tagName.equalsIgnoreCase("newcat")) {
					TermCategory cat = (TermCategory) objectStack.pop();
					TermCategoryHistoryItem item = (TermCategoryHistoryItem) objectStack
							.peek();
					item.setNewCat(cat);
				} else if (tagName.equalsIgnoreCase("catname")) {
					TermCategory cat = (TermCategory) objectStack.peek();
					cat.setName(getString());
				} else if (tagName.equalsIgnoreCase("catdesc")) {
					TermCategory cat = (TermCategory) objectStack.peek();
					cat.setDesc(getString());
				} else if (tagName.equalsIgnoreCase("syntext")) {
					Synonym s = (Synonym) objectStack.peek();
					s.setText(getString());
				} else if (tagName.equalsIgnoreCase("syntype")) {
					Synonym s = (Synonym) objectStack.peek();
					int type = Synonym.RELATED_SYNONYM;
					String typeStr = getString();
					if (typeStr.equals("Exact"))
						type = Synonym.EXACT_SYNONYM;
					else if (typeStr.equals("Narrow"))
						type = Synonym.NARROW_SYNONYM;
					else if (typeStr.equals("Broad"))
						type = Synonym.BROAD_SYNONYM;
					s.setScope(type);
				} else if (tagName.equalsIgnoreCase("oldns")) {
					Object oldObj = objectStack.peek();
					NamespaceHistoryItem item = (NamespaceHistoryItem) oldObj;
					item.setOldNamespace(new Namespace(getString(), null));
				} else if (tagName.equalsIgnoreCase("newns")) {
					Object oldObj = objectStack.peek();
					NamespaceHistoryItem item = (NamespaceHistoryItem) oldObj;
					item.setNewNamespace(new Namespace(getString(), null));
				} else if (tagName.equalsIgnoreCase("category")) {
					Object oldObj = objectStack.peek();
					CategoryChangeHistoryItem item = (CategoryChangeHistoryItem) oldObj;
					item.setCategory(getString());
				} else if (tagName.equalsIgnoreCase("oldCardinality")) {
					Object oldObj = objectStack.peek();
					CardinalityHistoryItem item = (CardinalityHistoryItem) oldObj;
					Integer val = null;
					try {
						val = new Integer(getString());
					} catch (NumberFormatException ex) {
					}
					item.setOldValue(val);
				} else if (tagName.equalsIgnoreCase("newCardinality")) {
					Object oldObj = objectStack.peek();
					CardinalityHistoryItem item = (CardinalityHistoryItem) oldObj;
					Integer val = null;
					try {
						val = new Integer(getString());
					} catch (NumberFormatException ex) {
					}
					item.setNewValue(val);
				} else if (tagName.equalsIgnoreCase("oldMaxCardinality")) {
					Object oldObj = objectStack.peek();
					MaxCardinalityHistoryItem item = (MaxCardinalityHistoryItem) oldObj;
					Integer val = null;
					try {
						val = new Integer(getString());
					} catch (NumberFormatException ex) {
					}
					item.setOldValue(val);
				} else if (tagName.equalsIgnoreCase("newMaxCardinality")) {
					Object oldObj = objectStack.peek();
					MaxCardinalityHistoryItem item = (MaxCardinalityHistoryItem) oldObj;
					Integer val = null;
					try {
						val = new Integer(getString());
					} catch (NumberFormatException ex) {
					}
					item.setNewValue(val);
				} else if (tagName.equalsIgnoreCase("oldMinCardinality")) {
					Object oldObj = objectStack.peek();
					MinCardinalityHistoryItem item = (MinCardinalityHistoryItem) oldObj;
					Integer val = null;
					try {
						val = new Integer(getString());
					} catch (NumberFormatException ex) {
					}
					item.setOldValue(val);
				} else if (tagName.equalsIgnoreCase("newMinCardinality")) {
					Object oldObj = objectStack.peek();
					MinCardinalityHistoryItem item = (MinCardinalityHistoryItem) oldObj;
					Integer val = null;
					try {
						val = new Integer(getString());
					} catch (NumberFormatException ex) {
					}
					item.setNewValue(val);
				} else if (tagName.equalsIgnoreCase("iscatdel")) {
					Object oldObj = objectStack.peek();
					TermCategoryHistoryItem item = (TermCategoryHistoryItem) oldObj;
					item.setIsDel(getString().equalsIgnoreCase("true"));
				} else if (tagName.equalsIgnoreCase("iscatadd")) {
					Object oldObj = objectStack.peek();
					TermCategoryHistoryItem item = (TermCategoryHistoryItem) oldObj;
					item.setIsAdd(getString().equalsIgnoreCase("true"));

				} else if (tagName.equalsIgnoreCase("issyncatadd")) {
					Object oldObj = objectStack.peek();
					SynonymCategoryHistoryItem item = (SynonymCategoryHistoryItem) oldObj;
					item.setIsAdd(getString().equalsIgnoreCase("true"));
				} else if (tagName.equalsIgnoreCase("issyncatdel")) {
					Object oldObj = objectStack.peek();
					SynonymCategoryHistoryItem item = (SynonymCategoryHistoryItem) oldObj;
					item.setIsDel(getString().equalsIgnoreCase("true"));

				} else if (tagName.equalsIgnoreCase("oldsyncat")) {
					SynonymCategory synCat = (SynonymCategory) objectStack
							.pop();
					Object oldObj = objectStack.peek();
					SynonymCategoryHistoryItem item = (SynonymCategoryHistoryItem) oldObj;
					item.setOldCat(synCat);
				} else if (tagName.equalsIgnoreCase("newsyncat")) {
					SynonymCategory synCat = (SynonymCategory) objectStack
							.pop();
					Object oldObj = objectStack.peek();
					SynonymCategoryHistoryItem item = (SynonymCategoryHistoryItem) oldObj;
					item.setNewCat(synCat);
				} else if (tagName.equalsIgnoreCase("istermcatdel")) {
					Object oldObj = objectStack.peek();
					CategoryChangeHistoryItem item = (CategoryChangeHistoryItem) oldObj;
					item.setIsDel(getString().equalsIgnoreCase("true"));
				} else if (tagName.equalsIgnoreCase("isNamespaceAdd")) {
					Object oldObj = objectStack.peek();
					TermNamespaceHistoryItem item = (TermNamespaceHistoryItem) oldObj;
					item.setIsAdd(getString().equalsIgnoreCase("true"));
				} else if (tagName.equalsIgnoreCase("isNamespaceDel")) {
					Object oldObj = objectStack.peek();
					TermNamespaceHistoryItem item = (TermNamespaceHistoryItem) oldObj;
					item.setIsDel(getString().equalsIgnoreCase("true"));
				} else if (tagName.equalsIgnoreCase("oldnamespace")) {
					Object oldObj = objectStack.peek();
					TermNamespaceHistoryItem item = (TermNamespaceHistoryItem) oldObj;
					item.setOldID(getString());
				} else if (tagName.equalsIgnoreCase("newnamespace")) {
					Object oldObj = objectStack.peek();
					TermNamespaceHistoryItem item = (TermNamespaceHistoryItem) oldObj;
					item.setNewID(getString());
				} else if (tagName.equalsIgnoreCase("oldNecessary")) {
					NecessarilyTrueHistoryItem item = (NecessarilyTrueHistoryItem) objectStack
							.peek();
					item.setOldNecessary(getString().equalsIgnoreCase("true"));
				} else if (tagName.equalsIgnoreCase("oldInvNecessary")) {
					InverseNecHistoryItem item = (InverseNecHistoryItem) objectStack
							.peek();
					item.setOldInverseNec(getString().equalsIgnoreCase("true"));
				} else if (tagName.equalsIgnoreCase("oldcompletes")) {
					CompletesHistoryItem item = (CompletesHistoryItem) objectStack
							.peek();
					item.setOldCompletes(getString().equalsIgnoreCase("true"));
				} else if (tagName.equalsIgnoreCase("oldcyclic")) {
					CyclicHistoryItem item = (CyclicHistoryItem) objectStack
							.peek();
					item.setOldCyclic(getString().equalsIgnoreCase("true"));
				} else if (tagName.equalsIgnoreCase("oldtransitive")) {
					TransitiveHistoryItem item = (TransitiveHistoryItem) objectStack
							.peek();
					item.setOldTransitive(getString().equalsIgnoreCase("true"));
				} else if (tagName.equalsIgnoreCase("oldsymmetric")) {
					SymmetricHistoryItem item = (SymmetricHistoryItem) objectStack
							.peek();
					item.setOldSymmetric(getString().equalsIgnoreCase("true"));
					System.err.println("got set old symmetric = "
							+ item.getOldSymmetric());
				} else if (tagName.equalsIgnoreCase("isDef")) {
					boolean isDef = getString().equalsIgnoreCase("true");
					HistoryItem item = (HistoryItem) objectStack.peek();
					if (item instanceof AddDbxrefHistoryItem) {
						((AddDbxrefHistoryItem) item).setDef(isDef);
					} else if (item instanceof DelDbxrefHistoryItem) {
						((DelDbxrefHistoryItem) item).setDef(isDef);
					}
				} else if (tagName.equalsIgnoreCase("target_xref")) {
					Dbxref ref = (Dbxref) objectStack.pop();
					HistoryItem item = (HistoryItem) objectStack.peek();
					if (item instanceof AddDbxrefHistoryItem)
						((AddDbxrefHistoryItem) item).setDbxref(ref);
					else if (item instanceof DelDbxrefHistoryItem)
						((DelDbxrefHistoryItem) item).setDbxref(ref);

				} else if (tagName.equalsIgnoreCase("target_synonym")) {
					String syn = getString();

					HistoryItem item = (HistoryItem) objectStack.peek();
					if (item instanceof AddDbxrefHistoryItem)
						((AddDbxrefHistoryItem) item).setSynonym(syn);
					else if (item instanceof DelDbxrefHistoryItem)
						((DelDbxrefHistoryItem) item).setSynonym(syn);
				} else if (tagName.equalsIgnoreCase("secondarydelete")) {
					SecondaryIDHistoryItem item = (SecondaryIDHistoryItem) objectStack
							.peek();
					item.setDelete(getString().equalsIgnoreCase("true"));
				} else if (tagName.equalsIgnoreCase("secondaryid")) {
					SecondaryIDHistoryItem item = (SecondaryIDHistoryItem) objectStack
							.peek();
					item.setSecondaryID(getString());
				} else if (tagName.equalsIgnoreCase("slave")) {
					TermMergeHistoryItem item = (TermMergeHistoryItem) objectStack
							.peek();
					item.setSlave(getString());
				} else if (tagName.equalsIgnoreCase("domain")) {
					DomainHistoryItem item = (DomainHistoryItem) objectStack
							.peek();
					item.setDomain(getString());
				} else if (tagName.equalsIgnoreCase("olddomain")) {
					DomainHistoryItem item = (DomainHistoryItem) objectStack
							.peek();
					item.setOldDomain(getString());
				} else if (tagName.equalsIgnoreCase("oldtrns")) {
					TRNamespaceHistoryItem item = (TRNamespaceHistoryItem) objectStack
							.peek();
					item.setOldNamespace(getString());
				} else if (tagName.equalsIgnoreCase("newtrns")) {
					TRNamespaceHistoryItem item = (TRNamespaceHistoryItem) objectStack
							.peek();
					item.setNewNamespace(getString());
				} else if (tagName.equalsIgnoreCase("range")) {
					RangeHistoryItem item = (RangeHistoryItem) objectStack
							.peek();
					item.setRange(getString());
				} else if (tagName.equalsIgnoreCase("oldrange")) {
					RangeHistoryItem item = (RangeHistoryItem) objectStack
							.peek();
					item.setOldRange(getString());
				} else if (tagName.equalsIgnoreCase("newtype")) {
					LinkTypeHistoryItem item = (LinkTypeHistoryItem) objectStack
							.peek();
					item.setRelType(getString());
				} else if (tagName.equalsIgnoreCase("target")) {
					Object oldObj = objectStack.peek();
					if (oldObj instanceof HistoryItem) {
						HistoryItem item = (HistoryItem) oldObj;
						item.setTarget(getString());
					}
				} else if (tagName.equalsIgnoreCase("synonym_edit_target")) {
					Object oldObj = objectStack.peek();
					if (oldObj instanceof AddSynonymHistoryItem)
						((AddSynonymHistoryItem) oldObj)
								.setSynonym(getString());
					else if (oldObj instanceof DelSynonymHistoryItem)
						((DelSynonymHistoryItem) oldObj)
								.setSynonym(getString());
					else if (oldObj instanceof ChangeSynScopeHistoryItem)
						((ChangeSynScopeHistoryItem) oldObj)
								.setSynonym(getString());
					else if (oldObj instanceof ChangeSynCategoryHistoryItem)
						((ChangeSynCategoryHistoryItem) oldObj)
								.setSynonym(getString());
					else
						throw new RuntimeException(
								"unexpected condition encountered!");
				} else if (tagName.equalsIgnoreCase("sourcerel")) {
					Object oldObj = objectStack.pop();

					StringRelationship rel = (StringRelationship) oldObj;
					if (objectStack.peek() instanceof TermMoveHistoryItem) {
						TermMoveHistoryItem item = (TermMoveHistoryItem) objectStack
								.peek();
						item.setRel(rel);
					} else if (objectStack.peek() instanceof DeleteLinkHistoryItem) {
						DeleteLinkHistoryItem item = (DeleteLinkHistoryItem) objectStack
								.peek();
						item.setRel(rel);
					} else if (objectStack.peek() instanceof CardinalityHistoryItem) {
						CardinalityHistoryItem item = (CardinalityHistoryItem) objectStack
								.peek();
						item.setRel(rel);
					} else if (objectStack.peek() instanceof MaxCardinalityHistoryItem) {
						MaxCardinalityHistoryItem item = (MaxCardinalityHistoryItem) objectStack
								.peek();
						item.setRel(rel);
					} else if (objectStack.peek() instanceof MinCardinalityHistoryItem) {
						MinCardinalityHistoryItem item = (MinCardinalityHistoryItem) objectStack
								.peek();
						item.setRel(rel);
					} else if (objectStack.peek() instanceof CompletesHistoryItem) {
						CompletesHistoryItem item = (CompletesHistoryItem) objectStack
								.peek();
						item.setRel(rel);
					} else if (objectStack.peek() instanceof TRNamespaceHistoryItem) {
						TRNamespaceHistoryItem item = (TRNamespaceHistoryItem) objectStack
								.peek();
						item.setRel(rel);
					} else if (objectStack.peek() instanceof InverseNecHistoryItem) {
						InverseNecHistoryItem item = (InverseNecHistoryItem) objectStack
								.peek();
						item.setRel(rel);
					} else if (objectStack.peek() instanceof NecessarilyTrueHistoryItem) {
						NecessarilyTrueHistoryItem item = (NecessarilyTrueHistoryItem) objectStack
								.peek();
						item.setRel(rel);
					} else if (objectStack.peek() instanceof LinkTypeHistoryItem) {
						LinkTypeHistoryItem item = (LinkTypeHistoryItem) objectStack
								.peek();
						item.setRel(rel);
					} else {
						throw new RuntimeException(
								"found <sourcerel> tag for inappropriate history item: "
										+ objectStack.peek());
					}
				} else if (tagName.equalsIgnoreCase("database")) {
					Object oldObj = objectStack.peek();

					Dbxref dbx = (Dbxref) oldObj;
					dbx.setDatabase(getString());
				} else if (tagName.equalsIgnoreCase("dbxrefid")) {
					Object oldObj = objectStack.peek();

					Dbxref dbx = (Dbxref) oldObj;
					dbx.setDatabaseID(getString());
				} else if (tagName.equalsIgnoreCase("dbxrefdesc")) {
					Object oldObj = objectStack.peek();

					Dbxref dbx = (Dbxref) oldObj;
					dbx.setDesc(getString());
				} else if (tagName.equalsIgnoreCase("dbxreftype")) {
					Object oldObj = objectStack.peek();

					Dbxref dbx = (Dbxref) oldObj;
					String str = getString();
					int type = Dbxref.UNKNOWN;
					if (str.equals("anatomical"))
						type = Dbxref.ANATOMICAL;
					else if (str.equals("synonym"))
						type = Dbxref.RELATED_SYNONYM;
					else if (str.equals("definition"))
						type = Dbxref.DEFINITION;
					else if (str.equals("analog"))
						type = Dbxref.ANALOG;
					dbx.setType(type);
				} else if (tagName.equalsIgnoreCase("child")) {
					Object oldObj = objectStack.peek();

					StringRelationship rel = (StringRelationship) oldObj;
					rel.setChild(getString());
				} else if (tagName.equalsIgnoreCase("parent")) {
					Object oldObj = objectStack.peek();
					if (oldObj instanceof StringRelationship) {
						StringRelationship rel = (StringRelationship) oldObj;
						rel.setParent(getString());
					} else if (oldObj instanceof CreateLinkHistoryItem) {
						((CreateLinkHistoryItem) oldObj)
								.setParentID(getString());
					}
				} else if (tagName.equalsIgnoreCase("type")) {
					Object oldObj = objectStack.peek();
					if (oldObj instanceof StringRelationship) {
						StringRelationship rel = (StringRelationship) oldObj;
						rel.setType(getString());
					} else if (oldObj instanceof CreateLinkHistoryItem) {
						((CreateLinkHistoryItem) oldObj).setTypeID(getString());
					}
				}
			} catch (Throwable ex) {
				ex.printStackTrace();
			}
		}

		public void endDocument() {
		}

		public void endPrefixMapping(String prefix) {
		}

		public void ignorableWhitespace(char[] ch, int start, int length) {
		}

		public void processingInstruction(String target, String data) {
		}

		public void setDocumentLocator(Locator locator) {
			this.locator = locator;
		}

		public void skippedEntity(String name) {
		}

		public void startPrefixMapping(String prefix, String uri) {
		}
	}

	public Object doOperation(IOOperation op, AdapterConfiguration config,
			Object o) throws DataAdapterException {
		FileAdapterConfiguration fac = (FileAdapterConfiguration) config;
		if (op.equals(OBOEditAdapter.WRITE_HISTORY)) {
			try {
				this.config = config;
				cancelled = false;
				PrintStream stream = new PrintStream(new BufferedOutputStream(
						new FileOutputStream(fac.getWritePath())));

				if (o instanceof HistoryList) {
					dumpHistory(stream, (HistoryList) o);
				}
				stream.close();
				return null;
			} catch (IOException ex) {
				throw new DataAdapterException("Couldn't write file", ex);
			}
		} else if (op.equals(OBOEditAdapter.READ_HISTORY)) {
			try {
				SAXParserFactory spfactory = SAXParserFactory.newInstance();
				spfactory.setValidating(false);

				XMLReader reader = spfactory.newSAXParser().getXMLReader();
				HistoryContentHandler handler = new HistoryContentHandler();
				reader.setContentHandler(handler);
				String path = (String) fac.getReadPaths().iterator().next();
				reader.parse(new InputSource(new FileInputStream(path)));
				return handler.getHistories();
			} catch (Exception e) {
				e.printStackTrace();
				System.err.println(e.getMessage());
				return null;
			}
		} else
			throw new DataAdapterException("Operation " + op + " not supported");
	}

	public String getID() {
		return "OBOEDIT:XMLHistory";
	}

	public String getName() {
		return "OBO-Edit XML History Adapter";
	}

	public IOOperation[] getSupportedOperations() {
		IOOperation[] supported = { OBOEditAdapter.WRITE_HISTORY,
				OBOEditAdapter.READ_HISTORY };
		return supported;
	}

	public void dumpHistory(PrintStream stream, HistoryList historyList) {
		dumpHistory(stream, historyList, 0);
	}

	public void dumpHistories(PrintStream stream, List list, int indentLevel) {
		printLine(stream, indentLevel, "<histories>");
		Iterator it = list.iterator();
		while (it.hasNext()) {
			HistoryList hl = (HistoryList) it.next();
			dumpHistory(stream, hl, indentLevel + 2);
		}
		printLine(stream, indentLevel, "</histories>");
	}

	protected String escapeText(String text) {
		StringBuffer out = new StringBuffer();
		for (int i = 0; i < text.length(); i++) {
			if (text.charAt(i) == '>')
				out.append("&gt;");
			else if (text.charAt(i) == '<')
				out.append("&lt;");
			else if (text.charAt(i) == '&')
				out.append("&amp;");
			else
				out.append(text.charAt(i));
		}
		return out.toString();
	}

	protected void dumpHistory(PrintStream stream, HistoryList l,
			int indentLevel) {
		printLine(stream, indentLevel, "<edithistory>");
		if (l instanceof SessionHistoryList) {
			SessionHistoryList list = (SessionHistoryList) l;
			String user = list.getUser();
			if (user != null) {
				printLine(stream, indentLevel + 2, "<user>" + escapeText(user)
						+ "</user>");
			}

			printLine(stream, indentLevel + 2, "<date>"
					+ escapeText(list.getDate().toString()) + "</date>");
			if (list.getVersion() != null)
				printLine(stream, indentLevel + 2, "<version>"
						+ escapeText(list.getVersion()) + "</version>");
			if (list.getComment() != null)
				printLine(stream, indentLevel + 2, "<comment>"
						+ escapeText(list.getComment()) + "</comment>");
		}
		printLine(stream, indentLevel + 2, "<historyList>");
		dumpHistoryItems(stream, l, indentLevel + 4);
		printLine(stream, indentLevel + 2, "</historyList>");
		printLine(stream, indentLevel, "</edithistory>");
	}

	protected void dumpHistoryItems(PrintStream stream,
			HistoryList historyList, int indentLevel) {
		if (historyList.size() == 0)
			return;
		Iterator it = historyList.getHistoryItems();
		printLine(stream, indentLevel, "<list>");
		printLine(stream, indentLevel + 2, "<head>");
		for (int i = 0; i < historyList.size(); i++) {
			HistoryItem item = (HistoryItem) historyList.getItemAt(i);
			dumpItem(stream, item, indentLevel + 4);
			if (i < historyList.size() - 1)
				printLine(stream, indentLevel + 2,
						"</head> <tail> <list> <head>");
			else if (historyList.size() == 1)
				printLine(stream, indentLevel + 2, "</head>");
			else
				printLine(stream, indentLevel + 2, "</head> </list>");
		}
		StringBuffer closer = new StringBuffer();
		for (int i = 0; i < historyList.size() - 1; i++) {
			if (i != 0)
				closer.append(" ");
			if (i < historyList.size() - 2)
				closer.append("</tail> </list>");
			else
				closer.append("</tail>");
		}
		if (closer.length() > 0)
			printLine(stream, indentLevel + 2, closer.toString());
		printLine(stream, indentLevel, "</list>");
		/*
		 * 
		 * Object item = historyList.get(0); if (item instanceof HistoryItem)
		 * 
		 * 
		 * printLine(stream, indentLevel+2, "</head>"); List subList =
		 * historyList.subList(1, historyList.size()); if (subList.size() > 0) {
		 * printLine(stream, indentLevel+2, "<tail>"); dumpHistory(stream,
		 * subList, indentLevel+4); printLine(stream, indentLevel+2, "</tail>"); }
		 * printLine(stream, indentLevel, "</list>");
		 */
	}

	protected void dumpCategory(PrintStream stream, TermCategory cat,
			int indentLevel) {
		printLine(stream, indentLevel, "<TermCategory>");
		printLine(stream, indentLevel, "  <catname>"
				+ escapeText(cat.getName()) + "</catname>");
		printLine(stream, indentLevel, "  <catdesc>"
				+ escapeText(cat.getDesc()) + "</catdesc>");
		printLine(stream, indentLevel, "</TermCategory>");
	}

	protected void dumpSynonymCategory(PrintStream stream, SynonymCategory cat,
			int indentLevel) {
		printLine(stream, indentLevel, "<SynonymCategory>");
		printLine(stream, indentLevel + 2, "<syncatid>"
				+ escapeText(cat.getID()) + "</syncatid>");
		printLine(stream, indentLevel + 2, "<syncatname>"
				+ escapeText(cat.getName()) + "</syncatname>");
		printLine(stream, indentLevel + 2, "<syncatscope>" + cat.getScope()
				+ "</syncatscope>");
		printLine(stream, indentLevel, "</SynonymCategory>");
	}

	protected void dumpDbxref(PrintStream stream, Dbxref dbx, int indentLevel) {
		printLine(stream, indentLevel, "<dbxref>");
		printLine(stream, indentLevel + 2, "<database>"
				+ escapeText(dbx.getDatabase()) + "</database>");
		printLine(stream, indentLevel + 2, "<dbxrefid>"
				+ escapeText(dbx.getDatabaseID()) + "</dbxrefid>");
		if (dbx.getDesc() != null)
			printLine(stream, indentLevel + 2, "<dbxrefdesc>"
					+ escapeText(dbx.getDesc()) + "</dbxrefdesc>");

		String typeStr = "";
		if (dbx.getType() == Dbxref.ANATOMICAL)
			typeStr = "anatomical";
		else if (dbx.getType() == Dbxref.RELATED_SYNONYM)
			typeStr = "synonym";
		else if (dbx.getType() == Dbxref.DEFINITION)
			typeStr = "definition";
		else if (dbx.getType() == Dbxref.ANALOG)
			typeStr = "analog";
		else
			typeStr = "unknown";
		printLine(stream, indentLevel + 2, "<dbxreftype>" + escapeText(typeStr)
				+ "</dbxreftype>");
		printLine(stream, indentLevel, "</dbxref>");
	}

	protected void dumpRelationship(PrintStream stream,
			StringRelationship tr, int indentLevel) {
		printLine(stream, indentLevel, "<Link>");
		printLine(stream, indentLevel + 2, "<child>" + tr.getChild()
				+ "</child>");
		if (tr.getParent() != null)
			printLine(stream, indentLevel + 2, "<parent>"
					+ escapeText(tr.getParent()) + "</parent>");
		if (tr.getType() != null)
			printLine(stream, indentLevel + 2, "<type>"
					+ escapeText(tr.getType()) + "</type>");
		printLine(stream, indentLevel, "</Link>");
	}

	protected void dumpSynonym(PrintStream stream, Synonym s, int indentLevel) {
		printLine(stream, indentLevel, "<synonym>");
		printLine(stream, indentLevel + 2, "<syntext>"
				+ escapeText(s.getText()) + "</syntext>");
		String typeStr = "";
		if (s.getScope() == Synonym.RELATED_SYNONYM)
			typeStr = "Synonym";
		else if (s.getScope() == Synonym.EXACT_SYNONYM)
			typeStr = "Exact";
		else if (s.getScope() == Synonym.NARROW_SYNONYM)
			typeStr = "Narrow";
		else if (s.getScope() == Synonym.BROAD_SYNONYM)
			typeStr = "Broad";
		printLine(stream, indentLevel + 2, "<syntype>" + escapeText(typeStr)
				+ "</syntype>");
		printLine(stream, indentLevel, "</synonym>");
	}

	protected void dumpItem(PrintStream stream, HistoryItem hitem,
			int indentLevel) {
		if (hitem instanceof CardinalityHistoryItem) {
			CardinalityHistoryItem item = (CardinalityHistoryItem) hitem;
			printLine(stream, indentLevel, "<CardinalityHistoryItem>");
			printLine(stream, indentLevel + 2, "<sourcerel>");
			dumpRelationship(stream, item.getRel(), indentLevel + 4);
			printLine(stream, indentLevel + 2, "</sourcerel>");
			if (item.getOldValue() != null) {
				printLine(stream, indentLevel + 2, "<oldCardinality>");
				printLine(stream, indentLevel + 4, item.getOldValue() + "");
				printLine(stream, indentLevel + 2, "</oldCardinality>");
			}
			if (item.getNewValue() != null) {
				printLine(stream, indentLevel + 2, "<newCardinality>");
				printLine(stream, indentLevel + 4, item.getNewValue() + "");
				printLine(stream, indentLevel + 2, "</newCardinality>");
			}
			printLine(stream, indentLevel, "</CardinalityHistoryItem>");
		} else if (hitem instanceof MinCardinalityHistoryItem) {
			MinCardinalityHistoryItem item = (MinCardinalityHistoryItem) hitem;
			printLine(stream, indentLevel, "<MinCardinalityHistoryItem>");
			printLine(stream, indentLevel + 2, "<sourcerel>");
			dumpRelationship(stream, item.getRel(), indentLevel + 4);
			printLine(stream, indentLevel + 2, "</sourcerel>");
			printLine(stream, indentLevel + 2, "<oldMinCardinality>");
			printLine(stream, indentLevel + 4, item.getOldValue() + "");
			printLine(stream, indentLevel + 2, "</oldMinCardinality>");
			printLine(stream, indentLevel + 2, "<newMinCardinality>");
			printLine(stream, indentLevel + 4, item.getNewValue() + "");
			printLine(stream, indentLevel + 2, "</newMinCardinality>");
			printLine(stream, indentLevel, "</MinCardinalityHistoryItem>");
		} else if (hitem instanceof MaxCardinalityHistoryItem) {
			MaxCardinalityHistoryItem item = (MaxCardinalityHistoryItem) hitem;
			printLine(stream, indentLevel, "<MaxCardinalityHistoryItem>");
			printLine(stream, indentLevel + 2, "<sourcerel>");
			dumpRelationship(stream, item.getRel(), indentLevel + 4);
			printLine(stream, indentLevel + 2, "</sourcerel>");
			printLine(stream, indentLevel + 2, "<oldMaxCardinality>");
			printLine(stream, indentLevel + 4, item.getOldValue() + "");
			printLine(stream, indentLevel + 2, "</oldMaxCardinality>");
			printLine(stream, indentLevel + 2, "<newMaxCardinality>");
			printLine(stream, indentLevel + 4, item.getNewValue() + "");
			printLine(stream, indentLevel + 2, "</newMaxCardinality>");
			printLine(stream, indentLevel, "</MaxCardinalityHistoryItem>");
		} else if (hitem instanceof NecessarilyTrueHistoryItem) {
			NecessarilyTrueHistoryItem item = (NecessarilyTrueHistoryItem) hitem;
			printLine(stream, indentLevel, "<NecessarilyTrueHistoryItem>");
			printLine(stream, indentLevel + 2, "<sourcerel>");
			dumpRelationship(stream, item.getRel(), indentLevel + 4);
			printLine(stream, indentLevel + 2, "</sourcerel>");
			printLine(stream, indentLevel + 2, "<oldNecessary>");
			printLine(stream, indentLevel + 4, item.getOldNecessary() + "");
			printLine(stream, indentLevel + 2, "</oldNecessary>");
			printLine(stream, indentLevel, "</NecessarilyTrueHistoryItem>");
		} else if (hitem instanceof TermCategoryHistoryItem) {
			TermCategoryHistoryItem item = (TermCategoryHistoryItem) hitem;
			printLine(stream, indentLevel, "<TermCategoryHistoryItem>");
			if (item.getTarget() != null) {
				printLine(stream, indentLevel + 2, "<target>");
				printLine(stream, indentLevel + 4, escapeText(item.getTarget()));
				printLine(stream, indentLevel + 2, "</target>");
			}
			if (!item.isDel()) {
				printLine(stream, indentLevel + 2, "<newcat>");
				dumpCategory(stream, item.getNewCategory(), indentLevel + 4);
				printLine(stream, indentLevel + 2, "</newcat>");
			}
			if (!item.isAdd()) {
				printLine(stream, indentLevel + 2, "<oldcat>");
				dumpCategory(stream, item.getOldCategory(), indentLevel + 4);
				printLine(stream, indentLevel + 2, "</oldcat>");
			}
			if (item.isDel() != item.isAdd()) {
				if (item.isAdd())
					printLine(stream, indentLevel + 2,
							"<iscatadd>true</iscatadd>");
				if (item.isDel())
					printLine(stream, indentLevel + 2,
							"<iscatdel>true</iscatdel>");
			}
			printLine(stream, indentLevel, "</TermCategoryHistoryItem>");
		} else if (hitem instanceof CompletesHistoryItem) {
			CompletesHistoryItem item = (CompletesHistoryItem) hitem;
			printLine(stream, indentLevel, "<CompletesHistoryItem>");
			printLine(stream, indentLevel + 2, "<sourcerel>");
			dumpRelationship(stream, item.getRel(), indentLevel + 4);
			printLine(stream, indentLevel + 2, "</sourcerel>");
			printLine(stream, indentLevel + 2, "<oldCompletes>"
					+ item.getOldCompletes() + "</oldCompletes>");
			printLine(stream, indentLevel, "</CompletesHistoryItem>");
		} else if (hitem instanceof CreateLinkHistoryItem) {
			CreateLinkHistoryItem item = (CreateLinkHistoryItem) hitem;
			printLine(stream, indentLevel, "<TermCopyHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>");
			printLine(stream, indentLevel + 4, escapeText(item.getTarget()));
			printLine(stream, indentLevel + 2, "</target>");
			printLine(stream, indentLevel + 2, "<type>");
			printLine(stream, indentLevel + 4, escapeText(item.getTypeID()));
			printLine(stream, indentLevel + 2, "</type>");
			printLine(stream, indentLevel + 2, "<parent>");
			printLine(stream, indentLevel + 4, escapeText(item.getParentID()));
			printLine(stream, indentLevel + 2, "</parent>");
			printLine(stream, indentLevel, "</TermCopyHistoryItem>");
		} else if (hitem instanceof CyclicHistoryItem) {
			CyclicHistoryItem item = (CyclicHistoryItem) hitem;
			printLine(stream, indentLevel, "<CyclicHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>");
			printLine(stream, indentLevel + 4, escapeText(item.getTarget()));
			printLine(stream, indentLevel + 2, "</target>");
			printLine(stream, indentLevel + 2, "<oldCyclic>"
					+ item.getOldCyclic() + "</oldCyclic>");
			printLine(stream, indentLevel, "</CyclicHistoryItem>");
		} else if (hitem instanceof SymmetricHistoryItem) {
			SymmetricHistoryItem item = (SymmetricHistoryItem) hitem;
			printLine(stream, indentLevel, "<SymmetricHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>");
			printLine(stream, indentLevel + 4, escapeText(item.getTarget()));
			printLine(stream, indentLevel + 2, "</target>");
			printLine(stream, indentLevel + 2, "<oldSymmetric>"
					+ item.getOldSymmetric() + "</oldSymmetric>");
			printLine(stream, indentLevel, "</SymmetricHistoryItem>");
		} else if (hitem instanceof TransitiveHistoryItem) {
			TransitiveHistoryItem item = (TransitiveHistoryItem) hitem;
			printLine(stream, indentLevel, "<TransitiveHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>");
			printLine(stream, indentLevel + 4, escapeText(item.getTarget()));
			printLine(stream, indentLevel + 2, "</target>");
			printLine(stream, indentLevel + 2, "<oldTransitive>"
					+ item.getOldTransitive() + "</oldTransitive>");
			printLine(stream, indentLevel, "</TransitiveHistoryItem>");
		} else if (hitem instanceof DeleteLinkHistoryItem) {
			DeleteLinkHistoryItem item = (DeleteLinkHistoryItem) hitem;
			printLine(stream, indentLevel, "<DeleteLinkHistoryItem>");
			/**
			 * Make it possible to dump an entire term definition to fill in the
			 * "destroyed" tag
			 * 
			 */

			printLine(stream, indentLevel + 2, "<sourcerel>");
			dumpRelationship(stream, item.getRel(), indentLevel + 4);
			printLine(stream, indentLevel + 2, "</sourcerel>");
			printLine(stream, indentLevel, "</DeleteLinkHistoryItem>");
		} else if (hitem instanceof DomainHistoryItem) {
			DomainHistoryItem item = (DomainHistoryItem) hitem;
			printLine(stream, indentLevel, "<DomainHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>");
			printLine(stream, indentLevel + 4, escapeText(item.getTarget()));
			printLine(stream, indentLevel + 2, "</target>");
			if (item.getDomain() != null) {
				printLine(stream, indentLevel + 2, "<domain>");
				printLine(stream, indentLevel + 4, escapeText(item.getDomain()));
				printLine(stream, indentLevel + 2, "</domain>");
			}
			if (item.getOldDomain() != null) {
				printLine(stream, indentLevel + 2, "<oldDomain>");
				printLine(stream, indentLevel + 4, escapeText(item
						.getOldDomain()));
				printLine(stream, indentLevel + 2, "</oldDomain>");
			}
			printLine(stream, indentLevel, "</DomainHistoryItem>");
		} else if (hitem instanceof RangeHistoryItem) {
			RangeHistoryItem item = (RangeHistoryItem) hitem;
			printLine(stream, indentLevel, "<RangeHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>");
			printLine(stream, indentLevel + 4, escapeText(item.getTarget()));
			printLine(stream, indentLevel + 2, "</target>");
			if (item.getRange() != null) {
				printLine(stream, indentLevel + 2, "<range>");
				printLine(stream, indentLevel + 4, escapeText(item.getRange()));
				printLine(stream, indentLevel + 2, "</range>");
			}
			if (item.getOldRange() != null) {
				printLine(stream, indentLevel + 2, "<oldRange>");
				printLine(stream, indentLevel + 4, escapeText(item
						.getOldRange()));
				printLine(stream, indentLevel + 2, "</oldRange>");
			}
			printLine(stream, indentLevel, "</RangeHistoryItem>");
		} else if (hitem instanceof TermMergeHistoryItem) {
			TermMergeHistoryItem item = (TermMergeHistoryItem) hitem;
			printLine(stream, indentLevel, "<TermMergeHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>"
					+ escapeText(item.getTarget()) + "</target>");
			printLine(stream, indentLevel + 2, "<slave>"
					+ escapeText(item.getSlave()) + "</slave>");
			if (item.size() > 0) {
				printLine(stream, indentLevel + 2, "<editlist>");
				dumpHistory(stream, item, indentLevel + 4);
				printLine(stream, indentLevel + 2, "</editlist>");
			}
			printLine(stream, indentLevel, "</TermMergeHistoryItem>");
		} else if (hitem instanceof TermSplitHistoryItem) {
			TermSplitHistoryItem item = (TermSplitHistoryItem) hitem;
			printLine(stream, indentLevel, "<TermSplitHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>"
					+ escapeText(item.getTarget()) + "</target>");
			if (item.addType())
				printLine(stream, indentLevel + 2,
						"<splitAddType>true</splitAddType>");
			printLine(stream, indentLevel + 2, "<splitresult>"
					+ escapeText(item.getResult()) + "</splitresult>");
			if (item.size() > 0) {
				printLine(stream, indentLevel + 2, "<editlist>");
				dumpHistory(stream, item, indentLevel + 4);
				printLine(stream, indentLevel + 2, "</editlist>");
			}
			printLine(stream, indentLevel, "</TermSplitHistoryItem>");
		} else if (hitem instanceof TermMoveHistoryItem) {
			TermMoveHistoryItem item = (TermMoveHistoryItem) hitem;
			printLine(stream, indentLevel, "<TermMoveHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>"
					+ escapeText(item.getTarget()) + "</target>");
			printLine(stream, indentLevel + 2, "<sourcerel>");
			dumpRelationship(stream, item.getRelationship(), indentLevel + 4);
			printLine(stream, indentLevel + 2, "</sourcerel>");
			printLine(stream, indentLevel, "</TermMoveHistoryItem>");
		} else if (hitem instanceof InverseNecHistoryItem) {
			InverseNecHistoryItem item = (InverseNecHistoryItem) hitem;
			printLine(stream, indentLevel, "<InverseNecHistoryItem>");
			printLine(stream, indentLevel + 2, "<sourcerel>");
			dumpRelationship(stream, item.getRel(), indentLevel + 4);
			printLine(stream, indentLevel + 2, "</sourcerel>");
			printLine(stream, indentLevel + 2, "<oldInvNec>"
					+ item.getOldInverseNecessary() + "</oldInvNec>");
			printLine(stream, indentLevel, "</InverseNecHistoryItem>");
		} else if (hitem instanceof TermNamespaceHistoryItem) {

			TermNamespaceHistoryItem item = (TermNamespaceHistoryItem) hitem;
			printLine(stream, indentLevel, "<TermNamespaceHistoryItem>");
			/*
			 * printLine(stream, indentLevel+2, "<target>"); printLine(stream,
			 * indentLevel+4, escapeText(item.getTarget())); printLine(stream,
			 * indentLevel+2, "</target>");
			 */
			if (!item.isAdd() && item.getOldID() != null) {
				printLine(stream, indentLevel + 2, "<oldnamespace>");
				printLine(stream, indentLevel + 4, escapeText(item.getOldID()));
				printLine(stream, indentLevel + 2, "</oldnamespace>");
			}
			if (!item.isDel() && item.getNewID() != null) {
				printLine(stream, indentLevel + 2, "<newnamespace>");
				printLine(stream, indentLevel + 4, escapeText(item.getNewID()));
				printLine(stream, indentLevel + 2, "</newnamespace>");
			}
			if (item.isDel() != item.isAdd()) {
				if (item.isAdd())
					printLine(stream, indentLevel + 2,
							"<isNamespaceAdd>true</isNamespaceAdd>");
				if (item.isDel())
					printLine(stream, indentLevel + 2,
							"<isNamespaceDel>true</isNamespaceDel>");
			}
			printLine(stream, indentLevel, "</TermNamespaceHistoryItem>");
		} else if (hitem instanceof LinkTypeHistoryItem) {
			LinkTypeHistoryItem item = (LinkTypeHistoryItem) hitem;
			printLine(stream, indentLevel, "<LinkTypeHistoryItem>");
			printLine(stream, indentLevel + 2, "<sourcerel>");
			dumpRelationship(stream, item.getRel(), indentLevel + 4);
			printLine(stream, indentLevel + 2, "</sourcerel>");
			printLine(stream, indentLevel + 2, "<newType>"
					+ escapeText(item.getRelationshipType()) + "</newType>");
			printLine(stream, indentLevel, "</LinkTypeHistoryItem>");
		} else if (hitem instanceof SecondaryIDHistoryItem) {
			SecondaryIDHistoryItem item = (SecondaryIDHistoryItem) hitem;
			printLine(stream, indentLevel, "<SecondaryIDHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>");
			printLine(stream, indentLevel + 4, escapeText(item.getTarget()));
			printLine(stream, indentLevel + 2, "</target>");
			printLine(stream, indentLevel + 2, "<secondaryid>"
					+ escapeText(item.getSecondaryID()) + "</secondaryid>");
			if (item.isDelete()) {
				printLine(stream, indentLevel + 2,
						"<secondarydelete>true</secondarydelete>");
			}
			printLine(stream, indentLevel, "</SecondaryIDHistoryItem>");
		} else if (hitem instanceof TRNamespaceHistoryItem) {
			TRNamespaceHistoryItem item = (TRNamespaceHistoryItem) hitem;
			printLine(stream, indentLevel, "<TRNamespaceHistoryItem>");

			printLine(stream, indentLevel + 2, "<sourcerel>");
			dumpRelationship(stream, item.getRel(), indentLevel + 4);
			printLine(stream, indentLevel + 2, "</sourcerel>");

			if (item.getOldNamespace() != null) {
				printLine(stream, indentLevel + 2, "<oldtrns>");
				printLine(stream, indentLevel + 4, escapeText(item
						.getOldNamespace()));
				printLine(stream, indentLevel + 2, "</oldtrns>");
			}
			if (item.getNewNamespace() != null) {
				printLine(stream, indentLevel + 2, "<newtrns>");
				printLine(stream, indentLevel + 4, escapeText(item
						.getNewNamespace()));
				printLine(stream, indentLevel + 2, "</newtrns>");
			}
			printLine(stream, indentLevel, "</TRNamespaceHistoryItem>");
		} else if (hitem instanceof TermMacroHistoryItem) {
			TermMacroHistoryItem item = (TermMacroHistoryItem) hitem;
			printLine(stream, indentLevel, "<TermMacroHistoryItem>");
			if (item.getTarget() != null)
				printLine(stream, indentLevel + 2, "<target>"
						+ escapeText(item.getTarget()) + "</target>");
			if (item.getResult() != null)
				printLine(stream, indentLevel + 2, "<result>"
						+ escapeText(item.getResult()) + "</result>");
			printLine(stream, indentLevel + 2, "<desc>"
					+ escapeText(item.getDescription()) + "</desc>");
			printLine(stream, indentLevel + 2, "<editlist>");
			dumpHistory(stream, item, indentLevel + 4);
			printLine(stream, indentLevel + 2, "</editlist>");
			printLine(stream, indentLevel, "</TermMacroHistoryItem>");
		} else if (hitem instanceof AddDbxrefHistoryItem) {
			AddDbxrefHistoryItem item = (AddDbxrefHistoryItem) hitem;
			printLine(stream, indentLevel, "<AddDbxrefHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>" + item.getTarget()
					+ "</target>");
			printLine(stream, indentLevel + 2, "<target_xref>");
			dumpDbxref(stream, item.getDbxref(), indentLevel + 4);
			printLine(stream, indentLevel + 2, "</target_xref>");
			if (item.isDef())
				printLine(stream, indentLevel + 2, "<isDef>true</isDef>");
			if (item.getSynonym() != null) {
				printLine(stream, indentLevel + 2, "<target_synonym>"
						+ escapeText(item.getSynonym()) + "</target_synonym>");
			}
			printLine(stream, indentLevel, "</AddDbxrefHistoryItem>");
		} else if (hitem instanceof DelDbxrefHistoryItem) {
			DelDbxrefHistoryItem item = (DelDbxrefHistoryItem) hitem;
			printLine(stream, indentLevel, "<DelDbxrefHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>" + item.getTarget()
					+ "</target>");
			printLine(stream, indentLevel + 2, "<target_xref>");
			dumpDbxref(stream, item.getDbxref(), indentLevel + 4);
			printLine(stream, indentLevel + 2, "</target_xref>");
			if (item.isDef())
				printLine(stream, indentLevel + 2, "<isDef>true</isDef>");
			if (item.getSynonym() != null) {
				printLine(stream, indentLevel + 2, "<synonym>"
						+ escapeText(item.getSynonym()) + "</synonym>");
			}
			printLine(stream, indentLevel, "</DelDbxrefHistoryItem>");
		} else if (hitem instanceof AddReplacementHistoryItem) {
			AddReplacementHistoryItem item = (AddReplacementHistoryItem) hitem;
			printLine(stream, indentLevel, "<AddReplacementHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>"
					+ escapeText(item.getTarget()) + "</target>");
			if (item.getReplace() != null)
				printLine(stream, indentLevel + 2, "<replacedby>"
						+ escapeText(item.getReplace()) + "</replacedby>");
			printLine(stream, indentLevel, "</AddReplacementHistoryItem>");
		} else if (hitem instanceof RemoveReplacementHistoryItem) {
			RemoveReplacementHistoryItem item = (RemoveReplacementHistoryItem) hitem;
			printLine(stream, indentLevel, "<RemoveReplacementHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>"
					+ escapeText(item.getTarget()) + "</target>");
			if (item.getReplace() != null)
				printLine(stream, indentLevel + 2, "<replacedby>"
						+ escapeText(item.getReplace()) + "</replacedby>");
			printLine(stream, indentLevel, "</RemoveReplacementHistoryItem>");
		} else if (hitem instanceof AddConsiderHistoryItem) {
			AddConsiderHistoryItem item = (AddConsiderHistoryItem) hitem;
			printLine(stream, indentLevel, "<AddConsiderHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>"
					+ escapeText(item.getTarget()) + "</target>");
			printLine(stream, indentLevel + 2, "<consider>"
					+ escapeText(item.getConsider()) + "</consider>");
			printLine(stream, indentLevel, "</AddConsiderHistoryItem>");
		} else if (hitem instanceof RemoveConsiderHistoryItem) {
			RemoveConsiderHistoryItem item = (RemoveConsiderHistoryItem) hitem;
			printLine(stream, indentLevel, "<RemoveConsiderHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>"
					+ escapeText(item.getTarget()) + "</target>");
			printLine(stream, indentLevel + 2, "<consider>"
					+ escapeText(item.getConsider()) + "</consider>");
			printLine(stream, indentLevel, "</RemoveConsiderHistoryItem>");
		} else if (hitem instanceof AddSynonymHistoryItem) {
			AddSynonymHistoryItem item = (AddSynonymHistoryItem) hitem;
			printLine(stream, indentLevel, "<AddSynonymHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>"
					+ escapeText(item.getTarget()) + "</target>");
			printLine(stream, indentLevel + 2, "<synonym_edit_target>"
					+ escapeText(item.getSynonym()) + "</synonym_edit_target>");
			printLine(stream, indentLevel, "</AddSynonymHistoryItem>");
		} else if (hitem instanceof DelSynonymHistoryItem) {
			DelSynonymHistoryItem item = (DelSynonymHistoryItem) hitem;
			printLine(stream, indentLevel, "<DelSynonymHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>"
					+ escapeText(item.getTarget()) + "</target>");
			printLine(stream, indentLevel + 2, "<synonym_edit_target>"
					+ escapeText(item.getSynonym()) + "</synonym_edit_target>");
			printLine(stream, indentLevel, "</DelSynonymHistoryItem>");
		} else if (hitem instanceof CategoryChangeHistoryItem) {
			CategoryChangeHistoryItem item = (CategoryChangeHistoryItem) hitem;
			printLine(stream, indentLevel, "<CategoryChangeHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>"
					+ escapeText(item.getTarget()) + "</target>");
			printLine(stream, indentLevel + 2, "<category>"
					+ escapeText(item.getCategory()) + "</category>");
			if (item.isDel())
				printLine(stream, indentLevel + 2,
						"<isTermCatDel>true</isTermCatDel>");

			printLine(stream, indentLevel, "</CategoryChangeHistoryItem>");
		} else if (hitem instanceof ChangeSynCategoryHistoryItem) {
			ChangeSynCategoryHistoryItem item = (ChangeSynCategoryHistoryItem) hitem;
			printLine(stream, indentLevel, "<ChangeSynCategoryHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>"
					+ escapeText(item.getTarget()) + "</target>");
			printLine(stream, indentLevel + 2, "<synonym_edit_target>"
					+ escapeText(item.getSynonym()) + "</synonym_edit_target>");
			if (item.getOldCategory() != null)
				printLine(stream, indentLevel + 2, "<oldSynCategory>"
						+ escapeText(item.getOldCategory())
						+ "</oldSynCategory>");
			if (item.getNewCategory() != null)
				printLine(stream, indentLevel + 2, "<newSynCategory>"
						+ escapeText(item.getNewCategory())
						+ "</newSynCategory>");

			printLine(stream, indentLevel, "</ChangeSynCategoryHistoryItem>");
		} else if (hitem instanceof ChangeSynScopeHistoryItem) {
			ChangeSynScopeHistoryItem item = (ChangeSynScopeHistoryItem) hitem;
			printLine(stream, indentLevel, "<ChangeSynScopeHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>"
					+ escapeText(item.getTarget()) + "</target>");
			printLine(stream, indentLevel + 2, "<synonym_edit_target>"
					+ escapeText(item.getSynonym()) + "</synonym_edit_target>");
			printLine(stream, indentLevel + 2, "<oldScope>"
					+ item.getOldScope() + "</oldScope>");
			printLine(stream, indentLevel + 2, "<newScope>"
					+ item.getNewScope() + "</newScope>");

			printLine(stream, indentLevel, "</ChangeSynScopeHistoryItem>");
		} else if (hitem instanceof CommentChangeHistoryItem) {
			CommentChangeHistoryItem item = (CommentChangeHistoryItem) hitem;
			printLine(stream, indentLevel, "<CommentChangeHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>"
					+ escapeText(item.getTarget()) + "</target>");
			if (item.getText() != null)
				printLine(stream, indentLevel + 2, "<oldComment>"
						+ escapeText(item.getText()) + "</oldComment>");
			if (item.getNewText() != null)
				printLine(stream, indentLevel + 2, "<newComment>"
						+ escapeText(item.getNewText()) + "</newComment>");

			printLine(stream, indentLevel, "</CommentChangeHistoryItem>");
		} else if (hitem instanceof CreateObjectHistoryItem) {
			CreateObjectHistoryItem item = (CreateObjectHistoryItem) hitem;
			printLine(stream, indentLevel, "<CreateObjectHistoryItem>");
			printLine(stream, indentLevel + 2, "<objectID>"
					+ escapeText(item.getObjectID()) + "</objectID>");
			printLine(stream, indentLevel + 2, "<typeID>"
					+ escapeText(item.getObjectType()) + "</typeID>");

			printLine(stream, indentLevel, "</CreateObjectHistoryItem>");
		} else if (hitem instanceof DefinitionChangeHistoryItem) {
			DefinitionChangeHistoryItem item = (DefinitionChangeHistoryItem) hitem;
			printLine(stream, indentLevel, "<DefinitionChangeHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>" + item.getTarget()
					+ "</target>");
			if (item.getText() != null)
				printLine(stream, indentLevel + 2, "<oldDef>" + item.getText()
						+ "</oldDef>");
			if (item.getNewText() != null)
				printLine(stream, indentLevel + 2, "<newDef>"
						+ item.getNewText() + "</newDef>");

			printLine(stream, indentLevel, "</DefinitionChangeHistoryItem>");
		} else if (hitem instanceof DestroyObjectHistoryItem) {
			DestroyObjectHistoryItem item = (DestroyObjectHistoryItem) hitem;
			printLine(stream, indentLevel, "<DestroyObjectHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>" + item.getTarget()
					+ "</target>");
			printLine(stream, indentLevel, "</DestroyObjectHistoryItem>");
		} else if (hitem instanceof NameChangeHistoryItem) {
			NameChangeHistoryItem item = (NameChangeHistoryItem) hitem;
			printLine(stream, indentLevel, "<NameChangeHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>" + item.getTarget()
					+ "</target>");
			printLine(stream, indentLevel + 2, "<oldText>"
					+ escapeText(item.getText()) + "</oldText>");
			printLine(stream, indentLevel + 2, "<newText>"
					+ escapeText(item.getNewText()) + "</newText>");
			printLine(stream, indentLevel, "</NameChangeHistoryItem>");
		} else if (hitem instanceof NamespaceHistoryItem) {
			NamespaceHistoryItem item = (NamespaceHistoryItem) hitem;
			printLine(stream, indentLevel, "<NamespaceHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>"
					+ escapeText(item.getTarget()) + "</target>");
			if (item.getOldNamespace() != null) {
				printLine(stream, indentLevel + 2, "<oldns>"
						+ escapeText(item.getOldNamespace().getID())
						+ "</oldns>");
			}
			if (item.getNewNamespace() != null) {
				printLine(stream, indentLevel + 2, "<newns>"
						+ escapeText(item.getNewNamespace().getID())
						+ "</newns>");
			}
			printLine(stream, indentLevel, "</NamespaceHistoryItem>");
		} else if (hitem instanceof ObsoleteObjectHistoryItem) {
			ObsoleteObjectHistoryItem item = (ObsoleteObjectHistoryItem) hitem;
			printLine(stream, indentLevel, "<ObsoleteObjectHistoryItem>");
			printLine(stream, indentLevel + 2, "<target>"
					+ escapeText(item.getTarget()) + "</target>");
			printLine(stream, indentLevel, "</ObsoleteObjectHistoryItem>");
		} else if (hitem instanceof SynonymCategoryHistoryItem) {
			SynonymCategoryHistoryItem item = (SynonymCategoryHistoryItem) hitem;
			printLine(stream, indentLevel, "<SynonymCategoryHistoryItem>");
			if (item.isAdd())
				printLine(stream, indentLevel + 2,
						"<isSynCatAdd>true</isSynCatAdd>");
			if (item.isDel())
				printLine(stream, indentLevel + 2,
						"<isSynCatDel>true</isSynCatDel>");
			if (item.getOldCategory() != null) {
				printLine(stream, indentLevel + 2, "<oldSynCat>");
				dumpSynonymCategory(stream, item.getOldCategory(),
						indentLevel + 4);
				printLine(stream, indentLevel + 2, "</oldSynCat>");
			}
			if (item.getNewCategory() != null) {
				printLine(stream, indentLevel + 2, "<newSynCat>");
				dumpSynonymCategory(stream, item.getNewCategory(),
						indentLevel + 4);
				printLine(stream, indentLevel + 2, "</newSynCat>");
			}

			printLine(stream, indentLevel, "</SynonymCategoryHistoryItem>");
		}
	}

	public void printLine(PrintStream stream, int indent, String line) {
		for (int i = 0; i < indent; i++)
			stream.print(" ");
		stream.print(line);
		stream.print("\n");
	}

	public String getProgressString() {
		// TODO Auto-generated method stub
		return null;
	}

	public Number getProgressValue() {
		// TODO Auto-generated method stub
		return null;
	}
}
