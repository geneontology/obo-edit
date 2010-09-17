package org.oboedit.gui.actions;


import java.awt.Toolkit;
import java.awt.event.*;
import java.util.*;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.history.*;
import org.obo.util.IDUtil;
import org.oboedit.controller.IDManager;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.util.GUIUtil;
import javax.swing.*;
import org.apache.log4j.*;

public class CloneAction implements ClickMenuAction {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CloneAction.class);

	protected Selection sources;

	protected boolean isLegal = false;

	protected static KeyStroke keyStroke = KeyStroke.getKeyStroke(
			KeyEvent.VK_L, Toolkit.getDefaultToolkit()
			.getMenuShortcutKeyMask());

	public KeyStroke getKeyStroke() {
		return keyStroke;
	}

	public String getName() {
		return "Clone";
	}

	public String getDesc() {
		return "Clone";
	}

	public List<EditAction> getSubActions() {
		return null;
	}

	public void clickInit(Selection sources, GestureTarget destItem) {
		isLegal = sources.getTerms().size() > 0;
		this.sources = sources;
	}

	public boolean isLegal() {
		return isLegal;
	}

	protected ObjectFactory defaultFactory = new DefaultObjectFactory();

	public HistoryItem execute() {
		List<HistoryItem> itemList = new LinkedList<HistoryItem>();
		Collection<String> ids = new HashSet<String>();
		List<LinkedObject> newTerms = new LinkedList<LinkedObject>();
		for(Object o : sources.getTerms()){
			HistoryList historyList = new DefaultHistoryList();

			IdentifiedObject io = (IdentifiedObject) o;
			io = (IdentifiedObject) io.clone();

			String id = IDUtil.fetchID(IDManager.getManager().getIDAdapter(),
					SessionManager.getManager().getSession(), null, ids, false);
			ids.add(id);

			io.setName("CLONE OF " + io.getName());

			IdentifiedObject clone = defaultFactory.createObject(id,(OBOClass) io.getType(), false);

			newTerms.add(new DanglingObjectImpl(id));

			historyList.addItem(new CreateObjectHistoryItem(id, io.getType().getID()));

			HistoryGenerator.getTermTextChanges(clone, io, historyList, true,
					null);
			if (io.getNamespace() != null) {
				historyList.addItem(new NamespaceHistoryItem(null, io.getNamespace(), id));
			}
			if (io instanceof LinkedObject) {
				for(Link childLink : ((LinkedObject)io).getChildren() ){
					historyList.addItem(new CreateLinkHistoryItem(childLink
							.getChild(), childLink.getType(),
							(LinkedObject) clone));
				}
				for(Link parentLink : ((LinkedObject) io).getParents()){

					HistoryItem item;

					if (parentLink instanceof OBORestriction){
						OBORestriction newLink = new OBORestrictionImpl((LinkedObject) clone, ((OBORestriction)parentLink).getType(),
								parentLink.getParent());
						// check for cross product links
						if(((OBORestriction) parentLink).getCompletes())
							item = new CreateIntersectionLinkHistoryItem(newLink);
						else
							item = new CreateLinkHistoryItem(newLink);
						historyList.addItem(item);
					} 
					else
						historyList.addItem(new CreateLinkHistoryItem(
								(LinkedObject) clone, parentLink.getType(),
								parentLink.getParent()));
				}
			}
			// HistoryGenerator.getParentageChanges(clone, io, historyList);
			HistoryGenerator.getObsoleteChanges(clone, io, historyList, null);

			TermMacroHistoryItem cloneItem = new TermMacroHistoryItem("Clone");
			for(HistoryItem item : historyList.getHistoryItems()){
				cloneItem.addItem(item);
			}
			itemList.add(cloneItem);
		}
		HistoryItem item;
		if (itemList.size() == 1) {
			item = itemList.get(0);
		} else {
			item = new TermMacroHistoryItem("Cloned multiple terms");
			for(Object o : itemList){
				HistoryItem subItem = (HistoryItem) o;
				((TermMacroHistoryItem) item).addItem(subItem);
			}
		}

		GUIUtil.setSelections(item, sources, SelectionManager.createSelection(sources
				.getComponent(), newTerms, null, newTerms.get(0), sources
				.getRootAlgorithm(), sources.getLinkDatabase()));
		return item;
	}
}
