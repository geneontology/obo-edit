package org.bbop.client.View;


import java.util.ArrayList;
import java.util.List;

import net.mygwt.ui.client.Events;
import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.event.BaseEvent;
import net.mygwt.ui.client.event.Listener;
import net.mygwt.ui.client.event.SelectionListener;
import net.mygwt.ui.client.widget.Button;
import net.mygwt.ui.client.widget.Dialog;
import net.mygwt.ui.client.widget.Info;
import net.mygwt.ui.client.widget.MessageBox;
import net.mygwt.ui.client.widget.ToolBar;

import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.Manager.BrowsePanelManagerI;
import org.bbop.client.model.NodeDTO;


import com.google.gwt.user.client.ui.VerticalPanel;

public class BrowsePanelView implements BrowsePanelManagerI {
	private RefGenomeViewListenerI refglistener;
	private RefGenomeView mainview;
	private VerticalPanel browseBar;
	private Button targetBtn;
	private Button targetBtn2;
	private Button orthologBtn;	
	private ResultPanelView resultView;
	private MessageBox info;
	
	private int items ;


	public BrowsePanelView (RefGenomeViewListenerI listener, RefGenomeView parent){
		refglistener = listener;
		mainview = parent;

		browseBar = new VerticalPanel();
		targetBtn = new Button("List Target");
		targetBtn2 = new Button("List Target (from db)");
		orthologBtn = new Button("List Ortholog");

		setItems(25);
		setAttr();
		addObservers();
	}

	public void creatView() {
		browseBar.add(targetBtn);
		browseBar.add(targetBtn2);
		browseBar.add(orthologBtn);


	}

	public void setAttr() {
		browseBar.setSpacing(8);
		targetBtn.setIconStyle("icon-list");
		targetBtn2.setIconStyle("icon-list");
		orthologBtn.setIconStyle("icon-list");


	}
	
	public void setItems(int  num) {
		items = num;
	}

	public VerticalPanel getView() { return browseBar; }

	private void addObservers() {
		targetBtn.addSelectionListener(new TargetListListener());
		targetBtn2.addSelectionListener(new TargetList2Listener());
		orthologBtn.addSelectionListener(new OrthologListListener());

	}

	public void addTargetIds(String[] ids) {
		// TODO Auto-generated method stub

	}


	private class TargetListListener implements SelectionListener {
		public void widgetSelected(BaseEvent be) {
			//refglistener.fetchTargetIds();
			TargetTableView tableView = new TargetTableView(refglistener, mainview);
			TargetToolBarView toolBarView = new TargetToolBarView(refglistener, mainview);
			TargetListToolBarView toolBarListView = new TargetListToolBarView(refglistener, mainview);
			// the following method should pass server side data
			tableView.createView();
			toolBarView.createView();
			toolBarListView.createView();

			//Get the two toolbarviews
			ToolBar[] tbars = new ToolBar[2];
			tbars[0] = toolBarView.getView();
			tbars[1] = toolBarListView.getView();

			resultView = mainview.getResultPanel();
			resultView.removeChildViews();
			resultView.addTargetListView(tableView.getView(), tbars, "List of target");
			resultView.resetView();

		}
	}

	private class TargetList2Listener implements SelectionListener {
		public void widgetSelected(BaseEvent be) {

			System.err.println("fetching targets..");
			refglistener.fetchTargets();
			info = new MessageBox(Style.ICON_INFO, Style.MODAL|Style.CLOSE); 
			info.setCloseOnButtonClick(true);
			info.setText("Click to abort search");  
			info.setMessage("Searching please wait.....");
			info.open();
			info.addListener(Events.Close, new Listener() {
				public void handleEvent(BaseEvent be) {
					info.setMessage("Aborting search wait ......");
					refglistener.cancelFetch();
				}
			});

		}
	}
	
	private class OrthologListListener implements SelectionListener {

		public void widgetSelected(BaseEvent be) {
			// TODO Auto-generated method stub 
			OrthologTableView orthoView = new OrthologTableView(refglistener,mainview);
			List colHeadings = new ArrayList();
			String[] colNames = new String[] {"Target","R.norvegicus","C.elegans","D.discoideum","A.thaliana","S.cerevisiae","S.pombe","M.musculus","D.melanogaster","E.coli","D.rio"};
			for(int i =0; i<colNames.length; i++) {
				colHeadings.add(colNames[i]);
			}
			
			orthoView.setColumnHeadings(colHeadings);
			orthoView.createView();
			
			resultView = mainview.getResultPanel();
			resultView.removeChildViews();
			resultView.addOrthologListView(orthoView.getView(), orthoView.getDataProvider(), items, "Ortholog List");
			resultView.resetView();
			orthoView.getDataProvider().load(0,items);
		}
		
	}

	public void displayTargets(NodeDTO[] targetNodes) {
		//info.close();
		System.err.println("making table view");
		GenericNodeListTableView tableView = new GenericNodeListTableView(refglistener, mainview);
		tableView.addColumnHeading("OBO_REL:in_organism","species");
		tableView.addColumnHeading("oboInOwl:hasDbXref","xref");
		tableView.addColumnHeading("OBO_REL:homologous_to","orth");
		TargetToolBarView toolBarView = new TargetToolBarView(refglistener, mainview);
		TargetListToolBarView toolBarListView = new TargetListToolBarView(refglistener, mainview);
		// the following method should pass server side data
		toolBarView.createView();
		toolBarListView.createView();

		//Get the two toolbarviews
		ToolBar[] tbars = new ToolBar[2];
		tbars[0] = toolBarView.getView();
		tbars[1] = toolBarListView.getView();

		tableView.createView(targetNodes);
		System.err.println("created generic view");
		resultView = mainview.getResultPanel();
		// Remove the initial table view
		resultView.removeChildViews();
		//Add the new one
		System.err.println("adding table view");
		resultView.addTableView(tableView.getView(), "Target List");
		resultView.resetView();
	}
	
	public void displayCancelMsg() {
		info.close();
		Info.show("Aborted search","dialog closed","");
		
	}


}
