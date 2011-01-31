package org.bbop.client.View;

import com.extjs.gxt.ui.client.Events;
import com.extjs.gxt.ui.client.Style;
import com.extjs.gxt.ui.client.event.BaseEvent;
import com.extjs.gxt.ui.client.event.ButtonEvent;
import com.extjs.gxt.ui.client.event.Listener;
import com.extjs.gxt.ui.client.event.SelectionListener;
import com.extjs.gxt.ui.client.widget.button.Button;
import com.extjs.gxt.ui.client.widget.Dialog;
import com.extjs.gxt.ui.client.widget.Info;
import com.extjs.gxt.ui.client.widget.toolbar.SeparatorToolItem;
import com.extjs.gxt.ui.client.widget.toolbar.ToolBar;
import com.extjs.gxt.ui.client.widget.toolbar.ToolItem;
import com.extjs.gxt.ui.client.widget.toolbar.AdapterToolItem;
import com.extjs.gxt.ui.client.widget.LayoutContainer;
import com.extjs.gxt.ui.client.widget.layout.FillLayout;

import org.bbop.client.Listener.RefGenomeViewListenerI;

import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.VerticalPanel;

public class TargetToolBarView {
	
	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	private ToolBar tbar;
	
	private Button summBtn;
//	private Button orthoBtn;
//	private Button reportBtn;
	private Button addTarget;
//	private Button delTarget;
	
	private AdapterToolItem summItem;
//	private ToolItemAdapter orthoItem;
//	private ToolItemAdapter reportItem;
	private AdapterToolItem addTargetItem;
//	private ToolItemAdapter delTargetItem;
	
	public TargetToolBarView(RefGenomeViewListenerI listener, RefGenomeView parent) {
		refgListener = listener;
		parent = mainView;
		
		tbar = new ToolBar();
		
		summBtn = new Button("Summary");
	//	orthoBtn = new Button("Ortholog");
	//	reportBtn = new Button("Curation report");
		addTarget = new Button("Add target");
	//	delTarget = new Button("Delete target");
		
		summBtn.setIconStyle("icon-application-view-list");
	//	orthoBtn.setIconStyle("icon-view");
	//	reportBtn.setIconStyle("icon-report");
		addTarget.setIconStyle("icon-database-add");
	//	delTarget.setIconStyle("icon-database-delete");
		
		summItem = new AdapterToolItem(summBtn);
	//	orthoItem = new ToolItemAdapter(orthoBtn);
	//	reportItem = new ToolItemAdapter(reportBtn);
		addTargetItem = new AdapterToolItem(addTarget);
	//	delTargetItem = new ToolItemAdapter(delTarget);
		
		setAttr();
		addObservers();
		
	}
	
	public void createView() {
		tbar.add(summItem);
		tbar.add(new SeparatorToolItem());
	//	tbar.add(orthoItem);
	//	tbar.add(new ToolItem(Style.SEPARATOR));
	//	tbar.add(reportItem);
	//	tbar.add(new ToolItem(Style.SEPARATOR));
		tbar.add(addTargetItem);
	//	tbar.add(new ToolItem(Style.SEPARATOR));
	//	tbar.add(delTargetItem);
		
	}
	
	public void setAttr() {
		summItem.setStyleAttribute("paddingTop", "4px");
		summItem.setStyleAttribute("paddingLeft", "5px");
		summItem.setStyleAttribute("paddingBottom", "6px");
		summItem.setStyleAttribute("paddingRight", "8px");
		
//		orthoItem.setStyleAttribute("paddingTop", "4px");
//		orthoItem.setStyleAttribute("paddingRight", "5px");
//		orthoItem.setStyleAttribute("paddingBottom", "6px");
//		orthoItem.setStyleAttribute("paddingLeft", "8px");
		
//		reportItem.setStyleAttribute("paddingTop", "4px");
//		reportItem.setStyleAttribute("paddingRight", "5px");
//		reportItem.setStyleAttribute("paddingBottom", "6px");
//		reportItem.setStyleAttribute("paddingLeft", "8px");
		
		addTargetItem.setStyleAttribute("paddingTop", "4px");
		addTargetItem.setStyleAttribute("paddingRigth", "5px");
		addTargetItem.setStyleAttribute("paddingBottom", "6px");
		addTargetItem.setStyleAttribute("paddingLeft", "8px");
		
//		delTargetItem.setStyleAttribute("paddingTop", "4px");
//		delTargetItem.setStyleAttribute("paddingBottom", "6px");
//		delTargetItem.setStyleAttribute("paddingLeft", "8px");
	}
	
	public void addObservers() {
		addTarget.addSelectionListener(new AddTargetListener());
		
	}
	
	public ToolBar getView() {
		return tbar;
	}
	
	private class AddTargetListener extends SelectionListener<ButtonEvent> {

		public void componentSelected(ButtonEvent ce) {
		
			// TODO Auto-generated method stub
			final HorizontalPanel geneidBar = new HorizontalPanel();
			final HorizontalPanel genesymBar = new HorizontalPanel();
			final HorizontalPanel dateBar = new HorizontalPanel();
			final VerticalPanel targetDialog = new VerticalPanel();
			
			Label idLabel = new Label("Gene id: ");
			Label symLabel = new Label("Gene symbol: ");
			Label dateLabel = new Label("Target Completion date ");
			
			TextBox idTerm = new TextBox();
			TextBox symTerm = new TextBox();
			TextBox dateTerm = new TextBox();
			
			geneidBar.setSpacing(10);
			genesymBar.setSpacing(10);
			dateBar.setSpacing(10);
			targetDialog.setSpacing(8);
			
			idTerm.setWidth("90");
			symTerm.setWidth("90");
			dateTerm.setWidth("90");
			
			geneidBar.add(idLabel);
			geneidBar.add(idTerm);
			genesymBar.add(symLabel);
			genesymBar.add(symTerm);
			dateBar.add(dateLabel);
			dateBar.add(dateTerm);
			
			targetDialog.add(geneidBar);
			targetDialog.add(genesymBar);
			targetDialog.add(dateBar);
			
			final Dialog complex = new Dialog();  
			complex.setMinHeight(200);
			complex.setMinWidth(300);  
			complex.addStyleName("my-shell-plain"); 
			complex.setHeading("Add new target gene");
			complex.setHideOnButtonClick(true);
			complex.setButtons(Dialog.OKCANCEL);
			
			
			complex.setBorders(false);
			complex.setLayout(new FillLayout());
			complex.add(targetDialog);
			complex.show();
			
			
			complex.addListener(Events.OnClick, new Listener<ButtonEvent>() {

				public void handleEvent(ButtonEvent be) {
					// TODO Auto-generated method stub
					Button btn = complex.getButtonPressed();
					if (btn.getType() == "OK") {
						Info.display("Adding new target", "Name: ", "Gene");
						
					}
					 complex.hide();
				}
					
			
				
			});
		
	  }
	
	
	}

}
