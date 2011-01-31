package org.bbop.client.View;


import java.util.ArrayList;
import java.util.List;


import com.google.gwt.user.client.rpc.AsyncCallback; 


import com.extjs.gxt.ui.client.Style.HorizontalAlignment;
import com.extjs.gxt.ui.client.binder.TableBinder;
import com.extjs.gxt.ui.client.data.BaseModel;

import com.extjs.gxt.ui.client.data.BasePagingLoadResult;
import com.extjs.gxt.ui.client.data.BasePagingLoader;
import com.extjs.gxt.ui.client.data.PagingLoadConfig;
import com.extjs.gxt.ui.client.data.PagingLoadResult;
import com.extjs.gxt.ui.client.data.RpcProxy;

import com.extjs.gxt.ui.client.store.ListStore;
import com.extjs.gxt.ui.client.widget.ContentPanel;
import com.extjs.gxt.ui.client.widget.PagingToolBar;
import com.extjs.gxt.ui.client.widget.layout.FitLayout;
import com.extjs.gxt.ui.client.widget.table.Table;
import com.extjs.gxt.ui.client.widget.table.TableColumn;
import com.extjs.gxt.ui.client.widget.table.TableColumnModel;

import org.bbop.client.Listener.RefGenomeViewListenerI;



public class OrthologTableView extends GenericNodeListTableView {
	
	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	private TableColumn[] orthoCols;
	private Table orthoTbl;
	private TableColumnModel colModel;
	private RpcProxy rpc;
	private ContentPanel orthoPanel;
	
	public OrthologTableView(RefGenomeViewListenerI listener,
			RefGenomeView parent) {
		super(listener,parent);
		refgListener = listener;
		mainView = parent;
		orthoPanel = new ContentPanel();
		
		// TODO Auto-generated constructor stub
	}
	
	public void setAttr() {
		
	}
	
	public void setObservers() {
		
	}
	
	public void createView() {
		orthoCols = new TableColumn[getNumberOfColumnHeadings()];
		List cols = getColumnHeadings();
		//System.err.println("Column size createview: " + cols.size());

		for (int i = 0; i < cols.size(); i++) {

			String col = (String) cols.get(i);
			String colLabel = (String) (getColumnHeadingMap().containsKey(col) ? getColumnHeadingMap().get(col) : col);
			orthoCols[i] = new TableColumn(colLabel,.15f);
			orthoCols[i].setMinWidth(20);
			orthoCols[i].setMaxWidth(90);
			//System.err.println("set up col: "+i);
		}
		
		colModel = new TableColumnModel(orthoCols);
		orthoTbl = new Table(colModel);
		orthoTbl.setBorders(false);
		
		rpc = new RpcProxy() {
				protected void load(Object loadConfig, AsyncCallback callback) {
					getTestData((PagingLoadConfig) loadConfig);
					
				}	
		};
		
		// loader
		final BasePagingLoader loader = new BasePagingLoader(rpc);
		
		// store  
		ListStore<BaseModel> store = new ListStore<BaseModel>(loader);  
		 
	    // binder  
		 new TableBinder<BaseModel>(orthoTbl, store);  
		 
		 //Toolbar for paging
		 final PagingToolBar toolBar = new PagingToolBar(20);  
		 toolBar.bind(loader);  
		 
		 //panel setup
		 orthoPanel.setFrame(true);  
		 orthoPanel.setCollapsible(true);  
		 orthoPanel.setAnimCollapse(false);  
		 orthoPanel.setButtonAlign(HorizontalAlignment.CENTER);  
		 orthoPanel.setIconStyle("icon-table");  
		 orthoPanel.setHeading("List of ortholog");  
		 orthoPanel.setLayout(new FitLayout());  
		 orthoPanel.add(orthoTbl);  
		 orthoPanel.setSize(600, 450);  
		 orthoPanel.setBottomComponent(toolBar); 
		 
		 loader.load(0,20);
		
	}
	
	public ContentPanel getPagingView() {
		return orthoPanel;
	}
	
    private PagingLoadResult<BaseModel> getTestData(PagingLoadConfig config) {
    	List<BaseModel> models = new ArrayList<BaseModel>();
    	int start = config.getOffset();
    	int step = config.getLimit();
        int records = 100;
        int end = start + step;
        
        if (start + step > end) {
        	end = records;
        }
        
        
        //System.err.println("Starting cursor: " + start);
        
    	for (int i = start ; i < end ; i++) {
    		OrthoData orthoData = getTestOrthoData();
    		BaseModel m = new BaseModel();
    		m.set("Target", orthoData.target + start);
    		m.set("R.norvegicus",orthoData.rat);
    		m.set("C.elegans", orthoData.worm);
    		m.set("D.discoideum", orthoData.dicty);
    		m.set("A.thaliana", orthoData.tair);
    		m.set("S.cerevisiae", orthoData.sgd);
    		m.set("S.pombe",orthoData.pombe);
    		m.set("M.musculus", orthoData.mouse);
    		m.set("D.melanogaster", orthoData.fly);
    		m.set("E.coli", orthoData.ecoli);
    		m.set("D.rio",orthoData.zfin);
    		models.add(m);
    	}
    	
    	//System.err.println("From getTestdata data size: " + sublist.size());
    	
    	
    	
    	//System.err.println("From getTestdata: after Loadresult: " + result.totalLength);
    	return new BasePagingLoadResult(models,step,records);
    	
    }
    
    
    private OrthoData getTestOrthoData () {
    	OrthoData data = new OrthoData();
    	data.target = "ADHIA|PO7327";
    	data.rat = "RGD:2044";
    	data.worm = "WB:WP:CE23822";
    	data.dicty = "DictyBase:DDB0238276";
    	data.tair = "TAIR:gene:1005715800";
    	data.sgd = "SGD:S000002327";
    	data.pombe = "GeneDB_Spombe:SPCC13B11.04C";
    	data.mouse = "MGI:1349472";
    	data.fly = "FB:FBgn0011768";
    	data.ecoli = "ECOGENE_G:adh1";
    	data.zfin = "ZFIN:ZDB-GENE-011003-1";
    	
    	return data;
    	
    }
    
    private class OrthoData  {
    	String target;
    	String rat,worm,dicty,tair,sgd,pombe,mouse,fly,ecoli,zfin;
    }

}
