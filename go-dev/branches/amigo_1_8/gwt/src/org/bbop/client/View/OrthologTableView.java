package org.bbop.client.View;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;




import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.data.DataCallback;
import net.mygwt.ui.client.data.DataList;
import net.mygwt.ui.client.data.LoadConfig;
import net.mygwt.ui.client.data.LoadResult;
import net.mygwt.ui.client.data.Model;
import net.mygwt.ui.client.viewer.IAsyncContentCallback;
import net.mygwt.ui.client.viewer.ModelCellLabelProvider;
import net.mygwt.ui.client.viewer.RemoteContentProvider;
import net.mygwt.ui.client.viewer.TableViewer;
import net.mygwt.ui.client.widget.table.Table;
import net.mygwt.ui.client.widget.table.TableColumn;
import net.mygwt.ui.client.widget.table.TableColumnModel;

import org.bbop.client.Listener.RefGenomeViewListenerI;



public class OrthologTableView extends GenericNodeListTableView {
	
	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	private TableColumn[] orthoCols;
	private Table orthoTbl;
	private TableColumnModel colModel;
	private RemoteContentProvider cp;
	
	public OrthologTableView(RefGenomeViewListenerI listener,
			RefGenomeView parent) {
		super(listener,parent);
		refgListener = listener;
		mainView = parent;
		
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
		orthoTbl = new Table(Style.SINGLE|Style.HORIZONTAL, colModel);
		orthoTbl.setBorders(false);
		
		cp = new RemoteContentProvider() {

			
			public void getData(LoadConfig config, DataCallback callback) {
				// TODO Auto-generated method stub
			    LoadResult result = getTestData(config);
				//System.err.println("From getData: " + result.totalLength);
				callback.setResult(result);
			}

			public void getElements(Object input, IAsyncContentCallback callback) {
				// TODO Auto-generated method stub
				if (input instanceof List) {
					 List list = (List)input;
					 //System.err.println("From getElements: " + list.size());
					// for(int i = 0 ; i < list.size(); i++)  {
						// Model m = (Model) list.get(i);
						 //System.err.println("Print getElements: " + m.getAsString("target"));
					// }
					//System.err.println(list.toArray());
					callback.setElements(list.toArray());
				}
				else {
					System.err.println("Error in input" + input.toString());
				}
				
				
				
			}
			
		};
		
		cp.setRemoteSort(false);
		final TableViewer viewer = new TableViewer(orthoTbl);
		viewer.setContentProvider(cp);
		
		ModelCellLabelProvider lp = new ModelCellLabelProvider();
			
		
		for (int i = 0; i < cols.size(); i++) {
			viewer.getViewerColumn(i).setLabelProvider(lp);
		}
		
	}
	
	public Table getView() {
		return orthoTbl;
	}
	
    private LoadResult getTestData(LoadConfig config) {
    	DataList sublist = new DataList();
    	int start = config.start;
    	int limit =100;
        int step = 25;
        
        //System.err.println("Starting cursor: " + start);
        
    	for (int i = start ; i < step + start ; i++) {
    		OrthoData orthoData = getTestOrthoData();
    		Model m = new Model();
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
    		sublist.add(m);
    	}
    	
    	//System.err.println("From getTestdata data size: " + sublist.size());
    	
    	LoadResult result = new LoadResult(sublist);
    	result.cursor = start;
    	result.totalLength = limit;
    	
    	//System.err.println("From getTestdata: after Loadresult: " + result.totalLength);
    	return result;
    	
    }
    
    public RemoteContentProvider getDataProvider() {
    	return cp;
    	
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
