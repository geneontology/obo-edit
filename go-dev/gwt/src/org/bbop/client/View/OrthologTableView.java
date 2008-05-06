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
import net.mygwt.ui.client.viewer.ViewerCell;
import net.mygwt.ui.client.widget.table.Table;
import net.mygwt.ui.client.widget.table.TableColumn;
import net.mygwt.ui.client.widget.table.TableColumnModel;

import org.bbop.client.Listener.RefGenomeViewListenerI;

import com.google.gwt.user.client.rpc.IsSerializable;

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

		for (int i = 0; i < cols.size(); i++) {

			String col = (String) cols.get(i);
			String colLabel = (String) (getColumnHeadingMap().containsKey(col) ? getColumnHeadingMap().get(col) : col);
			orthoCols[i] = new TableColumn(colLabel,.15f);
			orthoCols[i].setMinWidth(20);
			orthoCols[i].setMaxWidth(100);
			//System.err.println("set up col: "+i);
		}
		
		colModel = new TableColumnModel(orthoCols);
		orthoTbl = new Table(Style.SINGLE|Style.HORIZONTAL, colModel);
		orthoTbl.setBorders(false);
		
		cp = new RemoteContentProvider() {

			
			public void getData(LoadConfig config, DataCallback callback) {
				// TODO Auto-generated method stub
			    LoadResult result = getTestData();
				System.err.println("From getData");
				//System.err.println(list.toString());
				
				System.err.println(result.totalLength);
				//System.err.println(result.data.toString());
				//callback.setResult((LoadResult)result);
				callback.setResult(result);
			}

			public void getElements(Object input, IAsyncContentCallback callback) {
				// TODO Auto-generated method stub
				if (input instanceof List) {
					 List list = (List)input;
					System.err.println(list.toArray());
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
	
    private LoadResult getTestData() {
    	DataList sublist = new DataList();
    	for (int i = 0 ; i <30 ; i++) {
    		OrthoData orthoData = getTestOrthoData();
    		Model m = new Model();
    		m.set("target", orthoData.target);
    		m.set("r.norvegicus",orthoData.rat);
    		m.set("c.elegans", orthoData.worm);
    		m.set("d.discoideum", orthoData.dicty);
    		m.set("a.thaliana", orthoData.tair);
    		m.set("s.cerevisiae", orthoData.sgd);
    		m.set("s.pombe",orthoData.pombe);
    		m.set("m.musculus", orthoData.mouse);
    		m.set("d.melanogaster", orthoData.fly);
    		m.set("e.coli", orthoData.ecoli);
    		m.set("d.rio",orthoData.zfin);
    		sublist.add(m);
    	}
    	System.err.println("From getTestdata");
    	System.err.println(sublist.size());
    	LoadResult result = new LoadResult(sublist);
    	System.err.println(result.totalLength);
    	return result;
    	
    }
    
    public RemoteContentProvider getDataProvider() {
    	return cp;
    	
    }
    
    private OrthoData getTestOrthoData() {
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
    
    private class OrthoData implements Serializable {
    	String target;
    	String rat,worm,dicty,tair,sgd,pombe,mouse,fly,ecoli,zfin;
    }

}
