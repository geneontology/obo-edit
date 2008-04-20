package org.bbop.client.View;

import java.util.ArrayList;
import java.util.List;



import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.data.DataCallback;
import net.mygwt.ui.client.data.DataList;
import net.mygwt.ui.client.data.LoadConfig;
import net.mygwt.ui.client.data.LoadResult;
import net.mygwt.ui.client.viewer.IAsyncContentCallback;
import net.mygwt.ui.client.viewer.ModelCellLabelProvider;
import net.mygwt.ui.client.viewer.RemoteContentProvider;
import net.mygwt.ui.client.viewer.TableViewer;
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
				DataList list = new DataList(getTestData());
				LoadResult result = new LoadResult(list);	
				callback.setResult(result);
				
			}

			public void getElements(Object input, IAsyncContentCallback callback) {
				// TODO Auto-generated method stub
				if (input instanceof List) {
					 List list = (List) input;
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
	
    private List getTestData() {
    	List testData = new ArrayList();
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	testData.add(new String[]{"ADHIA|PO7327","RGD:2044","WB:WP:CE23822","DictyBase:DDB0238276","TAIR:gene:1005715800","SGD:S000002327","GEneDB_Spombe:SPCC13B11.04C","MGI:1349472","FB:FBgn0011768","ECOGENE_G:adh1","ZFIN:ZDB-GENE-011003-1"});
    	
    	return testData;
    }
    
    public RemoteContentProvider getDataProvider() {
    	return cp;
    	
    }

}
