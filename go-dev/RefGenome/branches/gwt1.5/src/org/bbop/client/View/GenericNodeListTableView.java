package org.bbop.client.View;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.extjs.gxt.ui.client.Style;
import com.extjs.gxt.ui.client.widget.button.Button;
import com.extjs.gxt.ui.client.widget.table.DateTimeCellRenderer;
import com.extjs.gxt.ui.client.widget.table.Table;
import com.extjs.gxt.ui.client.widget.table.TableColumn;
import com.extjs.gxt.ui.client.widget.table.TableColumnModel;
import com.extjs.gxt.ui.client.widget.table.TableItem;

import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.model.NodeDTO;


import com.google.gwt.user.client.ui.CheckBox;

/**
 * highly configurable table display for showing lists of nodes
 * @author cjm
 *
 */
public class GenericNodeListTableView {

	protected RefGenomeViewListenerI refgListener;
	protected RefGenomeView mainView;

	//private WidgetContainer summaryWidget;
	private TableColumnModel colModel;
	private Table targetTbl;
	private TableColumn[] tableCols;
	private Map columnLabelMap = new HashMap();

	/**
	 * @gwt.typeArgs <java.lang.String>
	 */
	private List columnHeadings = new ArrayList();

	public GenericNodeListTableView (RefGenomeViewListenerI listener, RefGenomeView parent) {
		refgListener = listener;
		mainView = parent;
		addColumnHeading("id");
		addColumnHeading("label");
	}

	public List getColumnHeadings() {
		return columnHeadings;
	}

	public Map getColumnHeadingMap() {
		return columnLabelMap;
	}

	public void addColumnHeading(String c) {
		columnHeadings.add(c);
	}
	public void addColumnHeading(String c, String label) {
		addColumnHeading(c);
		columnLabelMap.put(c, label);
	}


	public void setColumnHeadings(List columnHeadings) {
		this.columnHeadings = columnHeadings;
	}

	public int getNumberOfColumnHeadings() {
		return getColumnHeadings().size();
	}

	public void createView(NodeDTO[] nodes) {

		// This is where it should receive data from server
		tableCols = new TableColumn[getNumberOfColumnHeadings()];

		List cols = getColumnHeadings();

		for (int i = 0; i < cols.size(); i++) {

			String col = (String) cols.get(i);
			String colLabel = (String) (columnLabelMap.containsKey(col) ? columnLabelMap.get(col) : col);
			tableCols[i] = new TableColumn(colLabel,.15f);
			tableCols[i].setMinWidth(20);
			tableCols[i].setMaxWidth(100);
			System.err.println("set up col: "+i);
		}


		colModel = new TableColumnModel(tableCols);
		targetTbl = new Table(colModel);
		targetTbl.setBorders(true);

		for(int i = 0; i < nodes.length; i++) {
			Object[] targetData = new Object[cols.size()];
			NodeDTO n = (NodeDTO) nodes[i];
			System.err.println("adding to table: "+n);
			for (int c=0; c < cols.size(); c++) {
				String cn = (String) cols.get(c);
				String val = "-";
				if (cn.equals("id")) {
					val = n.getId();
				}
				else if (cn.equals("label")) {
					val = n.getLabel();
				}
				else {
					List vals = n.getTargetIds(cn);
					val = flattenMultipleVals(vals);
				}
				targetData[c] = val;
			}

			targetTbl.add(new TableItem(targetData));

		}



	}

	protected String flattenMultipleVals(List vals) {
		StringBuffer sb = new StringBuffer();
		Iterator it = vals.iterator();
		while (it.hasNext()) {
			sb.append(it.next().toString());
			if (it.hasNext())
				sb.append(", ");
		}
		return sb.toString();
	}


	public void setAttr() {

	}

	public void addObservers () {

	}

	public Table getView() {
		return targetTbl;
	}

	public RefGenomeViewListenerI getRefgListener() {
		return refgListener;
	}

	public void setRefgListener(RefGenomeViewListenerI refgListener) {
		this.refgListener = refgListener;
	}





}
