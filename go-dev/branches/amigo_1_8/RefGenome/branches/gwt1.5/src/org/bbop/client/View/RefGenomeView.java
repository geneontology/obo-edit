

package org.bbop.client.View;


import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.View.WebViewI;


import com.extjs.gxt.ui.client.Style.LayoutRegion;
import com.extjs.gxt.ui.client.util.Margins;
import com.extjs.gxt.ui.client.widget.Viewport;
import com.extjs.gxt.ui.client.widget.ContentPanel;
import com.extjs.gxt.ui.client.widget.LayoutContainer;
import com.extjs.gxt.ui.client.Style;
import com.extjs.gxt.ui.client.widget.layout.BorderLayout;
import com.extjs.gxt.ui.client.widget.layout.BorderLayoutData;
import com.extjs.gxt.ui.client.widget.layout.FillLayout;
import com.extjs.gxt.ui.client.widget.layout.FitLayout;

/**
 * @author  sid
 */
public class RefGenomeView  implements WebViewI {
	
	private Viewport refgViewer ;
	private LayoutContainer northpanel ;
	private LayoutContainer westpanel;
	private RefGenomeViewListenerI refglistener;
	
	
	
	//The views that are going to be added to various container
	TitlePanelView titleview;
	LoginPanelView loginview;
	NavPanelView navpanelview;
	ResultPanelView resultview;
	
	
	//Initial sizes for various container
	BorderLayoutData northData;
	BorderLayoutData westData;
	BorderLayoutData centerData;
	

	
	public RefGenomeView (RefGenomeViewListenerI listener) {
		refglistener = listener;
		
		refgViewer = new Viewport();
		refgViewer.setLayout(new BorderLayout());
		
		northpanel = new LayoutContainer();
		northpanel.setLayout(new FillLayout(Style.Orientation.VERTICAL));
		
		westpanel = new LayoutContainer();
		westpanel.setLayout(new FitLayout());
		
		northData = new BorderLayoutData(LayoutRegion.NORTH,68);
		northData.setCollapsible(true);  
        northData.setFloatable(true);  
		northData.setSplit(true);  
		northData.setMargins(new Margins(5, 5, 0, 5));
		
		
		westData = new BorderLayoutData(LayoutRegion.WEST,380);
		westData.setSplit(true);  
		westData.setCollapsible(true);  
		westData.setMargins(new Margins(5));  
		
		
		centerData = new BorderLayoutData(LayoutRegion.CENTER);
		centerData.setMargins(new Margins(5, 0, 5, 0)); 
		
		
		
	
		
		//layout setup for navigation panel
		//westpanel.setTitle("Navigation bar");
		//westpanel.setStyleName("title");
		
		
	}
	
	
	
	



	//Widgets with header and login toolbar
	public void setTitlePanel() {
		titleview = new TitlePanelView();
		titleview.createView();
		northpanel.add(titleview.getView());
	
	}
	
	public void setLoginPanel(){
		loginview = new LoginPanelView(refglistener,this);
		loginview.createView();	
		northpanel.add(loginview.getView());	
		
	}
	
	public void setNavPanel() {
		navpanelview = new  NavPanelView(refglistener,this);
		navpanelview.createView();
		westpanel.add(navpanelview.getView());
		
		
	}
	
	public void setResultPanel() {
		resultview = new ResultPanelView(refglistener,this);
		resultview.createView();
		
	}
	
	public void initView() {
		//refgviewer.setStyleName("my-border-layout");
		//refgviewer.setLayout(new BorderLayout());
		//northpanel.setLayout(new FitLayout());
		//westpanel.setLayout(new FitLayout());
		//setLayout(new BorderLayout());
		setTitlePanel();
		setLoginPanel();
		setNavPanel();
		setResultPanel();
		refgViewer.add(northpanel,northData);
		refgViewer.add(westpanel,westData);
		refgViewer.add(resultview.getView(),centerData);
		
		//this.add(refgviewer);
		
		//refgViewer.setLayout(new FitLayout());
		//refgViewer.add(this);
		//this.layout();
	}
	
	public Viewport getViewPort(){
		return refgViewer;
	}







	public LoginPanelView getLoginPanel() {
		// TODO Auto-generated method stub
		return loginview;
	}







	public NavPanelView getNavPanel() {
		// TODO Auto-generated method stub
		return navpanelview;
	}







	public TitlePanelView getTitlePanel() {
		// TODO Auto-generated method stub
		return titleview;
	}







	public ResultPanelView getResultPanel() {
		// TODO Auto-generated method stub
		return resultview;
	}







	public BrowsePanelView getBrowsePanel() {
		// TODO Auto-generated method stub
		return navpanelview.getBrowseView();
	}
	
	public SearchPanelView getSearchPanel() {
		return navpanelview.getSearchPanelView();
	}







	
}
