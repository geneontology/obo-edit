

package org.bbop.client.View;


import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.View.WebViewI;


import com.extjs.gxt.ui.client.Style.LayoutRegion;
import com.extjs.gxt.ui.client.widget.Viewport;
import com.extjs.gxt.ui.client.widget.ContentPanel;
import com.extjs.gxt.ui.client.widget.LayoutContainer;
import com.extjs.gxt.ui.client.Style;
import com.extjs.gxt.ui.client.widget.layout.BorderLayout;
import com.extjs.gxt.ui.client.widget.layout.BorderLayoutData;
import com.extjs.gxt.ui.client.widget.layout.FillLayout;







/**
 * @author  sid
 */
public class RefGenomeView   extends  Viewport implements WebViewI {
	
	private LayoutContainer refgviewer ;
	private LayoutContainer northpanel ;
	private ContentPanel westpanel;
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
		super();
		refglistener = listener;
		refgviewer = new LayoutContainer();
		northpanel = new LayoutContainer();
		westpanel = new ContentPanel();
		
		
		northData = new BorderLayoutData(LayoutRegion.NORTH,68);
		westData = new BorderLayoutData(LayoutRegion.WEST,380,220,420);
		centerData = new BorderLayoutData(LayoutRegion.CENTER);
		
		//layout setup for title and login panel
		FillLayout northfill = new FillLayout(Style.Orientation.VERTICAL);
		northpanel.setLayout(northfill);
		
		//layout setup for navigation panel
		westpanel.setLayout(new FillLayout());
		westpanel.setHeading("Navigation bar");
		westpanel.setStyleName("title");
		
		
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
		refgviewer.setStyleName("my-border-layout");
		refgviewer.setLayout(new BorderLayout());
		setTitlePanel();
		setLoginPanel();
		setNavPanel();
		setResultPanel();
		refgviewer.add(northpanel,northData);
		refgviewer.add(westpanel,westData);
		refgviewer.add(resultview.getView(),centerData);
		
		this.add(refgviewer);
		this.setLayout(new FillLayout());
		this.layout();
	}
	
	public Viewport getViewPort(){
		return this;
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
