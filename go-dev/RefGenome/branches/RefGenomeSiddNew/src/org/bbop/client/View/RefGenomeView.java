package org.bbop.client.View;


import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.View.WebViewI;


import net.mygwt.ui.client.widget.Viewport;
import net.mygwt.ui.client.widget.ContentPanel;
import net.mygwt.ui.client.widget.WidgetContainer;
import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.widget.layout.BorderLayout;
import net.mygwt.ui.client.widget.layout.BorderLayoutData;
import net.mygwt.ui.client.widget.layout.FillLayout;







/**
 * @author  sid
 */
public class RefGenomeView implements WebViewI {
	private Viewport refgcontainer ;
	private WidgetContainer refgviewer ;
	private WidgetContainer northpanel ;
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
		refglistener = listener;
		refgcontainer = new Viewport();
		refgviewer = new WidgetContainer();
		northpanel = new WidgetContainer();
		westpanel = new ContentPanel(Style.HEADER);
		
		
		northData = new BorderLayoutData(Style.NORTH,68);
		westData = new BorderLayoutData(Style.WEST,90,75,150);
		centerData = new BorderLayoutData(Style.CENTER);
		
		//layout setup for title and login panel
		FillLayout northfill = new FillLayout(4);
		northfill.setType(Style.VERTICAL);
		northpanel.setLayout(northfill);
		
		//layout setup for navigation panel
		westpanel.setLayout(new FillLayout());
		westpanel.setText("Tools");
		westpanel.setStyleName("title");
		
		
	}
	
	
	
	



	//Widgets with header and login toolbar
	public void setTitlePanel() {
		titleview = new TitlePanelView();
		titleview.createView();
		northpanel.add(titleview.getView());
	
	}
	
	public void setLoginPanel(){
		loginview = new LoginPanelView(refglistener,northpanel);
		loginview.createView();	
		northpanel.add(loginview.getView());	
	}
	
	public void setNavPanel() {
		navpanelview = new NavPanelView(refglistener,westpanel);
		navpanelview.createView();
		westpanel.add(navpanelview.getView());
		
	}
	
	public void setResultPanel() {
		resultview = new ResultPanelView();
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
		
		refgcontainer.add(refgviewer);
		refgcontainer.setLayout(new FillLayout(8));
		refgcontainer.layout();
	}
	
	public Viewport getViewPort(){
		return refgcontainer;
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







	
}
