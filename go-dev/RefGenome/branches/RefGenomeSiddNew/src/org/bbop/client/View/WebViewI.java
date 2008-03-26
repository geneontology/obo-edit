package org.bbop.client.View;





public interface WebViewI {
	
	public void setTitlePanel ();
	public void setLoginPanel();
	public void setNavPanel();
	public void setResultPanel();
	
	public  TitlePanelView getTitlePanel();
	public LoginPanelView getLoginPanel();
	public NavPanelView getNavPanel();
	public ResultPanelView getResultPanel();
	
}