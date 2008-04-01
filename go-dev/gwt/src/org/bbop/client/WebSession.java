package org.bbop.client;


public class WebSession {
		
	private String username;
	private String capacity;
	private boolean authenticated_p ;
	
	
    // Create the object according to a session.
    public WebSession () {
    	username = "";
    	capacity = "";
    	authenticated_p = false;
	}
	
	public boolean tryAuthentication (String uname, String pass, String cap) {

		boolean boo = false;

		if( uname.equals("foo") &&
			pass.equals("bar") ){
			
			authenticated_p = true;
			username = "Foo";
			capacity = cap;	
			boo = true;
		}
		
		return boo;
	}

	public void revokeAuthentication () {
		authenticated_p = false;
	}

	public boolean getAuthentication () {
		return authenticated_p;
	}

	public String getUsername () {
		return username;
	}

	public String getCapacity () {
		return capacity;
	}

//	//
//	// TODO/BUG: Everything below here should be be gotten on auto from a database. 
//	//
//	
//	public void setAuthentication (boolean boo) {
//		authenticated_p = boo;
//	}
//	public void setUsername (String str) {
//		username = str;
//	}
//	public void setAuthentication (String str) {
//		capacity = str;
//	}
}
