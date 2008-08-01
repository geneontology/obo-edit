package org.bbop.client.test.Widgets;

import com.google.gwt.user.client.ui.TextBox;

	
//
public class FreeInputBox extends TextBox{

		
		//
	public FreeInputBox (String str){
		super();
		this.setVisibleLength(30);
		this.setText(str);
	}
}
