package org.bbop.client.model;

import java.util.ArrayList;
import java.util.List;


import com.google.gwt.user.client.rpc.IsSerializable;


public class DateDTO implements IsSerializable {

	
	protected final Integer year;
	protected final Integer month;
	protected final Integer day;

	public DateDTO(Integer year, Integer month) {
		super();
		this.year = year;
		this.month = month;
		this.day = null;
	}

	public Integer getDay() {
		return day;
	}

	public Integer getMonth() {
		return month;
	}


	public Integer getYear() {
		return year;
	}

	public String toString() {
		return year + "-" + month; // TODO - 
	}
	
}
