package org.bbop.client.model;



import com.google.gwt.user.client.rpc.IsSerializable;


public class DateDTO implements IsSerializable {

	// TODO - date validation
	protected Integer year;
	protected Integer month;
	protected Integer day;

	public DateDTO() {
		super();
		this.year = new Integer(1970);
		this.month = new Integer(1);
		this.day = new Integer(1);
	}

	public DateDTO(Integer year, Integer month) {
		super();
		this.year = year;
		this.month = month;
		this.day = new Integer(1);
	}
	public DateDTO(Integer year, Integer month, Integer day) {
		super();
		this.year = year;
		this.month = month;
		this.day = day;
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
		// wow, we could really do with jdk1.5 here...
		return year+"-"+pad2(month)+"-"+pad2(day);
	}
	
	public String pad2(Integer num) {
		if (num.intValue() < 10) {
			return "0"+num;
		}
		return num.toString();
	}
	
}
