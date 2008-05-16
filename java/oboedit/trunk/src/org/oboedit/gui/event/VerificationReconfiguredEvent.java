package org.oboedit.gui.event;

import java.util.EventObject;

import org.oboedit.verify.Check;

import org.apache.log4j.*;

public class VerificationReconfiguredEvent extends EventObject {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(VerificationReconfiguredEvent.class);
	
		protected Check check;

		public VerificationReconfiguredEvent(Object source, Check check) {
			super(source);
			this.check = check;
		}
		
		public Check getCheck() {
			return check;
		}
}
