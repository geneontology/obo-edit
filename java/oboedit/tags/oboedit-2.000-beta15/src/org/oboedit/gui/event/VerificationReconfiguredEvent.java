package org.oboedit.gui.event;

import java.util.EventObject;

import org.oboedit.verify.Check;

public class VerificationReconfiguredEvent extends EventObject {
	
		protected Check check;

		public VerificationReconfiguredEvent(Object source, Check check) {
			super(source);
			this.check = check;
		}
		
		public Check getCheck() {
			return check;
		}
}
