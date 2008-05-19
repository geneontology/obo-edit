package org.bbop.util;

import java.beans.BeanInfo;
import java.beans.DefaultPersistenceDelegate;
import java.beans.Encoder;
import java.beans.Expression;
import java.beans.IntrospectionException;
import java.beans.Introspector;

import org.apache.log4j.*;

public class EnumPersistenceDelegate extends DefaultPersistenceDelegate {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(EnumPersistenceDelegate.class);
  private static EnumPersistenceDelegate epd = new EnumPersistenceDelegate();
  public static void installFor(Class<? extends Enum> enumClass) {
    try {
       BeanInfo info = Introspector.getBeanInfo( enumClass );
       info.getBeanDescriptor().setValue( "persistenceDelegate", epd );
    } catch( IntrospectionException exception ) {
       // Do whatever you'd normally do with exceptions here
       exception.printStackTrace();
    }
  }

  protected Expression instantiate(Object oldInstance, Encoder out) {
    return new Expression(Enum.class,
                          "valueOf",
                          new Object[] { oldInstance.getClass(),
                                         ((Enum) oldInstance).name() });
  }

  protected boolean mutatesTo(Object oldInstance, Object newInstance) {
    return oldInstance == newInstance;
  }
}
