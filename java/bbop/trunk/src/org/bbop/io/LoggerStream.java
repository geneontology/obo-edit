package org.bbop.io;

/*import java.util.logging.Level;
import java.util.logging.Logger;*/
import org.apache.log4j.*;

public class LoggerStream extends AbstractExtendedPrintStream {

	protected Logger logger;
	protected Level level;
	
	public LoggerStream(Logger logger, Level level) {
		super();
		this.logger = logger;
		this.level = level;
	}
	
	@Override
	protected void writeString(String s) {
		logger.log(level, s);
	}
}
