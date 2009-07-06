package org.bbop.util;

import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.util.Collection;
import java.util.LinkedList;

import org.apache.log4j.*;

public class ReferenceQueueCleanupThread<T> extends Thread implements Cloneable {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ReferenceQueueCleanupThread.class);
	protected boolean halt = false;
	protected ReferenceQueue<T> queue;
	protected Collection<ReferenceCleanupListener> listeners;
	
	public ReferenceQueueCleanupThread(ReferenceQueue<T> queue) {
		this.queue = queue;
	}
	
	public void addCleanupListener(ReferenceCleanupListener listener) {
		if (listeners == null)
			listeners = new LinkedList<ReferenceCleanupListener>();
		listeners.add(listener);
	}
	
	public void removeCleanupListener(ReferenceCleanupListener listener) {
		if (listeners != null) {
			listeners.remove(listener);
			if (listeners.size() == 0)
				listeners = null;
		}
	}
	
	protected void fireCleanup(Reference<? extends T> ref) {
		ReferenceCleanupEvent<? extends T> event = new ReferenceCleanupEvent<T>(queue, ref);
		for(ReferenceCleanupListener<T> listener: listeners) {
			listener.cleanup(event);
		}
	}
	
	@Override
	public Object clone() {
		ReferenceQueueCleanupThread<T> out = new ReferenceQueueCleanupThread<T>(queue);
		if (listeners != null) {
			for(ReferenceCleanupListener listener: listeners) {
				out.addCleanupListener(listener);
			}
		}
		return out;
	}
	
	@Override
	public void run() {
		logger.info("starting cleanup thread...");
		halt = false;
		while(!halt) {
			try {
				Reference<? extends T> r = queue.remove();
				logger.info("Cleaning up reference "+r);
				fireCleanup(r);
			} catch (InterruptedException e) {
			}
		}
	}
	
	public void halt() {
		halt = true;
	}
}
