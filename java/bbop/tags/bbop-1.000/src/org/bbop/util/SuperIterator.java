package org.bbop.util;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Stack;

public class SuperIterator<IN, OUT> implements Iterator<OUT> {

	protected Stack<Iterator> iteratorStack;
	
	protected Iterator<OUT> current;
	protected IN root;
	protected List<IteratorFactory> factories;
	
	public SuperIterator(IN root, IteratorFactory... factories) {
		this.factories = new LinkedList<IteratorFactory>();
		for(IteratorFactory factory : factories)
			this.factories.add(factory);
		this.root = root;
	}

	public SuperIterator(IN root, List<IteratorFactory> factories) {
		this.factories = factories;
		this.root = root;
	}

	public boolean hasNext() {
		current = getCurrentIterator();
		return current != null;
	}

	public OUT next() {
		current = getCurrentIterator();
		if (current == null)
			throw new NoSuchElementException();
		return current.next();
	}

	public void remove() {
		current = getCurrentIterator();
		current.remove();
	}

	protected Iterator<OUT> getCurrentIterator() {
		if (iteratorStack == null) {
			iteratorStack = new Stack();
			iteratorStack.push(factories.get(0).getIterator(root));
		}
		while (iteratorStack.size() > 0) {
			Iterator<OUT> current = iteratorStack.peek();
			if (current.hasNext()) {
				if (iteratorStack.size() == factories.size()) {
					// If the iterator stack has one item more than the factory
					// list, it means that our stack is full and we're ready to
					// go
					break;
				} else {
					// If the iterator stack is the same size or smaller than
					// the
					// factory stack, it means that we need to take the current
					// item off
					// the current iterator, build another iterator from it
					// using the
					// appropriate factory, and push that iterator onto the
					// stack
					OUT obj = current.next();
					IteratorFactory currentFactory = factories
							.get(iteratorStack.size());
					Iterator newIterator = currentFactory.getIterator(obj);
					iteratorStack.push(newIterator);
				}
			} else {
				iteratorStack.pop();
			}
		}
		if (iteratorStack.size() == 0)
			return null;
		else
			return iteratorStack.peek();
	}

}
