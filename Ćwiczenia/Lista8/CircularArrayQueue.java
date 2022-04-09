package Lista8;

import java.util.ArrayList;

public class CircularArrayQueue <E> implements MyQueue<E>{
	
	private int size, f, r;
	private ArrayList<E> circularArray;
	
	public CircularArrayQueue(int s) {
		
		size = s;
		f = 0;
		r = 0;
		circularArray = new ArrayList<E>(size+1);
	}
	
	@Override
	public void enqueue(E value) throws FullException{
		
		if(isFull()) {
			
			throw new FullException("CircularArrayQueue: enqueue(E value) - queue is full!");
		}
		else if(circularArray.size() < size+1) {
			
			if(r == size) {
				
				circularArray.add(r, value);	
				r = 0;
			}
			else {
				
				circularArray.add(r, value);	
				r++;
			}
		}
		else if(r == size) {
			
			circularArray.set(r, value);	
			r = 0;
		}
		else {
			
			circularArray.set(r, value);	
			r++;
		}
		
	}
	
	@Override
	public void dequeue() {
		
		if(!isEmpty()){
			
			if(f == size) {
				
				f = 0;
			}
			else {
				
				f++;
			}
		}
	}
	
	@Override
	public E first() throws EmptyException{
		
		if(isEmpty()) {
		
			throw new EmptyException("CircularArrayQueue: first() - queue is empty!");
		}
		
		return circularArray.get(f);
	}
	
	@Override
	public boolean isEmpty(){
	
		if(f == r) {
			
			return true;
		}
	
		return false;
	}
	
	@Override
	public boolean isFull() {
	
		if((r == size && f == 0) ||  r == f-1) {
			
			return true;
		}
		
		return false;
	}
}
