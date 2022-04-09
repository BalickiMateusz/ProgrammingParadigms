package Lista8;

public class Main {

	public static void main(String[] args) {

		CircularArrayQueue<Integer> cArray = new CircularArrayQueue<Integer>(3);
		
		try {
			
			System.out.println(cArray.isEmpty() == true);
			
			cArray.enqueue(1);
			cArray.enqueue(2);
			cArray.enqueue(3);
			
			System.out.println(cArray.isFull() == true);
			
			System.out.println(cArray.first() == 1);
			
			cArray.dequeue();		
			
			System.out.println(cArray.first() == 2);
			
			cArray.enqueue(4);
			
			System.out.println(cArray.first() == 2);
			
			cArray.dequeue();
			
			System.out.println(cArray.first() == 3);
			
			cArray.dequeue();
			cArray.dequeue();
			
			System.out.println(cArray.isEmpty() == true);

		}catch(FullException | EmptyException e) {
			System.err.println(e.getMessage() + "\n");
			e.printStackTrace();
		} 
	}

}
