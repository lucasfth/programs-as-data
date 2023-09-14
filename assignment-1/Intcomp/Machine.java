import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

/* Java implementation of a unified-stack abstract machine 
   sestoft@itu.dk * 2001-02-05 

   In a real stack machine, the stack is an array (not a list as in
   the SML or F# model), and there is a special register called the
   stack pointer sp which is updated as the stack grows and shrinks.
   Accessing a variable x stored n deep into the stack can be done in
   constant time by address arithmetic relative to the stack pointer:
   just access the element stack[sp-n].

   The interpreter seval below is a simple bytecode machine: each
   instruction is a single integer (representable in a byte).
   Instructions with arguments, such as SCST and SVAR, simply take their
   arguments from the next integer in the instruction stream.  

   This is a Java program but might be written in C instead; it does
   not rely on object-orientation or garbage collection.  */

class Machine {
  final static int 
    SCST = 0, SVAR = 1, SADD = 2, SSUB = 3, SMUL = 4, SPOP = 5, SSWAP = 6;
  
  public static void main(String[] args) {
      ArrayList<Integer> listOfint = new ArrayList<Integer>();
      Scanner scanner;
      try {
            scanner = new Scanner(new File("../"+args[0]));
            while(scanner.hasNext()){
                  listOfint.add(scanner.nextInt());
            }
            int[] rpn3 = listOfint.stream().mapToInt(i -> i).toArray();
            System.out.println(seval(rpn3));
      } catch (FileNotFoundException e) {
            e.printStackTrace();
      }
  }
  
  static int seval(int[] code) {
    int[] stack = new int[1000];	// evaluation and env stack
    int sp = -1;			// pointer to current stack top

    int pc = 0;				// program counter
    int instr;				// current instruction

    while (pc < code.length) 
      switch (instr = code[pc++]) {
      case SCST:
	stack[sp+1] = code[pc++]; sp++; break;
      case SVAR:
	stack[sp+1] = stack[sp-code[pc++]]; sp++; break;
      case SADD: 
	stack[sp-1] = stack[sp-1] + stack[sp]; sp--; break;
      case SSUB: 
	stack[sp-1] = stack[sp-1] - stack[sp]; sp--; break;
      case SMUL: 
	stack[sp-1] = stack[sp-1] * stack[sp]; sp--; break;
      case SPOP: 
	sp--; break;
      case SSWAP: 
	{ int tmp     = stack[sp]; 
	  stack[sp]   = stack[sp-1]; 
	  stack[sp-1] = tmp;
	  break;
	}
      default:			
	throw new RuntimeException("Illegal instruction " + instr 
				   + " at address " + (pc-1));
      }
    return stack[sp];      
  }
}
