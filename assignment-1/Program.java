import java.util.*;

public class Program {

    public static HashMap<String, Integer> env;

    public static void main(String[] args) {
        env = new HashMap<String, Integer>();
        env.put("a", 3);
        env.put("b", 111);
        env.put("c", 78);
        env.put("baf", 666);
        env.put("v", 23);
        env.put("w", 55);
        env.put("z", 27);
        env.put("x", 99);
        env.put("y", 42);

        Expr e1 = new Add(new CstI("17"), new Var("z"));
        Expr e2 = new Add(new CstI("27"), new Var("y"));
        Expr e3 = new Add(new CstI("37"), new Var("baf"));
        Expr e4 = new Add(new CstI("47"), new Var("a"));
        System.out.println(e1.toString());
        System.out.println(e2.toString());
        System.out.println(e3.toString());
        System.out.println(e4.toString());
    }

    public abstract static class Expr {
        abstract public String toString();
        abstract public int eval(HashMap<String, Integer> env);
    }
    
    public static class CstI extends Expr{
        public String val; 
        
        public CstI(String val){
            this.val = val.trim(); 
        }

        @Override
        public String toString() {
           return ("CstI " + val);
        }

        @Override
        public int eval(HashMap<String, Integer> map) {
            return Integer.parseInt(val);
        }

    }
    
    public static class Var extends Expr{
        public String val;

        public Var(String val){
            this.val = val.trim();
        }
        
        @Override
        public String toString() {
            return ("Val " + val);
        }

        @Override
        public int eval(HashMap<String, Integer> map) {
            return map.get(val);
        }
    }

    public static abstract class Binop extends Expr {
        Expr e1, e2;
        String ope; 

        Binop(String ope, Expr e1, Expr e2){
            this.ope = ope.trim(); 
            this.e1 = e1; 
            this.e2 = e2; 
        }
        
        @Override
        public String toString() {
            return ""+eval(env);
        }

        @Override
        public int eval(HashMap<String, Integer> map) {
            int r1 = e1.eval(map);
            int r2 = e2.eval(map);
            switch (ope){
                case "+":
                    return r1 + r2;
                case "-":
                    return r1-r2;
                case "*":
                    return r1 * r2;
                default:
                    throw new IllegalArgumentException();
            } 
        }
    }

    public static class Add extends Binop {
        Add(Expr e1, Expr e2){
            super("+", e1, e2);
        }
    }

    public static class Sub extends Binop{
        Sub(Expr e1, Expr e2){
            super("-", e1, e2);
        }
    }

    public static class Mul extends Binop {
        Mul(Expr e1, Expr e2) {
            super("*", e1, e2);
        }
    }
}