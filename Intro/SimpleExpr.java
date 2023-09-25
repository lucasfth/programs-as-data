package Intro;

// WHOLE THING IS OURS

import java.util.*;

//Exercise 1.4
public class SimpleExpr {

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

        //Exercise 1.4 (ii)
        Expr e1 = new Add(new CstI(17), new Var("z"));
        Expr e2 = new Add(new CstI(27), new Var("y"));
        Expr e3 = new Add(new CstI(37), new Var("baf"));
        Expr e4 = new Mul( new Add(new CstI(17), new Var("z")), new Add(new CstI(27), new Var("y")));
        System.out.println(e1.toString());
        System.out.println(e2.toString());
        System.out.println(e3.toString());
        System.out.println(e4.toString());
        //Exercise 1.4 (iii)
        System.out.println(e1.eval(env));
        System.out.println(e2.eval(env));
        System.out.println(e3.eval(env));
        System.out.println(e4.eval(env));

        //Exercise 1.4 (iv)
        Expr se1 = new Add(new CstI(0), new Var("e"));
        Expr se2 = new Add(new Var("e"), new CstI(0));
        Expr se3 = new Sub(new Var("e"), new CstI(0));
        Expr se4 = new Mul(new CstI(1), new Var("e"));
        Expr se5 = new Mul(new Var("e"), new CstI(1));
        Expr se6 = new Mul(new CstI(0), new Var("e"));
        Expr se7 = new Mul(new Var("e"), new CstI(0));
        Expr se8 = new Sub(new Var("e"), new Var("e"));
        Expr se9 = new Mul(new Var("e"), new Var("e"));
        Expr se10 = new Mul(new Add(new CstI(1), new CstI(0)), new Add(new Var ("x"), new CstI(0)));
        Expr se11 = new Mul(new Add(new Var("e"), new Var("e")), new Add(new Var("e"), new Var("e")));

        System.out.println(se1.simplify()); // Expected: "e"
        System.out.println(se2.simplify()); // Expected: "e"
        System.out.println(se3.simplify()); // Expected: "e"
        System.out.println(se4.simplify()); // Expected: "e"
        System.out.println(se5.simplify()); // Expected: "e"
        System.out.println(se6.simplify()); // Expected: "0"
        System.out.println(se7.simplify()); // Expected: "0"
        System.out.println(se8.simplify()); // Expected: "0"
        System.out.println(se9.simplify()); // Expected: "(val e * val e)"
        System.out.println(se10.simplify()); // Expected: "x"
        System.out.println(se11.simplify()); // Expected: "(val e + val e) * (val e + val e)"
    }

    public abstract static class Expr {
        abstract public String toString();
        //Exercise 1.4 (iii)
        abstract public int eval(HashMap<String, Integer> env);
        //Exercise 1.4 (iv)
        abstract public Expr simplify();
    }
    
    public static class CstI extends Expr{
        public int val; 
        
        public CstI(int val){
            this.val = val; 
        }

        @Override
        public String toString() {
           return ("CstI " + val);
        }

        //Exercise 1.4 (iii)
        @Override
        public int eval(HashMap<String, Integer> map) {
            return val;
        }

        //Exercise 1.4 (iv)
        @Override
        public Expr simplify() {
           return this;
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

        //Exercise 1.4 (iii)
        @Override
        public int eval(HashMap<String, Integer> map) {
            return map.get(val);
        }

        //Exercise 1.4 (iv)
        @Override
        public Expr simplify() {
            return this; 
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
            return "(" + e1 + " " + ope + " " + e2 + ")";
        }

        //Exercise 1.4 (iii)
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

        //Exercise 1.4 (iv)
        @Override
        public Expr simplify() {
            if(e1 instanceof Binop){
                this.e1 = e1.simplify();
            }
            if(e2 instanceof Binop){
                this.e2 = e2.simplify();
            }

            if (ope.equals("+")) {
                if (e1 instanceof CstI && ((CstI) e1).val == 0) {
                    return e2;
                } else if (e2 instanceof CstI && ((CstI) e2).val == 0) {
                    return e1;
                }else{
                    return this; 
                }
            } else if (ope.equals("-")) {
                if (e2 instanceof CstI && ((CstI) e2).val == 0) {
                    return e1.simplify();
                }else if(e1 instanceof Var && e2 instanceof Var){
                    return new CstI(0);
                }else{
                    return new Sub(e1, e2);
                }
            } else if (ope.equals("*")) {
                if ((e1 instanceof CstI && ((CstI) e1).val == 0) ||
                    (e2 instanceof CstI && ((CstI) e2).val == 0)) {
                    return new CstI(0);
                } else if (e1 instanceof CstI && ((CstI) e1).val == 1) {
                    return e2;
                } else if (e2 instanceof CstI && ((CstI) e2).val == 1) {
                    return e1;
                }else{
                    return this;
                }
            }else{
                return this;
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