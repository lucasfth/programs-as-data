import java.util.Arrays;
import java.util.Scanner;

public class test {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        System.out.println("Input first array with int and whitespace between");
        int[] lst1 = Arrays.stream(sc.nextLine().split(" ")).mapToInt(Integer::parseInt).toArray();
        
        System.out.println("Input second array with int and whitespace between");
        int[] lst2 = Arrays.stream(sc.nextLine().split(" ")).mapToInt(Integer::parseInt).toArray();
        
        int[] mrg = merge(lst1, lst2);

        System.out.println("Merged: " + Arrays.toString(mrg));
    }

    public static int[] merge(int[] lst1, int[] lst2){
        int[] merged = new int [(lst1.length + lst2.length)];
        int cntm = 0, cnt1 = 0, cnt2 = 0;

        while (cnt1 < lst1.length && cnt2 < lst2.length){
            if (lst1[cnt1] < lst2[cnt2]) {
                merged[cntm++] = lst1[cnt1++];
            } else {
                merged[cntm++] = lst2[cnt2++];
            }
        }

        while (cnt1 < lst1.length) {
            merged[cntm++] = lst1[cnt1++];
        }

        while (cnt2 < lst2.length) {
            merged[cntm++] = lst2[cnt2++];
        }

        return merged;
    }
}