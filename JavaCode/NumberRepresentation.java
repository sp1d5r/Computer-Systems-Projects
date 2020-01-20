// TODO: need to make the following functions: decToBin
//  (subtraction method and division method), decToOct, decToHex, binToDec, binToOct,
import java.util.ArrayList;
import java.util.List;
import java.util.Collections;

public class NumberRepresentation {

    public static void main(String[] args){
        NumberRepresentation test = new NumberRepresentation();
        test.decToBin (25);
    }


    public void decToBin (int decimalValue)  {
        // convert the decimal value into a binary value using the div
        // easy to use java's built in functions
        // String bin = Integer.toBinaryString(decimalValue);
        // System.out.println(bin);

        List<Integer> remaider = new ArrayList<>();
        int qoutient = decimalValue;
        while (decimalValue !=0) {
            qoutient = decimalValue / 2;
            remaider.add (qoutient % 2);
            decimalValue = qoutient;
        }
        Collections.reverse(remaider);
        System.out.println(remaider);


    }


}
