// letrec odd = \x ->
//          letrec even = \x ->
//              if x == 0
//              then true
//              else x1 = x - 1
//                   odd x1
//
//          if x == 0
//          then false
//          else x1 = x - 1
//               even x1
//
// res = odd 19
// out = getstatic System.out
// if res
// then invokevirtual PrintStream.println out "odd"
// else invokevirtual PrintStream.println out "even"

////////////////////////////////////////////////////////////////////////
// Direct

boolean odd(int odd_x) {
odd:
    if (odd_x == 0) {
        return false;
    } else {
        odd_x1 = odd_x - 1;

        even_x = odd_x1;
        goto even;
    }

even:
    if (even_x == 0) {
        return true;
    } else {
        even_x1 = even_x - 1;

        odd_x = odd_x1;
        goto odd;
    }
}

void main() {
    res = odd(19);
    out = System.out;
    if (res) {
        out.println("19 is odd");
    } else {
        out.println("19 is even");
    }
}

////////////////////////////////////////////////////////////////////////
// Indirect

interface FIZ {
    boolean invoke(int x);
}

class OddFun implements FIZ {
    boolean invoke(int odd_x) {
odd:
        if (odd_x == 0) {
            return false;
        } else {
            odd_x1 = odd_x - 1;

            even_x = odd_x1;
            goto even;
        }

even:
        if (even_x == 0) {
            return true;
        } else {
            even_x1 = even_x - 1;

            odd_x = odd_x1;
            goto odd;
        }
    }
}

void main() {
    odd = new OddFun();

    res = odd.invoke(19);
    out = System.out;
    if (res) {
        out.println("19 is odd");
    } else {
        out.println("19 is even");
    }
}
