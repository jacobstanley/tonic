// letrec odd = \x ->
//          if x == 0
//          then false
//          else y = x -1
//               even y
//
//        even = \x ->
//          if x == 0
//          then true
//          else y = x - 1
//               odd y
//
// res = odd 19
// out = getstatic System.out
// if res
// then invokevirtual PrintStream.println out "odd"
// else invokevirtual PrintStream.println out "even"

////////////////////////////////////////////////////////////////////////
// Direct

boolean odd(int x) {
    if (x == 0) {
        return false;
    } else {
        y = x - 1;
        even(y);
    }
}

boolean even(int x) {
    if (x == 0) {
        return true;
    } else {
        y = x - 1;
        odd(y);
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
    FIZ even;
    void init(FIZ even) {
        this.even = even;
    }
    boolean invoke(int x) {
        if (x == 0) {
            return false;
        } else {
            y = x - 1;
            even.invoke(y);
        }
    }
}

class EvenFun implements FIZ {
    FIZ odd;
    void init(FIZ odd) {
        this.odd = odd;
    }
    boolean invoke(int x) {
        if (x == 0) {
            return true;
        } else {
            y = x - 1;
            odd.invoke(y);
        }
    }
}

void main() {
    odd = new OddFun();
    even = new EvenFun();
    odd.init(even);
    even.init(odd);

    res = odd.invoke(19);
    out = System.out;
    if (res) {
        out.println("19 is odd");
    } else {
        out.println("19 is even");
    }
}
