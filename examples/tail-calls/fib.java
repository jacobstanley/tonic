// letrec fib = \n ->
//          letrec go = \n x y ->
//              if n == 0
//              then x
//              else let n1 = n - 1
//                   let xy = x + y
//                   go n1 y xy
//          go n 0 1
//
// a = fib 10000
// str = invokestatic Integer.toString a
// out = getstatic System.out
// invokevirtual PrintStream.println out str

////////////////////////////////////////////////////////////////////////
// Direct

int fib(int fib_n) {
    go_n = fib_n;
    go_x = 0;
    go_y = 1;
    goto go;

go:
    if (go_n == 0) {
        return go_x;
    } else {
        n1 = go_n - 1;
        xy = go_x + go_y;

        go_n = n1;
        go_x = go_y;
        go_y = xy;
        goto go;
    }
}

void main() {
    a = fib(10000);
    str = Integer.toString(a);
    out = System.out;
    out.println(str);
}

////////////////////////////////////////////////////////////////////////
// Indirect

interface FII {
    int invoke(int x);
}

class FibFun implements FII {
    int invoke(int fib_n) {
        go_n = fib_n;
        go_x = 0;
        go_y = 1;
        goto go;

go:
        if (go_n == 0) {
            return go_x;
        } else {
            n1 = go_n - 1;
            xy = go_x + go_y;

            go_n = n1;
            go_x = go_y;
            go_y = xy;
            goto go;
        }
    }
}

void main() {
    FII fib = new FibFun();

    a = fib.invoke(10000);
    str = Integer.toString(a);
    out = System.out;
    out.println(str);
}
