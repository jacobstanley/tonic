interface Invoke {
    void invoke(int x);
}

class Main implements Comparable {
    private float foo;

    public Main() {
        F f = new F();
        f.x = 10.0f;
    }

    public class F {
        private float x;
    }

    //static float bar = 2.5f;

    //too hard basket :)
    //long baz = 3L;
    //double quxx = 4.8;

    public static void main(String[] args) {
        Main main = new Main();

        int x = 42;
        boolean b = x > 1;
        System.out.println("Hello World = " + b);
        System.exit(x);
    }

    //public static int add(int x, int y) {
    //    return x + y;
    //}

    //public static int sub(int x, int y) {
    //    return x - y;
    //}

    //public static int mul(int x, int y) {
    //    return x * y;
    //}

    public int compareTo(Object o) {
        return -1;
    }
}
