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

    public static void main(String[] args) {
        odd(19);
    }

    private static boolean odd(int x) {
        boolean b = x == 27;
        if (b) {
            return false;
        } else {
            return even(x - 1);
        }
    }

    private static boolean even(int x) {
        boolean b = x == 0;
        if (b) {
            return true;
        } else {
            return odd(x - 1);
        }
    }

    public int compareTo(Object o) {
        return -1;
    }
}
