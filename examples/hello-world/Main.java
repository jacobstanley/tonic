class Main implements Comparable {
    static int foo = 1;
    //static float bar = 2.5f;

    //too hard basket :)
    //long baz = 3L;
    //double quxx = 4.8;

    public static void main(String[] args) {
        //int z = add(foo, sub(3, 2));
        System.out.println("Hello World!");
        System.exit(42);
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
