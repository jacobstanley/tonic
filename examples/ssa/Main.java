import java.io.PrintStream;

public final class Main
{
    public static final void main(String[] args)
    {
        float f1 = 42.0F;
        float f2 = f1 + f1;
        float f3 = f1 * f1;
        float f4 = f3 - f2;
        String str = Float.toString(f4);
        PrintStream out = System.out;
        out.println(str);
    }
}
