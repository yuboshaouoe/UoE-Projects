public class X{
    private int x,y;

    public void m1(boolean b){
        // double x by bitshifting to the left by 1 bit
        x <<= 1; // 0
        if(b)
            x++; //
        y++; // 1
    }

    public Boolean m2(){
        // if y == 0
        if(m6())
            return null;
        // if x is odd
        return x % 2 != 0;
    }

    public Boolean m3(){
        // null or x % 2 != 0
        Boolean z = m2();
        if(z != null){
            // unsigned right bitshift -> divide by 2
            // it doesn't preserve the sign bit
            x>>>=1; // 
            y--; // 0
        }
        return z;
    }

    public void m4(){
        // reset to 0
        x=y=0;
    }

    public int m5(){
        return y;
    }

    public boolean m6(){
        return y == 0;
    }
}