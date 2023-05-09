package com.javalearning;


public class Vector3D {
    private double x;
    private double y;
    private double z;

    public Vector3D(double x, double y, double z){
        this.x = x;
        this.y = y;
        this.z = z;
    }

    public static Vector3D add(Vector3D lhs, Vector3D rhs){
        return new Vector3D(lhs.x + rhs.x, lhs.y + rhs.y, lhs.z + rhs.z);
    }

    public static Vector3D scale(Vector3D v, double scaleFactor){
        return new Vector3D(v.x * scaleFactor, v.y * scaleFactor, v.z * scaleFactor);
    }

    public static Vector3D subtract(Vector3D lhs, Vector3D rhs){
        return add(lhs, scale(rhs, -1));
    }

    private double calculateR(){
        return Math.sqrt(x * x + y * y + z * z);
    }

    private double calculateTheta(){
        return Math.acos(z / calculateR());
    }

    private double calculatePhi(){
        return Math.atan2(y, x);
    }

    public double getR() {
        return calculateR();
    }

    public double getPhi() {
        return calculatePhi();
    }

    public double getTheta() {
        return calculateTheta();
    }
}
