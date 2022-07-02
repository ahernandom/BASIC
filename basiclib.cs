// To compile this module as a DLL:
//
//                  dmcs /t:library bcuplib.cs
//
// To link this DLL to a program written in C#:
//
//                  dmcs /r:bcuplib.dll someprogram.cs

using System.Collections;

namespace Basic {
    
    using System;
    
    public class Utils {

       private static Queue dataQ = new Queue();
       private static Random rnd = new Random();
       private static Stack substack = new Stack();
       
       public static void Exit(){
            System.Environment.Exit(0);
       }
       
       public static void Push(int line) {
            substack.Push(line);
       }
       
       public static int Pop() {
            return (int)substack.Pop();
       }
       
       public static void Data(double x) {
            dataQ.Enqueue(x);
       }
       
       public static double Read(){
            if(dataQ.Count == 0){
                System.Environment.Exit(0);
            }
            return (double)dataQ.Dequeue();
       }
       
       public static double Pow(double a, double b) {
            return Math.Pow(a,Math.Abs(b));
       }
       
       public static double Sin(double x){
            return Math.Sin(x);
       }
       
       public static double Cos(double x){
            return Math.Cos(x);
       }
       
       public static double Tan(double x){
            return Math.Tan(x);
       }
       
       public static double Atn(double x){
            return Math.Atan(x);
       }
       
       public static double Exp(double x){
            return Math.Exp(x);
       }
       
       public static double Abs(double x){
            return Math.Abs(x);
       }
       
       public static double Log(double x){
            return Math.Log10(Math.Abs(x));
       }
       
       public static double Sqr(double x){
            return Math.Sqrt(Math.Abs(x));
       }
       
       public static double Rnd(double x){
            return rnd.NextDouble();
       }
       
       public static double Int(double x){
            return (int)x;
       }
    }
}

