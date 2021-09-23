package co.s4n.calnat

import scala.io.StdIn

object Main extends App {
 def leerInt(prompt:String):Int = {
 val s = StdIn.readLine(prompt)
 s.toInt
 }
 def esCero(nat:Nat) = nat match {
 case Cero() => true
 case Suc(nat) => false
 }	
 def esMayorIgual(nat1:Nat,nat2:Nat):Boolean = nat1 match {
 case Cero() => nat2 match {
  case Cero() => true
  case _ => false
  }
 case Suc(pnat) => nat2 match {
  case Cero() => true
  case Suc(snat) => esMayorIgual(pnat,snat)
  }
 }

 def conIntANat(i:Int):Nat = i match {
  case 0 => Cero()
  case i => Suc(conIntANat(i-1))
 }
// def imprimirNat(nat:Nat):String =  {
//  val r = contIntAnat...  
// }
// def natToInt(nat1:Nat):Int = (nat1) match {
// case 0 => 0
// case Suc(nat1) 
// }
// def restaNat(nat1:Nat,nat2:Nat):Nat = (nat1,nat2) match {
// case (Cero(),nat2) => nat2
// case (nat1,Cero()) => nat1
// case (nat1,nat2) => 
// }  


//main

val n = leerInt("Ingrese su entero: ")
val r = conIntANat(n)
print(r)

}
