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

 def imprimirNat(nat:Nat):String = nat match{
  case Cero() => "No se puede hacer la operacion"
  case nat => nat.toString
 }

// n1
def restaNat(nat1:Nat,nat2:Nat):Nat = (nat1,nat2) match {
 //case (nat1,Cero()) => nat1
 case (Suc(nat1),Cero()) => Suc(nat1)
 case (nat1,Cero()) => nat1
// esta linea da para ambos casos case (Cero(),nat2) => nat2
 case (Suc(nat1),Suc(nat2)) if esMayorIgual(nat1,nat2) => restaNat(nat1,nat2)  
 case _ => Cero()  
 }  

//main

val n = leerInt("Ingrese su entero: ")
val m = leerInt("Ingrese otro entero: ")
val r = restaNat(conIntANat(n),conIntANat(m))
val a = print("" + imprimirNat(r))
}
