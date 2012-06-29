/**
 * Copyright © 2012 Gustav van der Merwe
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

object Nat { def apply(natural: Objct) = Judgement("nat", List(natural), PostFix) }
object Sum { def apply(a: Objct, b: Objct, sum: Objct) = Judgement("sum", List(a, b, sum)) }

case object Zero extends Objct
case class Succ(pre: Objct) extends Objct {
  override def matchVarObj(e: EnvMap, o: Objct): EnvMap = {
    o match {
      case Succ(n) ⇒ pre.matchVarObj(e, n)
      case _       ⇒ throw IncorrectJudgemntObjct
    }
  }
  override def vars = pre.vars
  override def replaceVars(e: EnvMap): Objct = Succ(pre.replaceVars(e))
}

object Naturals extends ObjctDef {
  def definition = Set(
    // church encoding of naturals
    Axiom(Nat(Zero)),
    InferenceRule(Set(Nat(Var("a"))),
      Nat(Succ(Var("a"))))
  )

  def rules = naturalEquality ++ naturalParity ++ naturalAddition ++ naturalMax
  //breaks on if succ(a) nat then a nat due increasing/building the theorem rather than reducing it
  // ++ naturalsTheorems

  def naturalEquality = Set(
    // equality definition
    Axiom(Eq(Zero, Zero)),
    InferenceRule(Set(Eq(Var("a"), Var("b"))),
      Eq(Succ(Var("a")), Succ(Var("b"))))
  )

  def naturalParity = Set(
    // even and odd numbers
    Axiom(Judgement("even", List(Zero))),
    InferenceRule(Set(Judgement("odd", List(Var("a")))),
      Judgement("even", List(Succ(Var("a"))))),
    InferenceRule(Set(Judgement("even", List(Var("a")))),
      Judgement("odd", List(Succ(Var("a")))))
  )

  def naturalAddition = Set(
    // sum definition (uniqueness not proved/shown here)
    InferenceRule(Set(Nat(Var("a"))),
      Sum(Zero, Var("a"), Var("a"))),
    InferenceRule(Set(Nat(Var("a")), Nat(Var("b")), Nat(Var("c")), Sum(Var("a"), Var("b"), Var("c"))),
      Sum(Succ(Var("a")), Var("b"), Succ(Var("c"))))
  )

  def naturalMax = Set(
    // max of two numbers is third
    InferenceRule(Set(Nat(Var("a"))),
      Judgement("max", List(Var("a"), Zero, Var("a")))),
    InferenceRule(Set(Nat(Var("a"))),
      Judgement("max", List(Zero, Var("a"), Var("a")))),
    InferenceRule(Set(Nat(Var("a")), Nat(Var("b")), Nat(Var("c")), Judgement("max", List(Var("a"), Var("b"), Var("c")))),
      Judgement("max", List(Succ(Var("a")), Succ(Var("b")), Succ(Var("c")))))
  )

  def naturalsTheorems = Set(
    // equality is reflexive
    InferenceRule(Set(Nat(Var("a"))),
      Eq(Var("a"), Var("a"))),
    // if succ(a) nat then a nat
    InferenceRule(Set(Nat(Succ(Var("a")))),
      Nat(Var("a")))
  )
}
