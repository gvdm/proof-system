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

sealed abstract case class Rule(statement: Judgement) extends Objct
// TODO: turn into extractors? unify with derivations? what about hypothetical judgements and derivability?
case class InferenceRule(premises: Set[Judgement], conclusion: Judgement) extends Rule(conclusion) {
  override def toString() = premises.mkString(", ") + "\n" +
    makeRuleLine(maxWidthString(premises.mkString(", ") + "\n" + conclusion)) + "\n" +
    conclusion
}
case class Axiom(axiom: Judgement) extends Rule(axiom) {
  override def toString() = makeRuleLine(axiom.toString.length) + "\n" + axiom
}
//object Axiom {
//  def apply(axiom: Judgement) = new InferenceRule(Set(), axiom)
//  override def toString() = makeRuleLine(axiom.toString.length) + "\n" + axiom
//}

object Rules {
  def objStructureDefs = naturalDefinition ++ treeDefinition ++ listDefinition
  def rules = objStructureDefs ++ judgementProperties ++ naturalsRules

  // should be true of any objcts that this applies to
  // TODO: think about the consequences of judgement overloading
  def judgementProperties = Set(
    Axiom(Eq(Var("a"), Var("a")))
  )

  def naturalDefinition = Set(
    // church encoding of naturals
    Axiom(Nat(Zero)),
    InferenceRule(Set(Nat(Var("a"))),
      Nat(Succ(Var("a"))))
  )

  def naturalsRules = naturalEquality ++ naturalParity ++ naturalAddition ++ naturalMax
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

  def treeDefinition = Set(
    Axiom(Tr(Leaf)),
    InferenceRule(Set(Tr(Var("a")), Tr(Var("b"))),
      Tr(Branch(Var("a"), Var("b"))))
  )

  def listDefinition = Set(
    Axiom(Lt(Nl)),
    InferenceRule(Set(Nat(Var("a")), Lt(Var("b"))),
      Lt(Cons(Var("a"), Var("b"))))
  )

  def iteratedTransitionDefinition = Set(
    Axiom(IteratedTransition(Var("s"), Var("s"))),
    InferenceRule(Set(Transition(Var("s"), Var("s1")), IteratedTransition(Var("s1"), Var("s2"))),
      IteratedTransition(Var("s"), Var("s2")))
  )
}