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
  // TODO: find a code pattern to collect all subclasses of ObjctDef so as to not have to update
  // here when adding new types
  def objctTypes = Set(Naturals, Trees, Lists, Strings)
  
  def objStructureDefs = objctTypes.map(_.definition).flatten
  def rules = objStructureDefs ++ objctTypes.map(_.rules).flatten// ++ judgementProperties

  // should be true of any objcts that this applies to
  // TODO: think about the consequences of judgement overloading
  // found a consequence, equivalency is not the same as identity, and variables should map to a unique objct
  //def judgementProperties = Set(
  //  Axiom(Eq(Var("a"), Var("a")))
  //)
}
