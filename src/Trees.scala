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

object Tr { def apply(tree: Objct) = Judgement("tree", List(tree), PostFix) }

case object Leaf extends Objct
case class Branch(left: Objct, right: Objct) extends Objct {
  override def matchVarObj(e: EnvMap, o: Objct) = {
    o match {
      case Branch(l, r) ⇒ e ++ left.matchVarObj(e, l) ++ (right.matchVarObj(e, r))
      case _            ⇒ throw IncorrectJudgemntObjct
    }
  }
  override def vars = left.vars ++ right.vars
  override def replaceVars(e: EnvMap): Objct = Branch(left.replaceVars(e), right.replaceVars(e))
}

object Trees extends ObjctDef {
  def definition = Set(
    Axiom(Tr(Leaf)),
    InferenceRule(Set(Tr(Var("a")), Tr(Var("b"))),
      Tr(Branch(Var("a"), Var("b"))))
  )
  def rules = Set()
}