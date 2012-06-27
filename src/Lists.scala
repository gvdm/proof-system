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

object Lst { def apply(list: Objct) = Judgement("list", List(list), PostFix) }
case object Nl extends Objct
case class Cons(head: Objct, tail: Objct) extends Objct {
  override def matchVarObj(e: EnvMap, o: Objct): EnvMap = {
    o match {
      case Cons(h, t) ⇒ e ++ head.matchVarObj(e, h) ++ (tail.matchVarObj(e, t))
      case _          ⇒ throw IncorrectJudgemntObjct
    }
  }
  override def vars = head.vars ++ tail.vars
  override def replaceVars(e: EnvMap): Objct = Cons(head.replaceVars(e), tail.replaceVars(e))
}

object Lists extends ObjctDef {
  def definition = Set(
    Axiom(Lst(Nl)),
    InferenceRule(Set(Nat(Var("a")), Lst(Var("b"))),
      Lst(Cons(Var("a"), Var("b"))))
  )
  def rules = Set()
}