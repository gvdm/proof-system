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

sealed trait AbstractSyntaxTree extends Objct

object Ast { def apply(ast: Objct) = Judgement("ast", List(ast), PostFix) }
object Symbol { def apply(sym: Objct) = Judgement("sym", List(sym)) }
object Distinct { def apply(a: Objct, b: Objct) = Judgement("#", List(a, b), InFix) }

object Char { def apply(c: Objct) = Judgement("char", List(c)) }
object Str { def apply(s: Objct, Σ: Set[Judgement] = Strng.alphabet) = Derivable(Σ, Judgement("str", List(s))) }

object Strng extends ObjctDef {

  val nullStr = Val("ε")
  //TODO: Create programmatically, for now we have a short alphabet
  val alphabetChars = Set("a", "b", "c", "d", "e", "f");
  val alphabet = alphabetChars map { c ⇒ Char(Val(c)) }

  def definition = Set(
    Axiom(Str(nullStr)) //,
    //TODO: write inductive definition of str judgement
  //InferenceRule(Set(Char(Var("c")), ),
  //    Str)
  )

  def rules = Set()
}

