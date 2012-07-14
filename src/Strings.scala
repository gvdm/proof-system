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

object Chr {
  def apply(c: Objct, Σ: Set[Judgement] = Strings.alphabet) =
    Derivable(Σ, Judgement("char", List(c), PostFix)) /*{
    override def toString = "Σ " + symbol + " " + subjects.last
  }*/ // TODO: make better toString for char and str judgements (collapse alphabet)

  //  def unapply(c: Object): Option[Objct] = c match {
  //    case Judgement(sym, List(o), fix) => Some(o)
  //    case _ => None
  //  }
}

object Str {
  def apply(s: Objct, Σ: Set[Judgement] = Strings.alphabet) =
    Derivable(Σ, Judgement("str", List(s), PostFix))
  //def apply(c: Objct, s: Objct, Σ: Set[Judgement]) =
  //  Derivable(Σ, Judgement("str", List(c, s), PostFix))

  //  def unapply(o: Objct): Option[] = o
}

case class Character(c: Objct) extends Objct {

  override def toString() = c match {
    case Val(char) ⇒ char.toString()
  }

  override def matchVarObj(e: EnvMap, o: Objct): EnvMap = o match {
    case Character(d) ⇒ c.matchVarObj(e, d)
    case _            ⇒ throw ObjctMismatch
  }

  override def vars = c match {
    case v: Var ⇒ Set(v)
    case _      ⇒ Set()
  }
  override def replaceVars(e: EnvMap): Objct = Character(c.replaceVars(e))
}

case class StringCons(c: Objct, s: Objct) extends Objct {

  override def matchVarObj(e: EnvMap, o: Objct): EnvMap = {
    o match {
      case StringCons(char, string) ⇒ c.matchVarObj(e, char) ++ s.matchVarObj(e, string)
      case _                        ⇒ throw ObjctMismatch
    }
  }
  override def vars = c.vars ++ s.vars
  override def replaceVars(e: EnvMap): Objct = StringCons(c.replaceVars(e), s.replaceVars(e))
}

object StringObjct {
  def apply(str: String): Objct =
    if (str == "") Strings.nullStr
    else StringCons(Character(Val(str.head)), StringObjct(str.tail))
}

object StringCat {
  def apply(s1: Objct, s2: Objct, str: Objct) =
    // TODO: is equivalence meaning what we think it is?
    Eq(Judgement("^", List(s1, s2), InFix), str)
}

object Strings extends ObjctDef {
  val nullStr = Val("ε")
  val alphabetChars = ('a' to 'z') ++ ('A' to 'Z') toSet
  val alphabet = alphabetChars map { c ⇒ Judgement("char", List(Character(Val(c))), PostFix) }

  def definition = Set(
    // string induction
    Axiom(Str(nullStr)),
    InferenceRule(Set(Chr(Var("c")), Str(Var("s"))),
      Str(StringCons(Var("c"), Var("s"))))
  )

  /* TODO: should we rework the rules to include 'type restraints' in the form of type
     judgements in the premises (change the axiom to inference rule). PFFP seems loose
     and fast in there definitions with liberal use of syntax, mixing in char/str/nat into
     'higher level' judgements, see the naturals rules for another example of premising
     inference rules with type constraints */
  def rules = Set(
    // string concatenation
    Axiom(StringCat(nullStr, Var("s"), Str(Var("s")))),
    InferenceRule(Set(StringCat(Var("s1"), Var("s2"), Var("s"))),
      StringCat(StringCons(Var("c"), Var("s1")), Var("s2"), StringCons(Var("c"), Var("s"))))
  )
}
