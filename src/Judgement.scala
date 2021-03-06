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

case class InvalidJudgementException(reason: String) extends Throwable

sealed abstract class NFix
case object PreFix extends NFix
case object InFix extends NFix
case object LastFix extends NFix
case object DerivFix extends NFix
case object PostFix extends NFix

// TODO: change to extractor pattern
case class Judgement(symbol: String, subjects: List[Objct], fix: NFix = PreFix, derivSymbol: String = null) extends Objct {
  override def toString() = fix match {
    case PreFix   ⇒ symbol + "(" + subjects.mkString(", ") + ")"
    case InFix    ⇒ subjects.head + " " + symbol + " " + subjects.tail.mkString(", ")
    case LastFix  ⇒ subjects.init.mkString(", ") + " " + symbol + " " + subjects.last
    case DerivFix ⇒ (if (derivSymbol == null) "Γ" else derivSymbol) + " " + symbol + " " + subjects.last
    case PostFix  ⇒ subjects.mkString + " " + symbol
  }

  override def matchVarObj(e: EnvMap, o: Objct): EnvMap = {
    o match {
      case Judgement(sym, subs, fix, dsym) ⇒ if (sym == symbol) {
        var varValues: EnvMap = new collection.immutable.HashMap[Var, Objct]
        for ((obsub, thsub) ← subjects zip subs) {
          val newVarAssignments = obsub.matchVarObj(varValues, thsub)
          val sharedVars = varValues.keySet.intersect(newVarAssignments.keySet)
          if (sharedVars != Set() && sharedVars.exists { v ⇒ varValues(v) != newVarAssignments(v) })
            throw VariableUniquenessException // came across a variable that was given different mappings
          if (thsub != obsub.replaceVars(newVarAssignments))
            throw InvalidJudgementException("Judgements are not made on the same subjects")
          varValues = varValues ++ newVarAssignments
        }
        return varValues
      } else throw InvalidJudgementException("Judgements are not the same (will this ever get called if symbols are always checked first? prereq?)")
      case _ ⇒ throw ObjctMismatch
    }
  }
  override def vars = (subjects map { _.vars }) reduceLeft { _ ++ _ }
  override def replaceVars(env: Objct#EnvMap): Judgement = {
    this copy (subjects = (this.subjects).map(_.replaceVars(env)))
  }

  /* checks that judgement is true against a given 'true' */
  def judge(given: Judgement): Boolean = {
    // TODO: use asserts instead?
    // same judgement being made (would this ever be called if that weren't the case?
    if (this.symbol != given.symbol) return false //maybe uneccesary?
    // same number of objects being judged
    if (this.subjects.size != given.subjects.size) return false //maybe uneccesary?
    // all the objects must be the same
    return (this.subjects, given.subjects).zipped.map(_ == _).forall(b ⇒ b)
    //for ((o1, o2) ← j.subjects.zip(given.subjects)) if (o1 != o2) return false // <- works
    //j.subjects.zip(given.subjects).exists(l => l match { case (o1, o2) => o1 != o2 })
    //return true
  }
}

object Derivable {
  def apply(hypothesis: Judgement, consequent: Judgement) =
    deriveJudgement(List(hypothesis), consequent)
  def apply(hypothesis: Set[Judgement], consequent: Judgement) =
    deriveJudgement((hypothesis toList), consequent)
  def apply(hypothesis: Set[Judgement], consequent: Judgement, derivSymbol: String) =
    deriveJudgement((hypothesis toList), consequent, derivSymbol)
    
  def deriveJudgement(hypothesis: List[Judgement], consequent: Judgement, derivSymbol: String = null) =
    Judgement("⊢", hypothesis :+ consequent, if (derivSymbol != null) DerivFix else LastFix, derivSymbol)
    
  def unapply(j: Judgement): Option[(Set[Judgement], Judgement)] = {
    if (j.symbol == "⊢") try {
      Some((j.subjects.init.map(_.asInstanceOf[Judgement]) toSet, j.subjects.last.asInstanceOf[Judgement]))
    } catch {
      case e: ClassCastException ⇒ throw InvalidJudgementException("Derivablility judgement does not have judgements as subjects")
    }
    else None
  }
  def unapply(rule: InferenceRule): Judgement =
    Judgement("⊢", (rule.premises toList) :+ rule.conclusion, LastFix)
  def unapply(str: String): Judgement = {
    // TODO: parse "x,y,z ⊢ d" into Derivable(List(x,y,z), d))
    null
  }
}

object Admissable {
  def apply(hypothesis: Judgement, consequent: Judgement) = Judgement("⊨", List(hypothesis, consequent), LastFix)
  def apply(hypothesis: List[Judgement], consequent: Judgement) = Judgement("⊨", hypothesis :+ consequent, LastFix)
  def unapply(j: Judgement): Option[(Set[Judgement], Judgement)] = {
    if (j.symbol == "⊨") try {
      Some((j.subjects.init.map(_.asInstanceOf[Judgement]) toSet, j.subjects.last.asInstanceOf[Judgement]))
    } catch {
      case e: ClassCastException ⇒ throw InvalidJudgementException("Derivablility judgement does not have judgements as subjects")
    }
    else None
  }
}

// TODO: parametric judgements
object Parametric {
  def apply(params: Set[Var], j: Judgement) = Judgement("|", (params toList) :+ j, LastFix)
}

object Eq {
  def apply(a: Objct, b: Objct) = Judgement("=", List(a, b), InFix)
  //def unapply(o: Objct): Option[Judgement] = o match {
  //  case j@Judgement("=", subs, fix) ⇒ Some(j)
  //  case _                           ⇒ None
  //}
  //def unapply(s: String): Option[Judgement] = try {
  //Some(Judgement("=", List(s.split("=")(0), s.split("=")(1)), InFix))
  //} catch {
  //  case _ => None
  //}
}

object IsType { def apply(typ: Objct) = Judgement("type", List(typ), PostFix) }
object HasType { def apply(expression: Objct, typ: Objct) = Judgement(":", List(expression, typ), InFix) }

object HasValue { def apply(expression: Objct, value: Objct) = Judgement("⇓", List(expression, value), InFix) }
