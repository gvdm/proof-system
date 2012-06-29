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

object InvalidJudgementException extends Throwable

sealed abstract class NFix
case object PreFix extends NFix
case object InFix extends NFix
case object LastFix extends NFix
case object PostFix extends NFix

// TODO: change to extractor pattern
case class Judgement(symbol: String, subjects: List[Objct], fix: NFix = PreFix) extends Objct {
  override def toString() = fix match {
    case PreFix  ⇒ symbol + "(" + subjects.mkString(", ") + ")"
    case InFix   ⇒ subjects.head + " " + symbol + " " + subjects.tail.mkString(", ")
    case LastFix ⇒ subjects.init.mkString(", ") + " " + symbol + " " + subjects.last
    case PostFix ⇒ subjects.mkString + " " + symbol
  }

  override def matchVarObj(e: EnvMap, o: Objct): EnvMap = {
    o match {
      case Judgement(sym, subs, fix) ⇒ if (sym == symbol) {
        var varValues: EnvMap = new collection.immutable.HashMap[Var, Objct]
        for ((obsub, thsub) ← subjects zip subs) {
          val newVarAssignments = obsub.matchVarObj(varValues, thsub)
          val sharedVars = varValues.keySet.intersect(newVarAssignments.keySet)
          if (sharedVars != Set() && sharedVars.exists { v ⇒ varValues(v) != newVarAssignments(v) })
            throw VariableUniquenessException // came across a variable that was given different mappings
          if (thsub != obsub.replaceVars(newVarAssignments))
            throw InvalidJudgementException
          varValues = varValues ++ newVarAssignments
        }
        return varValues
      } else throw IncorrectJudgemntObjct
      case _ ⇒ throw IncorrectJudgemntObjct
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
    Judgement("⊢", List(hypothesis, consequent), LastFix)
  def apply(hypothesis: Set[Judgement], consequent: Judgement) =
    Judgement("⊢", (hypothesis toList) :+ consequent, LastFix)
  def unapply(j: Judgement): Option[(Set[Judgement], Judgement)] = {
    if (j.symbol == "⊢") try {
      Some((j.subjects.init.map(_.asInstanceOf[Judgement]) toSet,
        j.subjects.last.asInstanceOf[Judgement]))
    } catch {
      // TODO: fix to be correct exception for failed cast
      case a: IllegalAccessError ⇒ None
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
    if (j.symbol == "⊢") try {
      Some((j.subjects.init.map(_.asInstanceOf[Judgement]) toSet, j.subjects.last.asInstanceOf[Judgement]))
    } catch {
      // TODO: fix to be correct exception for failed cast
      case a: IllegalAccessError ⇒ None
    }
    else None
  }
}

// TODO: parametric judgements
object Parametric {
  def apply(params: Set[Var], j: Judgement) = Judgement("|", (params toList) :+ j, LastFix)
}

object Eq { def apply(a: Objct, b: Objct) = Judgement("=", List(a, b)) }

object IsType { def apply(typ: Objct) = Judgement("type", List(typ), PostFix) }
object HasType { def apply(expression: Objct, typ: Objct) = Judgement(":", List(expression, typ), InFix) }

object HasValue { def apply(expression: Objct, value: Objct) = Judgement("⇓", List(expression, value), InFix) }
