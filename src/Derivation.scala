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

case class Derivation(statement: Judgement, derivations: Set[Derivation], reason: Rule) {
  // pretty printing is ugly
  override def toString() = {
    if (derivations.size == 0) statement.toString
    else {
      val statementStr = statement.toString
      val derStrs = derivations.toList.map(_.toString.lines.toList).map(_.reverse)
      val height = derStrs.map(_.length).max
      val ders = derStrs.map(_.padTo(height, "")).map(_.reverse)
      val dersLines = {
        if (ders.size == 1) ders
        else if (ders.size == 2) ders.head.map(_ + "    ") :: ders.tail
        else ders.head.map(_ + "  ") :: (ders.tail.init.map(_.map("  " + _ + "  ")) :+ ders.tail.last.map("  " + _))
      }
      val derBlocks = dersLines.map { derLines ⇒
        val w = derLines.map(_.length).max
        derLines.map(line ⇒ ("".padTo((w - line.length) / 2, " ").mkString + line).padTo(w, " ").mkString)
      }
      //val derBlocksPad = derBlocks.map(_.map(dline => ("".padTo((width - dline.length), " ")+dline).mkString))
      val derBlockString = derBlocks.transpose.map(_.foldLeft("")(_ + _)).map(_ + "\n").mkString
      val width = maxWidthString(statementStr + "\n" + derBlockString)
      derBlockString + makeRuleLine(width) + "\n".padTo((width - statementStr.length) / 2, " ").mkString + statementStr
    }
  }
}

class Derive(theorem: Judgement, context: Set[Rule] = Rules.rules) {
  def emptyEnv = new collection.immutable.HashMap[Var, Objct]

  // TODO: start a thread doing forward and backwards derivation and return whichever finishes first
  // what about checking forward and backward match?
  def derive() = null

  // TODO: return type represent possible failure to derive, using Maybe or perhaps a more
  // complex type allowing reasons to be given for failure, read on exceptions/errors
  def backward(): Derivation = {
    var theoremToProve = theorem
    var theoremContext = context

    // check whether we are checking for derivability or admissibility and adjust rule set accordingly
    theoremToProve match {
      // TODO: derivable judgements that rely on rules which contain statements of derivability seem not to work
      case Derivable(h, s)  ⇒ { theoremContext ++= h.map(Axiom(_)); theoremToProve = s }
      // TODO: exhaustively show all derivations for sure
      case Admissable(h, s) ⇒ null
      case _                ⇒ null
    }

    for (r ← theoremContext) {
      var rule: Rule = r

      // treat rules with derivability judgements as inference rules
      if (isDerivabilityJudgement(rule.statement)) {
        var derivabilityPremises: Set[Judgement] = Set()
        var derivabilityConclusion: Judgement = null
        rule match {
          case Axiom(axiom) ⇒ axiom match {
            case Derivable(h, c) ⇒ { derivabilityPremises = h; derivabilityConclusion = c }
            case _               ⇒ throw InvalidJudgementException("We checked it was derivable so this shouldn't be able to happen")
          }
          case InferenceRule(premises, conclusion) ⇒ conclusion match {
            // TODO: I'm pretty sure that if we have derivability statements as premises this will work, check sometime (12/7/12 - if it ain't a problem it ain't a problem)
            case Derivable(h, c) ⇒ { theoremContext ++= h.map(Axiom(_)); derivabilityPremises = premises; derivabilityConclusion = c }
            case _               ⇒ throw InvalidJudgementException("We checked it was derivable so this shouldn't be able to happen")
          }
        }
        rule = InferenceRule(derivabilityPremises, derivabilityConclusion)
      }

      if (theoremToProve.symbol == rule.statement.symbol) {
        try {
          val varValues: Objct#EnvMap = rule.statement.matchVarObj(emptyEnv, theoremToProve)

          rule match {
            case Axiom(a) ⇒ {
              val derivedJudgement = a.replaceVars(varValues)
              // the axiom matches, are the objects in the judgement the same?
              if (derivedJudgement.judge(theoremToProve)) {
                // we have a derivation with no premises, end search
                return Derivation(derivedJudgement, Set(), rule)
              }
            }

            case InferenceRule(premises, conclusion) ⇒ {
              // replace the parameters in the premises of the matching rule with the concrete
              // values given in the theorem
              val premisesReplaced = premises.map(_.replaceVars(varValues))
              // for each premise, find its derivation to complete the derivation for this theorem
              return Derivation(theoremToProve, premisesReplaced map (new Derive(_, theoremContext /*+Axiom(theorem) this will improve derivations as not having to reprove proven theorems*/ )
                .backward), rule)
            }
          }
        } catch {
          // TODO: handle incorrectly formed expressions
          case VariableUniquenessException       ⇒ throw new Error("VariableUniquenessException")
          // will happen when judgements are applied to different objct forms
          case InvalidJudgementException(reason) ⇒ null //println(reason)
          case ObjctMismatch                     ⇒ null //println("IncorrectJudgemntObjct") //return Derivation(theorem, Set(), Axiom(Judgement("⊥", List(theorem))))
        }
      }
    }
    throw new Error("Did not find derivation of " + theorem)
  }

  // TODO: forward derivation will never stop if given an invalid object construction
  // will we leave typing of objcts to the objct language? should we check this before searching?
  // should we then also do this check on backwards derivations?
  def forward(): Derivation = {
    // TODO: merge with context? rename context?
    var validDerivations: Set[Derivation] = Set() // ATM this should store no objcts with vars
    // as far as i can figure, this will be necessary with hypothetical judgements but
    // that is another day, check what happens with parameteric judgements too

    // TODO: do we need to check for valid objct structure?
    //if (theorem.subjects.map(o => o.matchVarObj(Map((Var("a"), o)), o)))

    while (!validDerivations.exists(_.statement == theorem)) {
      // accumulate the derivations that are found in the iteration instead of adding immediately
      // to the set of valid derivations so that ordering of rules does not affect search priority
      // (not doing so results in a deep search on the first few rules)
      var newDerivations: Set[Derivation] = Set()

      // filter rules to just the relevant ones
      def relevantRules(scope: Judgement, ruleSet: Set[Rule] = Set()): Set[Rule] = {
        // if we don't already have the rule as relevant and the judgement is the same add it
        context.filter(rule ⇒ !(ruleSet contains rule) && rule.statement.symbol == scope.symbol).map {
          _ match {
            case a: Axiom            ⇒ Set(a): Set[Rule]
            // and all the rules that may apply to their premises
            case rule: InferenceRule ⇒ Set(rule) ++ rule.premises.flatMap { relevantRules(_, ruleSet + rule) }: Set[Rule]
          }
        }.foldLeft(Set[Rule]()) { _ ++ _ } // as one set
      }

      for (rule ← relevantRules(theorem)) {

        val variables = rule.statement.subjects.flatMap(_.vars) toSet // same as distinct, but maybe makes it faster?

        // for every variable, map to every object (that has been seen in validDerivations)
        // the cartesian product of variables and objcts gives all the possible combinations 
        // then we construct every unique grouping of variables to create environments that map variables
        // uniquely and cover every object
        var varReplacements: Traversable[Traversable[(Var, Objct)]] = Traversable(Traversable())
        for (v ← cartesianProduct(variables, validDerivations.map(_.statement).flatMap(_.subjects))) {
          varReplacements = v.flatMap(vo ⇒ varReplacements.map(_ ++ Set(vo))) toSet
        } // varReplacements is now objects^variables large, luckily, most rules do not use /too/ many vars

        // can't work due to semi recursive/iterative nature of building unique var lists, henc above implementation
        //        val varReplacements = cartesianProduct(variables, validDerivations.map(_.statement).flatMap(_.subjects)).map(_.flatMap(vo ⇒ varReplacements.map(_ ++ Set(vo))))

        // create a new environment for every possible mapping
        val environments = varReplacements.map(_.foldLeft(emptyEnv)((env, varObjTuple) ⇒ env + (varObjTuple._1 -> varObjTuple._2)))

        rule match {
          case Axiom(a) ⇒ environments.foreach { env ⇒ newDerivations += Derivation(a.replaceVars(env), Set(), rule) }
          case InferenceRule(premises, conclusion) ⇒ {

            // TODO: figure out why this beautiful functional algorithm doesn't halt
            //environments.zip(environments.map(env ⇒ premises.map(_.replaceVars(env)))).
            //  filter { case (env, ps) ⇒ ps.subsetOf(validDerivations.map(_.statement)) }.
            //  foreach {
            //    case (env, ps) ⇒ new Derivation(conclusion.replaceVars(env),
            //      ps.map(p ⇒ validDerivations.find(p == _.statement).get))
            //  }

            // should be equivalent to above ^^s 
            val replacedPremises = environments.map(env ⇒ (env, premises.map(_.replaceVars(env))))
            replacedPremises.foreach { envPremises ⇒
              {
                if (envPremises._2.subsetOf(validDerivations.map(_.statement))) {
                  newDerivations += Derivation(conclusion.replaceVars(envPremises._1),
                    envPremises._2.map(p ⇒ validDerivations.find(p == _.statement).get), rule)
                }
              }
            }
          }
        }
      }
      validDerivations ++= newDerivations
    }
    return validDerivations.find(_.statement == theorem).get
  }

  def isDerivabilityJudgement(j: Judgement): Boolean = j.symbol == "⊢"

  def derivableJudgement(derivable: Objct): Judgement = try {
    derivable match {
      // asInstanceOf should be safe as the Objcts in a ⊢ Judgement should be Judgements themselves
      case Judgement("⊢", subs, fix) ⇒ subs.last.asInstanceOf[Judgement]
      case _                         ⇒ throw InvalidJudgementException("Objct is not a derivability judgement")
    }
  } catch {
    // but if it isn't we throw a nice exception
    case e: ClassCastException ⇒ throw InvalidJudgementException("Derivablility judgement does not have judgements as subjects")
  }

  // because we want to uniformly handle derivable statements
  def judgementStatement(j: Judgement) = if (isDerivabilityJudgement(j)) derivableJudgement(j) else j

  def derivableJudgementStatements(js: Traversable[Judgement]) = js.map(judgementStatement(_))
}
