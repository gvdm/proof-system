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

// TODO: do we need to check for valid objct structure?
class Derive(theorem: Judgement, context: Set[Rule] = Rules.rules) {
  // check whether we are checking for derivability or admissibility and adjust rule set accordingly
  val derivationContext = context ++ (theorem match {
    case Derivable(h, c)  ⇒ h.map(Axiom(_))
    // TODO: exhaustively show all derivations for sure
    case Admissable(h, c) ⇒ Set()
    case _                ⇒ Set()
  })

  def emptyEnv = new collection.immutable.HashMap[Var, Objct]

  /* given a judgement recursively filter rules to just the relevant ones */
  def relevantRules(scope: Judgement = theorem, ruleSet: Set[Rule] = Set()): Set[Rule] = {
    val scopeSymbol = judgementStatement(scope).symbol
    // if we don't already have the rule as relevant
    derivationContext.filter(rule ⇒ !(ruleSet contains rule) &&
      // and the judgement is the same
      (rule.statement.symbol == scopeSymbol ||
        // or if the rule has a ⊢ judgement as the statement, then check the judgement that the ⊢ judgement has as its consequent
        (isDerivabilityJudgement(rule.statement) && derivableJudgement(rule.statement).symbol == scopeSymbol))
    ).map {
      // then add it
      _ match {
        case a: Axiom            ⇒ Set(a): Set[Rule]
        // and all the rules that may apply to their premises
        case rule: InferenceRule ⇒ Set(rule) ++ rule.premises.flatMap { relevantRules(_, ruleSet + rule) }: Set[Rule]
      }
    }.foldLeft(Set[Rule]()) { _ ++ _ } // as one set
  }

  // TODO: start a thread doing forward and backwards derivation and return whichever finishes first
  // what about checking forward and backward match?
  def derive() = null

  // TODO: return type represent possible failure to derive, using Maybe or perhaps a more
  // complex type allowing reasons to be given for failure, mabe using exceptions/errors
  def backward(): Derivation = {

    for (rule ← relevantRules()) {
      if (judgementStatement(theorem).symbol == judgementStatement(rule.statement).symbol) {
        try {
          val varValues: Objct#EnvMap = judgementStatement(rule.statement).matchVarObj(emptyEnv, judgementStatement(theorem))

          rule match {
            case Axiom(a) ⇒ {
              val derivedJudgement = a.replaceVars(varValues)
              // the axiom matches, are the objects in the judgement the same?
              if (judgementStatement(derivedJudgement).judge(judgementStatement(theorem))) {
                // we have a derivation with no premises, end search
                return Derivation(derivedJudgement, Set(), rule)
              }
            }

            case InferenceRule(premises, conclusion) ⇒ {
              // replace the parameters in the premises of the matching rule with the concrete
              // values given in the theorem
              val premisesReplaced = premises.map(_.replaceVars(varValues))
              // for each premise, find its derivation to complete the derivation for this theorem
              return Derivation(judgementStatement(theorem), premisesReplaced map (new Derive(_, derivationContext /*+Axiom(theorem) this will improve derivations as not having to reprove proven theorems*/ )
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
    // TODO: merge with context? rename context? filter context for relevant rules?
    var validDerivations: Set[Derivation] = Set()
    // ATM this should store no objcts with vars as far as i can figure, this will be necessary
    // with hypothetical atd/or parameteric judgements but that is another day

    /* collect all derivability hypotheses along with their conclusions into a homogenous collection */
    def derivedJudgements = validDerivations.map(_.statement).map(p ⇒ p match {
      case Derivable(h, c) ⇒ h + c
      case _               ⇒ Set(p)
    }).flatten

    while (!validDerivations.exists(d ⇒ judgementStatement(d.statement) == judgementStatement(theorem))) {
      // accumulate the derivations that are found in the iteration instead of adding immediately
      // to the set of valid derivations so that ordering of rules does not affect search priority
      // (not doing so results in a deep search on the first few rules)
      var newDerivations: Set[Derivation] = Set()
      for (rule ← relevantRules()) {

        val variables = rule.statement.subjects.flatMap(_.vars) toSet // same as distinct, but maybe makes it faster?
        // get all objcts from the subjcts of derived judgements
        // when working with derivability judgements pull out all the subjects of the judgements in the judgement
        val objcts = validDerivations.map(d ⇒ if (isDerivabilityJudgement(d.statement)) d.statement.subjects.map(_.asInstanceOf[Judgement])
        else List(d.statement)
        ).flatten.flatMap(_.subjects)

        // for every variable, map to every object (that has been seen in validDerivations)
        // the cartesian product of variables and objcts gives all the possible combinations
        // then we construct every unique grouping of variables to create environments that map variables
        // uniquely and cover every object
        var varReplacements: Traversable[Traversable[(Var, Objct)]] = Traversable(Traversable())
        for (v ← cartesianProduct(variables, objcts)) {
          varReplacements = v.flatMap(vo ⇒ varReplacements.map(_ ++ Set(vo))) toSet
        } // varReplacements is now objects^variables large, luckily, most rules do not use /too/ many vars

        // can't work due to semi recursive/iterative nature of building unique var lists, hence above implementation
        //val varReplacements = cartesianProduct(variables, validDerivations.map(_.statement).flatMap(_.subjects)).map(_.flatMap(vo ⇒ varReplacements.map(_ ++ Set(vo))))

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
            // \/ should be equivalent to above ^^

            val premiseStatements = derivableJudgementStatements(premises) toSet
            val replacedPremises = environments.map(env ⇒ (env, premiseStatements.map(_.replaceVars(env))))
            replacedPremises.foreach { envPremises ⇒
              {
                // if the premises have already been derived
                if (envPremises._2.subsetOf(derivedJudgements)) {
                  newDerivations += Derivation(judgementStatement(conclusion).replaceVars(envPremises._1),
                    envPremises._2.map(p ⇒ validDerivations.find(d ⇒ p == judgementStatement(d.statement))
                      .get), rule)
                }
              }
            }
          }
        }
      }
      validDerivations ++= newDerivations
    }
    return validDerivations.find(d ⇒ judgementStatement(d.statement) == judgementStatement(theorem)).get
  }

  def isDerivabilityJudgement(j: Judgement): Boolean = j.symbol == "⊢"

  // TODO: many of these helper functions deal with the conversion between the (nested) judgment of
  // a statement of derivability and the flat structure of normal judgements. question: can we deal
  // with this by overriding the statement definition in derivable judgements (without subclassing)
  // much as we would like to do with printing out the hypothesis (especially when its an alphabet)
  // This following helper function essentially flattens the view of judgements allowing you to
  // treat derivable judgements as their consequents
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
