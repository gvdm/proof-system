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

// TODO: create test system
object ProofExplorationNegotiationandInspectionSystem {

  def main(args: Array[String]) {
    valid ::: derivable foreach { theorem ⇒
      {
        println("Deriving: " + theorem)
        val d = new Derive(theorem)
        val b = d.backward
        val f = d.forward
        println(b)
        println
        assert(b == f)
      }
    }
  }

  val derivable = List(Derivable(Nat(Val("junk")), Nat(Succ(Succ(Val("junk"))))))
  
  val valid = List(
    Nat(Succ(Succ(Succ(Zero)))),
    Tr(Branch(Branch(Leaf, Leaf), Leaf)),
    Sum(Zero,Zero,Zero),
    Sum(Succ(Zero), Succ(Succ(Zero)), Succ(Succ(Succ(Zero)))),
    Sum(Succ(Zero), Zero, Succ(Zero)),
    Judgement("even", List(Succ(Succ(Zero)))),
    Judgement("odd", List(Succ(Succ(Succ(Zero))))),
    Judgement("max", List(Succ(Zero), Succ(Succ(Zero)), Succ(Succ(Zero)))),
    Judgement("max", List(Succ(Succ(Succ(Zero))), Succ(Succ(Zero)), Succ(Succ(Succ(Zero)))))
  )

  val invalid = List(
    Nat(Succ(Leaf)),
    Tr(Succ(Zero)),
    Nat(Branch(Leaf, Leaf)),
    Tr(Branch(Zero, Leaf)),
    Judgement("even", List(Succ(Zero))),
    Judgement("odd", List(Succ(Succ(Zero))))
  )
}
