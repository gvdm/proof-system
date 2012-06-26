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

object State { def apply(state: Objct) = Judgement("state", List(state), PostFix) }
object Initial { def apply(state: Objct) = Judgement("initial", List(state), PostFix) }
object Final { def apply(state: Objct) = Judgement("final", List(state), PostFix) }
object Transition { def apply(s1: Objct, s2: Objct) = Judgement("→", List(s1,s2), InFix) }
object LabeleledTransition { def apply(s1: Objct, s2: Objct, label: Objct) = new Judgement("→", List(s1,s2,label), InFix) {
  override def toString() = subjects.head + " " + label+symbol + " " + subjects.tail.mkString(", ")
  }
}

object IteratedTransition { def apply(s1: Objct, s2: Objct) = Judgement("→*", List(s1,s2), InFix) }

