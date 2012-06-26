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

object makeRuleLine {
  def apply(length: Int): String = List.fill(length)("-").mkString("")
}
object maxWidthString {
  def apply(s: String): Int = s.lines.map(_.length).max
}

// should strictly speaking be .flatten ed, but it suits us not to
object cartesianProduct {
  def apply[X, Y](a: Traversable[X], b: Traversable[Y]) = a.map(x => b.map(y => (x, y)))
}
