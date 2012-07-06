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

object IncorrectJudgemntObjct extends Throwable
object VariableUniquenessException /*(v:String, o1:String, o2:String)*/ extends Throwable

abstract class ObjctDef {
  def definition: Set[Rule]
  def rules: Set[Rule]
}

abstract class Objct {
  type EnvMap = collection.immutable.Map[Var, Objct]
  
  // matches variables in this object construction against an object with the same construction 
  def matchVarObj(e: EnvMap, o: Objct): EnvMap = { return e }
    //if (o != this) throw new Error("Objct constructions did not match")

  // returns all (unique) variables in object constructions
  def vars: Set[Var] = Set()

  // replaces all var objs that the env maps to their objs in env
  // TODO: is this still the neatest way to do it given the case class copy method?
  def replaceVars(e: EnvMap) = this
}

case class Var(name: String) extends Objct {
  override def matchVarObj(e: EnvMap, o: Objct) =
    if (e.contains(this) && e(this) != o) throw VariableUniquenessException
    else e + (this -> o)
  override def vars = Set(this)
  override def replaceVars(e: EnvMap): Objct = e(this)
  override def toString = "$"+name
}

case class Val(value: Any) extends Objct {
  override def toString = value.toString
}
