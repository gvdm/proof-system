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

abstract class Objct {
  type EnvMap = collection.immutable.Map[Var, Objct]
  
  // matches variables in this object construction against an object with similar construction 
  def matchVarObj(e: EnvMap, o: Objct): EnvMap = {
    //if (o != this) throw new Error("Objct constructions did not match")
    return e
  }

  // returns all (unique) variables in object constructions
  def vars: Set[Var] = Set()

  // replaces all var objs that the env maps to their objs in env
  // TODO: is this still the neatest way to do it given the case class copy method?
  def replaceVars(e: EnvMap) = this
}

case class Var(name: String) extends Objct {
  override def matchVarObj(e: EnvMap, o: Objct) = e + (this -> o) // should match one var to another
  override def vars = Set(this)
  override def replaceVars(e: EnvMap): Objct = e(this)
}

case class Val(value: String) extends Objct

//sealed trait Natural extends Value
case object Zero extends Objct
case class Succ(pre: Objct) extends Objct {
  override def matchVarObj(e: EnvMap, o: Objct): EnvMap = {
    o match {
      case Succ(n) ⇒ pre.matchVarObj(e, n)
      case _       ⇒ throw IncorrectJudgemntObjct
    }
  }
  override def vars = pre.vars
  override def replaceVars(e: EnvMap): Objct = Succ(pre.replaceVars(e))
}

//sealed trait Tree extends Value
case object Leaf extends Objct
case class Branch(left: Objct, right: Objct) extends Objct {
  override def matchVarObj(e: EnvMap, o: Objct) = {
    o match {
      case Branch(l, r) ⇒ e ++ left.matchVarObj(e, l) ++ (right.matchVarObj(e, r))
      case _            ⇒ throw IncorrectJudgemntObjct
    }
  }
  override def vars = left.vars ++ right.vars
  override def replaceVars(e: EnvMap): Objct = Branch(left.replaceVars(e), right.replaceVars(e))
}

//sealed trait Lst extends Value
case object Nl extends Objct
case class Cons(head: Objct, tail: Objct) extends Objct {
  override def matchVarObj(e: EnvMap, o: Objct): EnvMap = {
    o match {
      case Cons(h, t) ⇒ e ++ head.matchVarObj(e, h) ++ (tail.matchVarObj(e, t))
      case _            ⇒ throw IncorrectJudgemntObjct
    }
  }
  override def vars = head.vars ++ tail.vars
  override def replaceVars(e: EnvMap): Objct = Cons(head.replaceVars(e), tail.replaceVars(e))
}
