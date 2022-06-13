package microc.middleend.solver

import microc.frontend.util.UnionFind
import microc.frontend.util.logger.Logger

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * A generic term. It can be either
  *   - a variable [[Var]],
  *   - a constructor [[Constructor]], or
  *   - a recursive term [[Mu]].
  *
  * @tparam A the kind of constraint system
  */
sealed trait Term[A] {

  /** Returns the set of free variables in this term. */
  def freeVariables: Set[Var[A]]

  /** Produces a new term from this term by substituting the variable `v` with the term `t`. */
  def subst(v: Var[A], t: Term[A]): Term[A]
}

/**
  * A constraint variable.
  */
trait Var[A] extends Term[A] {

  val freeVariables: Set[Var[A]] = Set(this)

  def subst(v: Var[A], t: Term[A]): Term[A] =
    if (v == this) t else this
}

/**
  * An n-ary term constructor.
  *
  * 0-ary constructors are constants.
  */
trait Constructor[A] extends Term[A] {

  /** The sub-terms. */
  val args: List[Term[A]]

  /** The arity of the constructor. */
  def arity: Int = args.length

  lazy val freeVariables: Set[Var[A]] = args.flatMap(_.freeVariables).toSet

  /**
    * Checks whether the term `t` matches this term, meaning that it has the same constructor class and the same arity.
    */
  def matches(t: Term[A]): Boolean =
    this.getClass == t.getClass && arity == t.asInstanceOf[Constructor[A]].arity

  def unboundedVariables: Set[Var[A]] = {
    @tailrec def visit(rem: Set[Term[A]], bounded: Set[Var[A]], free: Set[Var[A]]): Set[Var[A]] =
      if (rem.isEmpty) free
      else {
        rem.head match {
          case x: Var[A] if !bounded(x) => visit(rem.tail, bounded, free + x)
          case _: Var[A]                => visit(rem.tail, bounded, free)
          case x: Constructor[A]        => visit(rem.tail ++ x.args, bounded, free)
          case x: Mu[A]                 => visit(rem.tail + x.term, bounded + x.variable, free)
        }
      }

    visit(Set(this), Set(), Set())
  }
}

/**
  * Recursive term.
  * Whenever a term is such that v = t[v] where v appears free in t[v], then we represent it finitely as µv.t[v].
  * v is a binder in the term, and the copy rule holds: µv.t[v] == t [ µv.t[v] ]
  */
trait Mu[A] extends Term[A] {

  /**
    * The variable (v).
    */
  val variable: Var[A]

  /**
    * The term (t).
    */
  val term: Term[A]

  def freeVariables: Set[Var[A]] = term.freeVariables - variable

  override def toString: String = s"µ$variable.$term"
}

case class UnificationFailure[A](message: String, left: A, right: A) extends RuntimeException(message)

class UnificationSolver[A](log: Logger) {
  type Solution = Map[Var[A], Term[A]]

  private val unionFind = new UnionFind[Term[A]]()

  import unionFind._

  def unify(t1: Term[A], t2: Term[A]): Term[A] = {
    makeSet(t1)
    makeSet(t2)

    val r1 = find(t1)
    val r2 = find(t2)

    log.debug(s"unifying terms: $t1${if (t1 == r1) "" else s" (canonical $r1)"} and $t2${if (t2 == r2) ""
    else s" (canonical $r2)"}")

    (r1, r2) match {
      case _ if r1 == r2 =>
        log.debug(s"$r1 == $r2")
        r1
      case (v1: Var[A], v2: Var[A]) =>
        log.debug(s"$v1 ----> $v2")
        union(v1, v2)

      case (v1: Var[A], t2: Term[A]) =>
        log.debug(s"$v1 ----> $t2")
        union(v1, t2)

      case (t1: Term[A], v2: Var[A]) =>
        log.debug(s"$v2 ----> $t1")
        union(v2, t1)

      case (c1: Constructor[A], c2: Constructor[A]) if c1.matches(c2) =>
        log.debug(s"$c1 ----> $c2")
        val r = union(c1, c2)

        c1.args.zip(c2.args).foreach {
          case (x1, x2) =>
            log.debug(s"unifying sub-term $x1 (from $c1) with $x2 (from $c2)")
            unify(x1, x2)
        }

        r
      case (x, y) =>
        throw UnificationFailure(s"Cannot unify '$t1' and '$t2' (with canonicals '$x' and '$y')", x, y)
    }
  }

  def solution(): Solution = {
    // for each constraint variable, find its canonical representative (using the variable itself as default)
    graph.keys.collect { case v: Var[A] => (v, find(v)) }.toMap
  }
}

abstract class TermClosingSolution[A](unclosed: UnificationSolver[A]#Solution) {
  // memoization for fully closed terms
  private val typeMap = {
    val tmp = mutable.Map[Var[A], Term[A]]()
    tmp ++= unclosed
    tmp
  }

  private val freshVars = mutable.Map[Var[A], Var[A]]()

  def close(term: Term[A]): Term[A] = {
    def close0(t: Term[A], visited: Set[Term[A]]): Term[A] = {
      t match {
        case v: Var[A] =>
          val closed = typeMap.get(v) match {
            case Some(w) if w != v && !visited.contains(v) =>
              // no recursion found, and the variable does not map to itself
              val cterm = close0(w, visited + v)
              freshVars.get(v) match {
                case Some(fv) if cterm.freeVariables.contains(fv) =>
                  // recursive term found, make a [[Mu]]
                  makeMu(fv, cterm.subst(v, fv))
                case _ =>
                  cterm
              }
            case _ =>
              // recursive or unconstrained term variables, make a fresh term variable
              freshVars.getOrElseUpdate(v, makeFreshVar())
          }

          if (closed.freeVariables.isEmpty) {
            // we managed to resolve all free variables
            // the term is fully closed so we can memoize it
            typeMap += v -> closed
          }

          closed
        case m: Mu[A] =>
          // it is already a recursive type
          m

        case c: Constructor[A] =>
          c.freeVariables.foldLeft(t) { (acc, v) =>
            acc.subst(v, close0(v, visited))
          }
      }
    }

    close0(term, Set())
  }

  def makeMu(v: Var[A], t: Term[A]): Mu[A]

  def makeFreshVar(): Var[A]
}
