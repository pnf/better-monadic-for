package com.olegpy.bm4

import scala.tools.nsc
import nsc.Global
import nsc.plugins.PluginComponent
import nsc.transform.{Transform, TypingTransformers}
import scala.collection.mutable
import scala.reflect.internal.Flags
import scala.tools.nsc.typechecker.{Analyzer, Contexts, TreeCheckers}


class TupleLifter(plugin: BetterMonadicFor, val global: Global)
  extends PluginComponent with Transform with TypingTransformers {
  import global._

  // Can't be vals, because initialization occurs before plugin parameters are set.
  def verbose: Boolean = plugin.verboseTupleLifting
  def liftTuples = plugin.liftTuples

  override protected def newTransformer(unit: global.CompilationUnit): global.Transformer = new TupleLiftingTransformer(unit)

  val tupleLiftableClassName = "applicativish.TupleLiftable"
  val tupleLifterClassConstructor = rootMirror.getClassIfDefined(tupleLiftableClassName)

  override val phaseName: String = "bm4-tuple-lifter"
  override val runsAfter: List[String] = "typer" :: Nil


  class TupleLiftingTransformer(unit: CompilationUnit)
    extends TypingTransformer(unit) {

    override def transformUnit(unit: CompilationUnit) {
      if (liftTuples) {
        if (tupleLifterClassConstructor == NoSymbol)
          reporter.info(unit.body.pos, s"$tupleLiftableClassName trait unavailable; not lifting tuples", true)
        else {
          try {
            val ret = transform(unit.body)
            unit.body = ret
          } catch {
            case ex: Exception =>
              log(supplementErrorMessage("unhandled exception while lifting tuples in " + unit))
              throw ex
          }
        }
      }
    }


    private object PossiblyNestedMap {
      def unapply(tree: global.Tree): Option[global.Tree] = tree match {

        // Convert
        //   vm.flatMap { v => wm.mapOrFlatMap { w => expr } }
        // to
        //   tupleLift((v,w)).mapOrFlatMap { x$123 =>
        //     val v = x$123._1
        //     val w = x$123._2
        //     expr
        //   }
        case Apply(TypeApply(Select(vm, nme.flatMap), outerMethTypeParam),
                   Function(vValDef :: Nil,
                           PossiblyNestedMap(Apply(TypeApply(Select(wm, innerMeth), innerMethTypeParam),
                                                    Function(wValDef :: Nil, expr) :: Nil))) :: Nil)
          if (innerMeth == nme.flatMap || innerMeth == nme.map) &&
            vm.tpe.typeArgs.size == 1 && wm.tpe.typeArgs.size == 1 &&
            // Require that both are the same type of monad.
            (vm.tpe.typeConstructor == wm.tpe.typeConstructor) ⇒

          val ctxt = localTyper.context
          val mTpe: Type = vm.tpe.typeConstructor
          val tlc = tupleLifterClassConstructor
          val lifterType = appliedType(tlc, vm.tpe.typeConstructor :: Nil)
          val implSearch = analyzer.inferImplicitByTypeSilent(lifterType, ctxt, tree.pos)

          if (implSearch.isFailure) {
            if (verbose)
              reporter.info(tree.pos, s"Failed to find implicit $lifterType", true)
            Some(TupleLiftingTransformer.super.transform(tree))
          }

          else {
            // Crucial part: ensure that v is never used as the qualifier
            val vUsed = wm.find(vValDef.symbol == _.symbol)
            if(vUsed.isDefined) {
              if(verbose)
                reporter.info(vUsed.get.pos, s"Not lifting, because ${vValDef.symbol} is used on rhs", true)
              Some(TupleLiftingTransformer.super.transform(tree))
            }

            else {
              val tupleLifterObj = implSearch.tree

              reporter.info(tree.pos, s"Found implicit $lifterType => ${tupleLifterObj.symbol}", true)

              val transformedExpr = transform(expr)

              val vt: global.Type = vm.tpe.typeArgs.head
              val wt: global.Type = wm.tpe.typeArgs.head

              // tupleLift[V,W]((wm,vm))
              val newQual = q"$tupleLifterObj.tupleLift[$vt,$wt](($vm,$wm))"

              // Parameter of type (V,W) for new closure
              val tupleOfXYtt: global.Tree = tq"($vt,$wt)"
              val vwArgName = global.internal.reificationSupport.freshTermName("x$")
              val vwArgDef = ValDef(Modifiers(Flags.SYNTHETIC | Flags.PARAM), vwArgName, tupleOfXYtt, EmptyTree)
              // rhs of new closure, extracting v and w from tuple.
              // Note: at this point, the symbols for v and w are still owned by original closures.  We'll change
              // ownership below, after typing.
              val newExpr = Block(
                ValDef(vValDef.symbol, q"$vwArgName._1"),
                ValDef(wValDef.symbol, q"$vwArgName._2"),
                transformedExpr)

              val newClosure = Function(vwArgDef :: Nil, newExpr)

              // Assemble and type new combinator application:
              val ret = Apply(TypeApply(Select(newQual, innerMeth), innerMethTypeParam), newClosure :: Nil)
              val rett = localTyper.typedPos(tree.pos)(ret)
              newExpr.changeOwner((vValDef.symbol.owner, newClosure.symbol), (wValDef.symbol.owner, newClosure.symbol))

              Some(rett)
            }
          }

        // Boring map.  Not nested.
        case Apply(TypeApply(Select(wm, comb), _),
                   Function(_ :: Nil, _) :: Nil) ⇒
          Some(TupleLiftingTransformer.super.transform(tree))

        case t ⇒
          None


      }

    }

    override def transform(tree: Tree): Tree = tree match {
      case PossiblyNestedMap(xformed) ⇒ xformed

      case _ ⇒
        super.transform(tree)
    }

  }

}


