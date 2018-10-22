package applicativish

import annotation.unchecked._
import scala.annotation.Annotation
import scala.language.higherKinds
import scala.languageFeature.implicitConversions


trait TupleLiftable[+M[_]] {
  def tupleLift[A,B](t: (M[A],M[B]) @uncheckedVariance): M[(A,B)] @uncheckedVariance
}


object TupleLiftable {

  import scala.language.experimental.macros
  import scala.reflect.macros.blackbox.Context


  def liftTuples[T, M[_]](expr: M[T])(implicit lifter: TupleLiftable[M]): M[T] = macro liftTuplesImpl[T, M]

  def liftTuplesImpl[T: c.WeakTypeTag, M[_]](c: Context)(expr: c.Expr[M[T]])(lifter: c.Expr[TupleLiftable[M]])(implicit M: c.WeakTypeTag[M[_]]) = {
    import c.universe._

    val flatMapName = TermName("flatMap")
    val mapName = TermName("map")
    val tupleLiftableClassName = "applicativish.TupleLiftable"
    val tupleLifterClassConstructor = rootMirror.staticClass(tupleLiftableClassName)

    class TupleLiftingTransformer extends Transformer {

      private object PossiblyNestedMap {
        def unapply(tree: Tree): Option[Tree] = tree match {

          // Convert
          //   vm.flatMap { v => wm.mapOrFlatMap { w => expr } }
          // to
          //   tupleLift((v,w)).mapOrFlatMap { x$123 =>
          //     val v = x$123._1
          //     val w = x$123._2
          //     expr
          //   }
          case Apply(TypeApply(Select(vm, TermName("flatMap")), outerMethTypeParam),
          Function(vValDef :: Nil,
          PossiblyNestedMap(Apply(TypeApply(Select(wm, innerMeth), innerMethTypeParam),
          Function(wValDef :: Nil, expr) :: Nil))) :: Nil)
            if (innerMeth == flatMapName || innerMeth == mapName) &&
              vm.tpe.typeArgs.size == 1 && wm.tpe.typeArgs.size == 1 &&
              // Require that both are the same type of monad.
              (vm.tpe.typeConstructor == wm.tpe.typeConstructor) ⇒


            val ctxt = c
            val mTpe: Type = vm.tpe.typeConstructor
            val tlc = tupleLifterClassConstructor
            val lifterType = appliedType(tlc, vm.tpe.typeConstructor :: Nil)

            val tupleLifterObj = lifter.tree // c.inferImplicitValue(lifterType)

            if (tupleLifterObj.isEmpty) {
              c.info(tree.pos, s"Failed to find implicit $lifterType", true)
              Some(TupleLiftingTransformer.super.transform(tree))
            }

            else {
              // Crucial part: ensure that v is never used as the qualifier
              val vUsed = wm.find(vValDef.symbol == _.symbol)
              if (vUsed.isDefined) {
                c.info(vUsed.get.pos, s"Not lifting, because ${vValDef.symbol} is used on rhs", true)
                Some(TupleLiftingTransformer.super.transform(tree))
              }

              else {

                c.info(tree.pos, s"Found implicit $lifterType => ${tupleLifterObj.symbol}", true)

                val transformedExpr = transform(expr)

                val vt: Type = vm.tpe.typeArgs.head
                val wt: Type = wm.tpe.typeArgs.head

                // tupleLift[V,W]((wm,vm))
                val newQual = q"$tupleLifterObj.tupleLift[$vt,$wt](($vm,$wm))"

                // Parameter of type (V,W) for new closure
                val tupleOfXYtt: Tree = tq"($vt,$wt)"
                val vwArgName = internal.reificationSupport.freshTermName("x$")

                val vwArgDef = ValDef(Modifiers(Flag.SYNTHETIC | Flag.PARAM), vwArgName, tupleOfXYtt, EmptyTree)


                // rhs of new closure, extracting v and w from tuple.
                // Note: at this point, the symbols for v and w are still owned by original closures.  We'll change
                // ownership below, after typing.
                val newExpr = Block(
                  c.internal.valDef(vValDef.symbol, q"$vwArgName._1"),
                  c.internal.valDef(wValDef.symbol, q"$vwArgName._2"),
                  transformedExpr)

                val newClosure = c.typecheck(Function(vwArgDef :: Nil, newExpr))
                c.internal.changeOwner(newExpr, vValDef.symbol.owner, newClosure.symbol)
                c.internal.changeOwner(newExpr, wValDef.symbol.owner, newClosure.symbol)

                // Assemble and type new combinator application:
                val ret = Apply(TypeApply(Select(newQual, innerMeth), innerMethTypeParam), newClosure :: Nil)
                val rett = c.typecheck(ret)
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

    val ret =  (new TupleLiftingTransformer).transform(expr.tree)
    ret

  }

}