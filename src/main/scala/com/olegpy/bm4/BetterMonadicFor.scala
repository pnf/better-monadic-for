package com.olegpy.bm4

import scala.tools.nsc
import nsc.Global
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import nsc.transform.{Transform, TypingTransformers}
import scala.reflect.internal.{Definitions, Flags}
import scala.tools.nsc.typechecker.{Analyzer, Implicits}


class BetterMonadicFor(val global: Global) extends Plugin {
  val name = "bm4"
  val description = "Remove withFilter / partial matches in for-comprehension"
  val components =
    new ForRewriter(this, global) ::
      new MapRemover(this, global) ::
      new TupleRemover(this, global) ::
      new ApplicativeSimulator(this, global) ::
      Nil

  var noUncheckedFilter = true
  var noMapIdentity     = true
  var noTupling         = true

  val knobs = Map(
    "no-filtering" -> "Remove .withFilter from generator desugaring",
    "no-map-id"    -> "Optimize .map(x => x) and .map(_ => ())",
    "no-tupling"   -> "Not implemented yet"
  )


  override val optionsHelp: Option[String] = Some(
    knobs
      .map { case (key, help) =>
        s"  -P:$name:$key:(y/n)".padTo(31, ' ') ++ help
      }
      .mkString(System.lineSeparator)
  )

  override def init(options: List[String], error: String => Unit): Boolean = {
    val (known, unknown) = options.partition(s => knobs.keys.exists(s.startsWith))
    if (unknown.nonEmpty) {
      error(s"Unknown options: ${unknown.mkString(", ")}")
      return false
    }

    val toBoolean = (txt: String) => txt.toLowerCase match {
      case "y" | "yes" | "1" | "true"  => true
      case "n" | "no"  | "0" | "false" => false
      case _ =>
        error(s"Unknown boolean value $txt")
        return false
    }

    for {
      key <- known
      _ = if (!key.contains(':')) {
        error(s"Option $key does not include the parameter (e.g. $key:y)")
        return false
      }
      Array(prefix, value) = key.split(":", 2)
    } prefix match {
      case "no-filtering" => noUncheckedFilter = toBoolean(value)
      case "no-map-id"    => noMapIdentity     = toBoolean(value)
      case "no-tupling"   => noTupling         = toBoolean(value)
    }

    noUncheckedFilter || noMapIdentity || noTupling
  }
}

class ForRewriter(plugin: BetterMonadicFor, val global: Global)
  extends PluginComponent with Transform with TypingTransformers
    with NoUncheckedFilter
{

  import global._

  override val noUncheckedFilter: Boolean = plugin.noUncheckedFilter

  val runsAfter = "parser" :: Nil
  override val runsRightAfter = Some("parser")
  override val runsBefore = "namer" :: Nil
  val phaseName = "bm4-parser"

  def newTransformer(unit: CompilationUnit): Transformer = new MyTransformer(unit)

  class MyTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    // The magic happens in `unapply` of objects defined in mixed in traits
    override def transform(tree: Tree): Tree = tree match {
      case NoUncheckedFilter(cleaned) =>
        transform(cleaned)
      case _ =>
        super.transform(tree)
    }
  }
}

class MapRemover(plugin: BetterMonadicFor, val global: Global)
   extends PluginComponent with Transform with TypingTransformers
     with NoMapIdentity
{
  import global._


  protected def newTransformer(unit: global.CompilationUnit) =
    new MapIdentityRemoveTransformer(unit)

  def noMapIdentity = plugin.noMapIdentity

  val phaseName = "bm4-typer"
  val runsAfter = "typer" :: Nil


  override val runsBefore: List[String] = "patmat" :: Nil

  class MapIdentityRemoveTransformer(unit: CompilationUnit)
    extends TypingTransformer(unit)
  {
    override def transform(tree: Tree): Tree = {
      tree match {
        case NoMapIdentity(cleaned) =>
          transform(cleaned)
        case _ =>
          super.transform(tree)
      }
    }
  }
}

class TupleRemover(plugin: BetterMonadicFor, val global: Global)
  extends PluginComponent with Transform with TypingTransformers
  with NoTupleBinding {
  import global._
  protected def newTransformer(unit: global.CompilationUnit): Transformer =
    new TupleRemoveTransformer(unit)

  def noTupling: Boolean = plugin.noTupling
  val phaseName: String = "bm4-parser2"
  val runsAfter: List[String] = "parser" :: "bm4-parser" :: Nil
  override val runsRightAfter: Option[String] = Some("bm4-parser")
  override val runsBefore: List[String] = "patmat" :: Nil

  class TupleRemoveTransformer(unit: CompilationUnit)
    extends TypingTransformer(unit)
  {
    override def transform(tree: Tree): Tree = tree match {
      case NoTupleBinding(cleaned) =>
        transform(cleaned)
      case _ =>
        super.transform(tree)
    }
  }
}

class ApplicativeSimulator(plugin: BetterMonadicFor, val global: Global)
  extends PluginComponent with Transform with TypingTransformers  {
  import global._

  val tupleLifterClassConstructor = rootMirror.getRequiredClass("applicativish.TupleLifter")

  override protected def newTransformer(unit: global.CompilationUnit): global.Transformer = new ApplicatizingTransformer(unit)

  val tupleClass = rootMirror.requiredClass[Tuple2[_,_]].tpe

  override val phaseName: String = "bm4-applicatizer"
  override val runsAfter: List[String] = "typer" :: Nil

  class ApplicatizingTransformer(unit: CompilationUnit)
    extends TypingTransformer(unit)  {

    override def transform(tree: Tree): Tree = tree match {

      case Apply(ta1@TypeApply(fm1@Select(vm, nme.flatMap), fmType1 :: Nil),
      (Function(vValDef :: Nil, Apply(TypeApply(fm2@Select(wm, nme.flatMap), fmType2 :: Nil), Function(wValDef :: Nil, expr_u) :: Nil))) :: Nil)
        if vm.tpe.typeArgs.size == 1 && wm.tpe.typeArgs.size == 1 &&
          (vm.tpe.typeConstructor == wm.tpe.typeConstructor) &&
          !wm.exists(vValDef.symbol == _.symbol) ⇒ {

        val ctxt = localTyper.context

        val mTpe: Type = vm.tpe.typeConstructor

        val tt = tq"$tupleLifterClassConstructor[$mTpe]"

        val tlc = tupleLifterClassConstructor

        val lifterType = appliedType(tlc, vm.tpe.typeConstructor :: Nil)

        val a = analyzer  // keep local for debugging

        localTyper.context.owner.info

        val ctx = localTyper.context.asInstanceOf[a.Context]


        val implSearch =  a.inferImplicitByTypeSilent(lifterType, ctx, tree.pos)

        reporter.info(tree.pos, s"implied search for $lifterType => $implSearch", true)

        if(implSearch.isFailure)
          super.transform(tree)
        else {

          val  tupleLifterObj = implSearch.tree

          val expr = transform(expr_u)


          val vt: global.Type = vm.tpe.typeArgs.head
          val wt: global.Type = wm.tpe.typeArgs.head

          /*
          // Look for a way to convert (M[X],M[Y]) to M[(X,Y]]
          val tupleLifterType = appliedType(tupleLifterClassConstructor, wm.tpe.typeConstructor)
          val ctxt = localTyper.context.asInstanceOf[Context]
          val tupleLifterSearch = inferImplicitFor(tupleLifterType, EmptyTree, ctxt, true)
          if (tupleLifterSearch.isFailure)
            reporter.info(vm.pos, "Can't find implicit tuple lifter", true)
          val tupleLifterObj = tupleLifterSearch.tree
          */

          // (M[X], M[Y]]
          val tupleOfMs: Type = TypeRef(NoPrefix, typeOf[Tuple2[_, _]].typeSymbol, vm.tpe :: wm.tpe :: Nil)
          val tupleOfXY: Type = TypeRef(NoPrefix, typeOf[Tuple2[_, _]].typeSymbol, vt :: wt :: Nil)
          val mOfTuple: Type = TypeRef(NoPrefix, vm.tpe.typeConstructor.typeSymbol, tupleOfXY :: Nil)
          val tupleOfXYtt: global.Tree = tq"($vt,$wt)"


          val vwArgName = global.internal.reificationSupport.freshTermName("x$")
          val vwArgDef = ValDef(Modifiers(Flags.SYNTHETIC | Flags.PARAM), vwArgName, tupleOfXYtt, EmptyTree)
          // vwArgSym.setInfo(tupleOfXY)
          val newExpr = Block(
            ValDef(vValDef.symbol, q"$vwArgName._1"), // at this point, they still have the wrong owners
            ValDef(wValDef.symbol, q"$vwArgName._2"),
            expr)

          val newQual = q"$tupleLifterObj.tupleLift[$vt,$wt](($vm,$wm))"
          // newQual.setType(mOfTuple)

          val f3 = Function(vwArgDef :: Nil, newExpr)

          val ret = Apply(TypeApply(Select(newQual, nme.flatMap), fmType2 :: Nil), f3 :: Nil)
          val rett = localTyper.typedPos(tree.pos)(ret)
          newExpr.changeOwner((vValDef.symbol.owner, f3.symbol), (wValDef.symbol.owner, f3.symbol))

          rett
        }
      }

      case _ ⇒ super.transform(tree)

    }

  }

}
