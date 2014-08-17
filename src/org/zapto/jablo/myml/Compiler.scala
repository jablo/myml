
package org.zapto.jablo.myml

object Compiler extends ExHelper {
  def compile(e: Ex): List[ByteCode] = e match {
    case ErrorEx(n)          => List(Push(Str(n)), ErrIns)
    case Var(n)              => List(Push(n), Lookup)
    case Par(e1)             => compile(e)
    case Un(e1, op)          => compile(e1) :+ op
    case Bin(e1, e2, op)     => compile(e1) ++ compile(e2) :+ op
    case Tri(e1, e2, e3, op) => compile(e1) ++ compile(e2) ++ compile(e3) :+ op
    case Ife(e1, e2, e3)     => List(Push(Code(compile(e2))), Push(Code(compile(e3)))) ++ compile(e1) :+ Cond
    case Fun(fargs, body)    => List(Push(Subr(fargs, compile(body), BCNilEnv)), MakeClosure)
    case App(fexp, args) =>
      val argscode: List[ByteCode] = (args.map(compile)).flatten
      val fcode = compile(fexp)
      argscode ++ fcode :+ Call
    case LetR(fargs, args, body) =>
      val argscode: List[ByteCode] = (args.map(compile)).flatten
      val asgncode = fargs.reverse.map((n) => List(Push(n), Assign)).flatten
      RecEnv :: argscode ++ asgncode ++ compile(body)
    case Def(n, e)  => compile(e) ++ List(Push(n), Assign)
    case Undef(n)   => List(Push(ReplUnDef(n)))
    case Load(n)    => List(Push(ReplLoad(n)))
    case ReLoad()   => List(Push(ReplReLoad()))
    case Compile(e) => List(Push(Code(compile(e))))
    case Comment(_) => List()
    case c: Const   => List(Push(c))
  }
}