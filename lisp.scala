import scala.io.Source

val kLPar = '('
val kRPar = ')'
val kQuote = '\''

abstract class LObj

class Nil0 {
}

class Num0(n: Int) {
  val data = n
}

class Sym0(s: String) {
  val data = s
}

class Error0(s: String) {
  val data = s
}

class Cons0(a: LObj, d: LObj) {
  var car = a
  var cdr = d
}

class Subr0(f: LObj => LObj) {
  val fn = f
}

class Expr0(a: LObj, b: LObj, e: LObj) {
  val args = a
  val body = b
  val env = e
}

case class Nil(obj: Nil0) extends LObj
val kNil = Nil(new Nil0)

case class Num(obj: Num0) extends LObj
def makeNum(n: Int) = Num(new Num0(n))

case class Sym(obj: Sym0) extends LObj
val sym_table = collection.mutable.Map[String, LObj]()
def makeSym(s: String) = {
  sym_table.get(s) match {
    case Some(v) => v
    case None => {
      val sym = Sym(new Sym0(s))
      sym_table += s -> sym
      sym
    }
  }
}

val sym_t = makeSym("t")
val sym_quote = makeSym("quote")
val sym_if = makeSym("if")
val sym_lambda = makeSym("lambda")
val sym_defun = makeSym("defun")
val sym_setq = makeSym("setq")

case class Error(obj: Error0) extends LObj
def makeError(s: String) = Error(new Error0(s))

case class Cons(obj: Cons0) extends LObj
def makeCons(a: LObj, d: LObj) = Cons(new Cons0(a, d))

def safeCar(obj: LObj) = {
  obj match {
    case Cons(c) => c.car
    case _ => kNil
  }
}

def safeCdr(obj: LObj) = {
  obj match {
    case Cons(c) => c.cdr
    case _ => kNil
  }
}

case class Subr(obj: Subr0) extends LObj
def makeSubr(f: LObj => LObj) = Subr(new Subr0(f))

case class Expr(obj: Expr0) extends LObj
def makeExpr(a: LObj, e: LObj) = Expr(new Expr0(safeCar(a), safeCdr(a), e))

def nreverse(lst: LObj) = {
  def doit(lst: LObj, ret: LObj): LObj = {
    lst match {
      case Cons(c) => {
        val tmp = c.cdr
        c.cdr = ret
        doit(tmp, lst)
      }
      case _ => ret
    }
  }
  doit(lst, kNil)
}

def pairlis(lst1: LObj, lst2: LObj) = {
  def doit(lst1: LObj, lst2: LObj, ret: LObj): LObj = {
    (lst1, lst2) match {
      case (Cons(c1), Cons(c2)) =>
        doit(c1.cdr, c2.cdr, makeCons(makeCons(c1.car, c2.car), ret))
      case _ => nreverse(ret)
    }
  }
  doit(lst1, lst2, kNil)
}

def isDelimiter(c: Char) = {
  c == kLPar || c == kRPar || c == kQuote || c.isWhitespace
}

def skipSpaces(s: String) = {
  var i = 0
  while (i < s.length && s(i).isWhitespace) {
    i += 1
  }
  s.substring(i)
}

def makeNumOrSym(s: String) = {
  try {
    makeNum(s.toInt)
  } catch {
    case e: Exception => makeSym(s)
  }
}

def readAtom(s: String): (LObj, String) = {
  var i = 0
  while (i < s.length && !isDelimiter(s(i))) {
    i += 1
  }
  (makeNumOrSym(s.substring(0, i)), s.substring(i))
}

def parseError(s: String) = (makeError(s), "")

def read(s: String): (LObj, String) = {
  val str = skipSpaces(s)
  if (str.length == 0)
    parseError("empty input")
  else if (str(0) == kRPar)
    parseError("invalid syntax: " + str)
  else if (str(0) == kLPar)
    readList(str.substring(1))
  else if (str(0) == kQuote) {
    val (elm, next) = read(str.substring(1))
    (makeCons(sym_quote, makeCons(elm, kNil)), next)
  }
  else
    readAtom(str)
}

def readList(s: String) = {
  def doit(s: String, ret: LObj): (LObj, String) = {
    if (s.length == 0)
      parseError("unfinished parenthesis")
    else if (s(0) == kRPar)
      (nreverse(ret), s.substring(1))
    else {
      val (elm, next) = read(s)
      elm match {
        case Error(_) => (elm, next)
        case _ => doit(skipSpaces(next), makeCons(elm, ret))
      }
    }
  }
  doit(skipSpaces(s), kNil)
}

def printObj(obj: LObj): String = {
  obj match {
    case Nil(_) => "nil"
    case Num(n) => n.data.toString
    case Sym(s) => s.data
    case Error(e) => "<error: " + e.data + ">"
    case Cons(_) => printList(obj)
    case Subr(_) => "<subr>"
    case Expr(_) => "<expr>"
  }
}

def printList(obj: LObj): String = {
  def doit(obj: LObj, first: Boolean, ret: String): String = {
    obj match {
      case Cons(c) =>
        doit(c.cdr, false, ret + (if (first) "" else " ") + printObj(c.car))
      case Nil(_) => "(" + ret + ")"
      case _ => "(" + ret + " . " + printObj(obj) + ")"
    }
  }
  doit(obj, true, "")
}

def findVar(sym: LObj, env: LObj): LObj = {
  def doit(alist: LObj): LObj = {
    alist match {
      case Cons(c) =>
        if (safeCar(c.car) == sym)
          c.car
        else
          doit(c.cdr)
      case _ => kNil
    }
  }
  env match {
    case Cons(c) => {
      val x = doit(c.car)
      if (x == kNil)
        findVar(sym, c.cdr)
      else
        x
    }
    case _ => kNil
  }
}

val g_env = makeCons(kNil, kNil)

def addToEnv(sym: LObj, value: LObj, env: LObj) {
  env match {
    case Cons(c) => c.car = makeCons(makeCons(sym, value), c.car)
  }
}

def eval(obj: LObj, env: LObj): LObj = {
  obj match {
    case Nil(_) => obj
    case Num(_) => obj
    case Error(_) => obj
    case Sym(s) => {
      val bind = findVar(obj, env)
      bind match {
        case Cons(c) => c.cdr
        case _ => makeError(s.data + " has no value")
      }
    }
    case _ => {
      val op = safeCar(obj)
      val args = safeCdr(obj)
      if (op == sym_quote)
        safeCar(args)
      else if (op == sym_if) {
        val c = eval(safeCar(args), env)
        c match {
          case Error(_) => c
          case Nil(_) => eval(safeCar(safeCdr(safeCdr(args))), env)
          case _ => eval(safeCar(safeCdr(args)), env)
        }
      }
      else if (op == sym_lambda)
        makeExpr(args, env)
      else if (op == sym_defun) {
        val expr = makeExpr(safeCdr(args), env)
        val sym = safeCar(args)
        addToEnv(sym, expr, g_env)
        sym
      }
      else if (op == sym_setq) {
        val value = eval(safeCar(safeCdr(args)), env)
        val sym = safeCar(args)
        findVar(sym, env) match {
          case Cons(c) => c.cdr = value
          case _ => addToEnv(sym, value, g_env)
        }
        value
      }
      else
        apply(eval(op, env), evlis(args, env), env)
    }
  }
}

def evlis(lst: LObj, env: LObj) = {
  def doit(lst: LObj, ret: LObj): LObj = {
    lst match {
      case Cons(c) => {
        eval(c.car, env) match {
          case Error(e) => Error(e)
          case elm => doit(c.cdr, makeCons(elm, ret))
        }
      }
      case _ => nreverse(ret)
    }
  }
  doit(lst, kNil)
}

def progn(body: LObj, env: LObj) = {
  def doit(body: LObj, ret: LObj): LObj = {
    body match {
      case Cons(c) => doit(c.cdr, eval(c.car, env))
      case _ => ret
    }
  }
  doit(body, kNil)
}

def apply(fn: LObj, args: LObj, env: LObj) = {
  args match {
    case Error(_) => args
    case _ => {
      fn match {
        case Error(_) => fn
        case Subr(s) => s.fn(args)
        case Expr(e) => progn(e.body, makeCons(pairlis(e.args, args), e.env))
        case _ => makeError(printObj(fn) + " is not function")
      }
    }
  }
}

val subrCar = {(args: LObj) => safeCar(safeCar(args))}

val subrCdr = {(args: LObj) => safeCdr(safeCar(args))}

val subrCons = {(args: LObj) => makeCons(safeCar(args), safeCar(safeCdr(args)))}

val subrEq = {(args: LObj) => {
  val x = safeCar(args)
  val y = safeCar(safeCdr(args))
  (x, y) match {
    case (Num(n1), Num(n2)) =>
      if (n1.data == n2.data) sym_t
      else kNil
    case _ =>
      if (x == y) sym_t
      else kNil
  }
}}

val subrAtom = {(args: LObj) => {
  safeCar(args) match {
    case Cons(_) => kNil
    case _ => sym_t
  }
}}

val subrNumberp = {(args: LObj) => {
  safeCar(args) match {
    case Num(_) => sym_t
    case _ => kNil
  }
}}

val subrSymbolp = {(args: LObj) => {
  safeCar(args) match {
    case Sym(_) => sym_t
    case _ => kNil
  }
}}

def subrAddOrMul(fn: (Int, Int) => Int, init_val: Int) = {
  def doit(args: LObj, ret: Int): LObj = {
    args match {
      case Cons(c) => {
        c.car match {
          case Num(n) => doit(c.cdr, fn(ret, n.data))
          case _ => makeError("wrong type")
        }
      }
      case _ => makeNum(ret)
    }
  }
  {(args: LObj) => doit(args, init_val)}
}
val subrAdd = subrAddOrMul({(x: Int, y: Int) => x + y}, 0)
val subrMul = subrAddOrMul({(x: Int, y: Int) => x * y}, 1)

def subrSubOrDivOrMod(fn: (Int, Int) => Int) = {
  {(args: LObj) =>
    (safeCar(args), safeCar(safeCdr(args))) match {
      case (Num(n1), Num(n2)) => makeNum(fn(n1.data, n2.data))
      case _ => makeError("wrong type")
    }
  }
}
val subrSub = subrSubOrDivOrMod({(x: Int, y: Int) => x - y})
val subrDiv = subrSubOrDivOrMod({(x: Int, y: Int) => x / y})
val subrMod = subrSubOrDivOrMod({(x: Int, y: Int) => x % y})

addToEnv(makeSym("car"), makeSubr(subrCar), g_env)
addToEnv(makeSym("cdr"), makeSubr(subrCdr), g_env)
addToEnv(makeSym("cons"), makeSubr(subrCons), g_env)
addToEnv(makeSym("eq"), makeSubr(subrEq), g_env)
addToEnv(makeSym("atom"), makeSubr(subrAtom), g_env)
addToEnv(makeSym("numberp"), makeSubr(subrNumberp), g_env)
addToEnv(makeSym("symbolp"), makeSubr(subrSymbolp), g_env)
addToEnv(makeSym("+"), makeSubr(subrAdd), g_env)
addToEnv(makeSym("*"), makeSubr(subrMul), g_env)
addToEnv(makeSym("-"), makeSubr(subrSub), g_env)
addToEnv(makeSym("/"), makeSubr(subrDiv), g_env)
addToEnv(makeSym("mod"), makeSubr(subrMod), g_env)
addToEnv(sym_t, sym_t, g_env)

print("> ")
for (line <- Source.stdin.getLines) {
  println(printObj(eval(read(line)._1, g_env)))
  print("> ")
}
