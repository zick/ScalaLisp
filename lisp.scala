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

val sym_quote = makeSym("quote")

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

print("> ")
for (line <- Source.stdin.getLines) {
  println(printObj(read(line)._1))
  print("> ")
}
