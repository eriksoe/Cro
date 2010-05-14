package cro.test;

import scala.util.parsing.combinator._
import scala.util.parsing.json.Lexer;
import scala.util.parsing.input.CharArrayReader.EofCh

import scala.util.parsing.input.PagedSeqReader
import scala.collection.immutable.PagedSeq

object TokenizerBenchmark extends Lexer {
  // Configure lexical parsing
  reserved ++= List("true", "false", "null")
  delimiters ++= List("{", "}", "[", "]", ":", ",")

  def myToken1 = rep1(letter) | token

  override def delim = super.delim

  // Restatement of lexer, in an accept-all-strings-and-always-make-progress kind of way:
  def myToken: Parser[Token] =
     ( string ^^ StringLit
      | number ~ letter ^^ { case n ~ l => ErrorToken("Invalid number format : " + n + l) }
      | '-' ~> whitespace ~ number ~ letter ^^ { case ws ~ num ~ l => ErrorToken("Invalid number format : -" + num + l) }
      | '-' ~> whitespace ~ number ^^ { case ws ~ num => NumericLit("-" + num) }
      | number ^^ NumericLit
      | EofCh ^^^ EOF
      | delim
      | rep1(letter) ^^ checkKeyword
      | elem("character", _=>true) ^^^ Identifier("bad char")
    )

  def parse[T](parser:Parser[T], source:java.io.Reader) : Int = {
    var src : Input = new PagedSeqReader(PagedSeq fromReader source)
    var done = false;
    var cnt = 0;
    do {
      cnt += 1;
      val r = parser(src)
      src = whitespace(r.next).next
    } while (! src.atEnd)
    return cnt
  }

  def parseFile[T](parser:Parser[T], filename:String) = {
    parse(parser, new java.io.FileReader(filename))
  }

  def main(args:Array[String]) = args.toList match {
    case List(":test") => {
      val org = myToken;//{x=>parser(x)};
      val repl = Cro.reopen(org, classOf[Parser[Token]])
      System.err.println("DB| parser: "+org+" => "+repl)
    }

    case List(filename) => {
      var totalDuration : Long = 0
      var totalCnt = 0
      for (i <- 1 until 20+1) {
	val before = System.currentTimeMillis
	val cnt = parseFile(myToken, filename)
	val after = System.currentTimeMillis
	val duration = after-before
	val speed = cnt / (duration / 1000.0)
	System.out.println(i+": "+cnt+" tokens in "+duration+" ms  (= "+speed+"/s)")
	if (i>10) {totalDuration += duration; totalCnt += cnt}
      }
      val totalSpeed = totalCnt / (totalDuration / 1000.0)
      System.out.println("Total: "+totalCnt+" tokens in "+totalDuration+" ms  (= "+totalSpeed+"/s)")
    }
  }
}
