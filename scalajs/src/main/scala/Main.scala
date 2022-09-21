import scalanat.proof.Proof

@main def hello: Unit = 
  given scalanat.term.Symbols = scalanat.term.DefaultSymbols
  val p = Proof("assume a and b\ndischarge")

  println("Hello world!")
  println(msg)
  println(p)

def msg = "I was compiled by Scala 3. :)"
