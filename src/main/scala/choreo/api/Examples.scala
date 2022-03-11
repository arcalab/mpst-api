package choreo.api

import choreo.syntax.*
import caos.common.Example
import choreo.common.Simplify

object Examples:

  def simple(e:Choreo): Choreo = Simplify(e)

  val m: Agent = Agent("m")
  val w1: Agent = Agent("w1")
  val w2: Agent = Agent("w2")
  val w3: Agent = Agent("w3")
  val b: Agent = Agent("b")
  val s: Agent = Agent("s")
  val price: Msg = Msg(List("Price"))
  val descr: Msg = Msg(List("Descr"))
  val acc: Msg = Msg(List("Acc"))
  val rej: Msg = Msg(List("Rej"))
  val ack: Msg = Msg(List("Ack"))
  val work: Msg = Msg(List("Work"))
  val done: Msg = Msg(List("Done"))

  // Example 3 in paper
  val buyerSeller:Choreo =
    ((s->b|descr) || (s->b|price)) > ((b->s|acc) + (b->s|rej))

  // Example 4 in paper
  val masterWorker2:Choreo =
    (m->w1|work) > (m->w2|work) > ((w1->m|done) || (w2->m|done))

  // Example 12 in paper
  val masterWorker3:Choreo =
    (m->w1|work) > (m->w2|work) > (m->w3|work) >
      ((w1->m|done) || (w2->m|done) || (w3->m|done))

  val basicBuyerSellerImpl:String =
    """def seller(s: S$State$Init): S$State$Final = s
      |  .send(B, Descr)
      |  .send(B, Price)
      |  .recv(
      |    (_, _, s) => { println("offer accepted"); s },
      |    (_, _, s) => { println("offer rejected"); s }
      |  )
      |
      |def buyer(s: B$State$Init): B$State$Final = s
      |  .recv(
      |    (_, _, s) => s.recv(
      |      (_, _, s) => s.send(S, Rej) // or s.send(S, Acc)
      |    )
      |  )
      |
      |val pr = new Protocol
      |pr.run(seller)
      |pr.run(buyer)""".stripMargin

  val relaxedBuyerSellerImpl:String =
    """def seller(s: S$Initial): S$Final = s
       |  .send(B, new Descr)
       |  .send(B, new Price)
       |  .recv(
       |    (_, _, s) => { println("offer accepted"); s },
       |    (_, _, s) => { println("offer rejected"); s }
       |  )
       |
       |def buyer(s: B$Initial): B$Final = s
       |  .recv(
       |    (_, _, s) => s.recv((_, _, s) => s.send(S, new Acc)),
       |    (_, _, s) => s.recv((_, _, s) => s.send(S, new Rej))
       |  )
       |
       |val pr = new Protocol
       |pr.run(seller)
       |pr.run(buyer)
       |""".stripMargin

  val masterWorkerImpBasic =
    """def master(s: M$State$Init): M$State$Final =
      | s.send(W1, Work).send(W2,  Work).recv((_, _, s) => {
      |     println("#1"); s.recv((_, _, s) => {
      |       println("#2"); s }) })
      |
      |
      |def worker1(s: W1$State$Init): W1$State$Final = s
      |  .recv((_, _, s) => {
      |    Thread.sleep(scala.util.Random.nextInt(1000)); s.send(M, Done)
      |  })
      |
      |def worker2(s: W2$State$Init): W2$State$Final = s
      |  .recv((_, _, s) => {
      |    Thread.sleep(scala.util.Random.nextInt(1000)); s.send(M, Done)
      |  })""".stripMargin

  val masterWorkerImpl =
     """def master(s: M$State$Init): M$State$Final =
       |  val (s1, s2) = s.send(W1, Work).send(W2,  Work).fork()
       |  val f1 = Future { s1.recv((_, _, s) => { println("#1"); s }) }
       |  val f2 = Future { s2.recv((_, _, s) => { println("#2"); s }) }
       |  Await.result(
       |    for { t1 <- f1; t2 <- f2 } yield M$State.join(t1, t2),
       |    Duration.Inf
       |  )
       |
       |def worker1(s: W1$State$Init): W1$State$Final = s
       |  .recv((_, _, s) => {
       |    Thread.sleep(scala.util.Random.nextInt(1000)); s.send(M, Done)
       |  })
       |
       |def worker2(s: W2$State$Init): W2$State$Final = s
       |  .recv((_, _, s) => {
       |    Thread.sleep(scala.util.Random.nextInt(1000)); s.send(M, Done)
       |  })
       |
       |val pr = new Protocol
       |pr.run(master)
       |pr.run(worker1)
       |pr.run(worker2)
       |""".stripMargin

  val masterWorker3Impl =
    """def master(s: M$State$Init): M$State$Final =
      |  val (s1, s2, s3) = s.send(W1, Work).send(W2,  Work).send(W3, Work).fork()
      |  val f1 = Future { s1.recv((_, _, s) => { println("#1"); s }) }
      |  val f2 = Future { s2.recv((_, _, s) => { println("#2"); s }) }
      |  val f3 = Future { s3.recv((_, _, s) => { println("#3"); s }) }
      |  Await.result(
      |    for { t1 <- f1; t2 <- f2 ; t3 <- f3 } yield M$State.join(t1, t2, t3),
      |    Duration.Inf
      |  )
      |
      |def worker1(s: W1$State$Init): W1$State$Final = s
      |  .recv((_, _, s) => {
      |    Thread.sleep(scala.util.Random.nextInt(1000)); s.send(M, Done)
      |  })
      |
      |def worker2(s: W2$State$Init): W2$State$Final = s
      |  .recv((_, _, s) => {
      |    Thread.sleep(scala.util.Random.nextInt(1000)); s.send(M, Done)
      |  })
      |
      |def worker3(s: W3$State$Init): W3$State$Final = s
      |  .recv((_, _, s) => {
      |    Thread.sleep(scala.util.Random.nextInt(1000)); s.send(M, Done)
      |  })
      |
      |val pr = new Protocol
      |pr.run(master)
      |pr.run(worker1)
      |pr.run(worker2)
      |pr.run(worker3)
      |""".stripMargin

  def htmlify(s:String):String =
    s.replace("\n","<br>")

  val examples =
    Example(
      "// Buyer-Seller, Basic\n" +
        "s->b:Descr .\ns->b:Price .\n(b->s:Acc+b->s:Rej)",
      "Buyer-Seller, Basic",
      s"""<strong>Basic protocol for the Buyer-Seller example</strong>
        |
        |The code below is a possible implementation of a process that follows this protocol.
        |<pre style="font-size: 1.1rem;">${htmlify(basicBuyerSellerImpl)}</pre>
        |
        |To quickly run this example you can:
        |<ul><li>open <a target="#" href="https://scastie.scala-lang.org">https://scastie.scala-lang.org</a>
        |  to run Scala code online</li>
        |  <li> Copy the code in the "All" tab of the "Scala APIs" widget and paste it on the Scastie</li>
        |  <li> Copy the process implementations above and paste it on the bottom of the same Sastie</li>
        |  <li> Press "Run" to run the process.</li>
        |</ul>""".stripMargin
    ):: Example(
      s"""// 1 Master - 2 Workers, Basic\n""" +
        "m->w1:Work . m->w2:Work .\nw1->m:Done . w2->m:Done",
      "1Master-2Workers, Basic",
      s"""<strong>Master-Worker: Basic protocol</strong>
        |
        |A master sends some Work to 2 workers sequentially, later it collects their results also sequentially.
        |The code below is a possible implementation of a process that follows this protocol.
        |<pre style="font-size: 1.1rem;">${htmlify(masterWorkerImpBasic)}</pre>""".stripMargin
    ):: Example(
      s"""// Buyer-Seller, Relaxed\n""" +
        "(s->b:Descr || s->b:Price) .\n(b->s:Acc + b->s:Rej)",
      "Buyer-Seller, Relaxed",
      s"""<strong>Buyer-Seller, Relaxed</strong>
         |
         |The code below is a possible implementation of a process that follows this protocol.
         |<pre style="font-size: 1.1rem;">${htmlify(relaxedBuyerSellerImpl)}</pre>
         |""".stripMargin
    ):: Example(
      s"""// 1 Master - 2 Workers, Relaxed\n""" +
        "m->w1:Work . m->w2:Work .\n(w1->m:Done || w2->m:Done)",
      "1Master-2Workers, Relaxed" ,
      s"""<strong>Master-Worker: relaxed protocol</strong>
        |
        |The code below is a possible implementation of a process that follows this protocol, using forks and joins.
        |<pre style="font-size: 1.1rem;">${htmlify(masterWorkerImpl)}</pre>""".stripMargin
    ):: Example(
      s"""// 1 Master - 3 Workers, Relaxed\n""" +
        "m->w1:Work . m->w2:Work . m->w3:Work .\n(w1->m:Done || w2->m:Done || w3->m:Done)",
      """1Master-3Workers, Relaxed""" ,
      s"""<strong>Master - 3 Workers: relaxed protocol</strong>
        |
        |The code below is a possible implementation of a process that follows this protocol, using forks and joins.
        |<pre style="font-size: 1.1rem;">${htmlify(masterWorker3Impl)}</pre>""".stripMargin
    ):: Example(
      "// Ex. 1\n" +
      "(a->b:M1 . (a->b:N1 || a->b:N2) || \n a->b:M2) . a->b:End",
      "Ex. 1",
      ""
    ):: Example(
      "// Ex. 2\n" +
        "a->b:M1 . (a->b:N1 || a->b:N2) || \n a->b:M2",
      "Ex. 2",
      ""
    ):: Example(
      "// Ex. 3\n" +
        "a->b:M . (a->b:M1 || a->b:M2) . \n a->b:End",
      "Ex. 3",
      ""
    ):: Example(
      "// Ex. 4\n" +
        "a->b:M . (a->b:M1 || a->b:M2)",
      "Ex. 4",
      ""
    ):: Example(
      s"""a->b:x + a->b:y""",
      "Ex. 5","Simple realisable choice"
    ):: Example(
      s"""a->b:x . b->c:z +
         |a->c:y . c->b:w""".stripMargin,
      "Ex. 6","Less simple realisable choice"
    ):: Example(
      s"""a->b:x + c->d:x""",
      "Ex. 7","Simple unrealisable choice"
    ):: Example(
      s"""(a->b:x || c->b:x). a->b:z +
         |a->b:y . c->b:y . a->b:z""".stripMargin,
      "Ex. 8","Complex unrealisable protocol"
    ):: Example(
      "// Ex. 9\n" +
        "a->b:M . (a->b:M1 || a->b:M2) . \n a->b:End1 || a->b:End2",
      "Ex. 9",
      ""
    ):: Example(
      "// Ex. 10\n" +
        "a->b:M . (a->b:M1 || a->b:M2) . \n a->b:M3 . a->b:End1 ||\n a->b:End2",
      "Ex. 10",
      ""
    )::Nil

