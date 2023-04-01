package multithreading2

import java.util.concurrent.CountDownLatch
import java.util.concurrent.CyclicBarrier
import java.util.concurrent.Exchanger
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.Semaphore
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.ArrayBlockingQueue


object ppconcurency_2 extends App{
  def sum_factorials(a: Int,b: Int,fn: (Int) => Int,next: (Int) => Int): Int = {
    if(a > b) 0
    else fn(a) + sum_factorials(next(a), b, fn, next)

  }

  def fact(n:Int): Int=
  {
    if(n == 1) 1
    else n * fact(n - 1)
  }
  def armstrong(n:Int):Int = {
    var sum = 0
    var temp = n
    while(temp != 0) {
      var r = temp % 10
      sum += r * r * r
      temp /= 10
    }
    if(sum == n)
      return 2
    else
      return 3
  }
  def doRandomWork(): Int ={
    Thread.sleep(util.Random.nextInt(1000)+1)
    //Thread.sleep(3000)
    util.Random.nextInt(10)
  }
  println("---------------------------------")
  println("CyclicBarrier and countDownLatch")

  // cyclicBarrier and countdownLatch
  val numThreads = 5
  val cb = new CyclicBarrier(numThreads)
  val cdl = new CountDownLatch(numThreads)
  val cbFutures = for(i <- 1 to numThreads) yield Future{
    doRandomWork()
    //sum_factorials(1, 3, (x: Int) => fact(x), (x: Int) => x + 1)
    println(s"CB Thread $i awaiting")
    cb.await() // cyclic barrier waits till all threads are ready
    cdl.countDown() // prevents main thread from exiting
  }
  println("Main Tread await CDL")
  cdl.await()
  println("Cyclic Barrier all Done")
  println("---------------------------------")
  println("Exchanger")
  //Exchanger - use to avoid deadlock and race condition
  val ex = new Exchanger[Int]
  Future{
    //val v1 = doRandomWork()
    val v1 = sum_factorials(1, 3, (x: Int) => fact(x), (x: Int) => x + 1)
    println(s"Exchange 1 calculated $v1")
    val v2 = ex.exchange(v1)
    println(s"Exchange 1 received $v2")
  }

  Future{
    //val v2 = doRandomWork()
    val v2 = sum_factorials(1, 4, (x: Int) => fact(x), (x: Int) => x + 2)
    println(s"Exchange 2 calculated $v2")
    val v1 = ex.exchange(v2)
    println(s"Exchange 2 received $v1")
  }
  println("---------------------------------")
  println("BlockingQueue and blocking Dequeue")

  //Blocking Queue and Blocking Dequeue
  val bq = new ArrayBlockingQueue[Int](3)
  val numProducts = 7
  val cdl2 = new CountDownLatch(numProducts)
  Future{
    for(i <- 1 to numProducts){
      val prod = doRandomWork()
      println(s"Producer $i produced $prod")
      bq.put(prod) // adding to the blocking queue
      //cdl2.countDown()
    }
  }
  Thread.sleep(3000)
  Future{
    for(i <- 1 to numProducts){
      val cons = bq.take() // waits for the element to be available and take from the blocking queue
      println(s"Consumer $i Consumed $cons")
      cdl2.countDown() // makes sure that all the elements are consumed does,t allow main thread to run
    }
  }
  cdl2.await()
  println("---------------------------------")
  println("Concurrent HashMap using semaphore")
  //concurentHashMap
  val chm = new ConcurrentHashMap[String,String]
  //semaphore
  val sem = new Semaphore(3)
  for(i <- 1 to numThreads*2)Future{
    sem.acquire()
    println(s"Running Thread $i")
    Thread.sleep(2000)
    chm.put(s"t$i","Running")
    sem.release()
  }
  while(chm.size() < numThreads){
    Thread.sleep(10)
  }
  println(chm)
}