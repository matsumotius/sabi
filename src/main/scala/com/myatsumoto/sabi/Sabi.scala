package com.myatsumoto.sabi
import scala.collection.mutable.HashMap

class Sabi(input:String) {

  def search(str:String):List[Int] = {
    val (found, head, tail) = binarySearch(str)
    if (found)
      (head to tail).map(index =>
        sortedInput(index)("index").asInstanceOf[Int]
      ).toList
    else
      List(-1)
  }

  val sortedInput = sortInput(input)
  private def sortInput(input:String):Array[HashMap[String, Any]] = {
    (0 until input.size).map(index => 
      HashMap(
        "index" -> index,
        "value" -> input.drop(index)
      )
    ).sortWith((a, b) => 
      a("value").toString < b("value").toString
    ).toArray
  }

  private def binarySearch(target:String):(Boolean, Int, Int) = {
    var (lower, upper) = (0, sortedInput.size - 1)
    while (upper >= lower) {
      var i = lower + (upper - lower) / 2
      val str = sortedInput(i)("value").toString
      if (str.startsWith(target)) {
        val head = linearSearch(target, i, false)
        val tail = linearSearch(target, i, true)
        return (true, head, tail)
      }
      if (str < target) lower = i + 1 else upper = i - 1
    }
    return (false, upper, lower)
  }

  private def linearSearch(target:String, pivot:Int, dir:Boolean):Int = {
    var (lower, upper) = if (dir) (pivot, sortedInput.size - 1) else (0, pivot)
    while (upper >= lower) {
      val i = lower + (upper - lower) / 2
      val str = sortedInput(i)("value").toString
      if (str.startsWith(target) ^ dir) upper = i - 1 else lower = i + 1
    }
    if (dir) upper else lower
  }
}
