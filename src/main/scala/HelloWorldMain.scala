import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
object HelloWorldMain {

  def main(args: Array[String]): Unit = {

    //println(sum_multiples_3_5(500))
    //println(patternCount("Abcde","abc"))
    //println(product("hi"))
    val testArray = Array(1,1,3,1,5,7,3)
    noDuplicates(testArray)
    //sumRepeatedArray()
    //sumAccuracy()
    //randomInts(7,55)
    //minmax(testArray)
    averageMedian()
  }

  //q1
  def sum_multiples_3_5(N: Double): Int = {
    var num = N.toInt
    var totalSum = 0;
    if(num > 0) {
      for( i<- 1 until num) {
        if(i < num && i % 15 != 0) {
          if(i %3 == 0 || i % 5 == 0) {
            totalSum += i
          }
        }
      }
      return totalSum
    } else {
      return 0
    }
  }
  // q2
  def patternCount(text: String, pattern: String): Int = {
    var count = 0
    var i = 0
    while(i < text.length && i <= (text.length - pattern. length) ){
      var temp = text.substring(i, i +pattern.length)
      if(temp == pattern) {
        count += 1
      }
      i += 1
    }
    return count
  }

  //q3
  def product(s: String): BigInt = {
    return s.map(_.toLong).product
  }

  //q4
  def noDuplicates(ints: Array[Int]): Array[Int] = {
    return ints.toSet.toArray
  }

  //q5
  def sumRepeatedArray() = {
    val repeatedArray = Array.fill[Float](1000000)(0.00001f)
    val actualSum = repeatedArray.sum
    val error = Math.abs(actualSum - 10) / 10

    println("The total sum is " + repeatedArray.sum)
    println("Error is: " +  error)
  }

  //q6
  def sumAccuracy() = {
    val firstActualSum = 0.0001f + 8000.0f

    val firstVal =  BigDecimal("0.000000001")
    val secondVal = BigDecimal("90000000")
    val secondSumExpected = firstVal + secondVal
    val secondActualSum = 0.000000001 + 90000000

    val firstSumExpected = (8000.0 + 0.0001)

    println("The first sum is: " + firstActualSum)
    println("The first expected is: " + firstSumExpected)
    println("The error for the first sum is: " + (Math.abs(firstActualSum - firstSumExpected) / firstSumExpected) + "\n")
    println("The second expected is: " + f"$secondSumExpected%8.9f")
    println("The second sum is: " + secondActualSum)
    println("The error for the second sum is: " + (secondActualSum - secondSumExpected).abs / secondSumExpected)
  }

  //q7
  def randomInts(n: Int, range: Int): Array[Int] = {
    val randomInt = scala.util.Random
    return Array.fill[Int](n)(randomInt.nextInt(range))
  }

  //q8
  def averageMedian(): Unit ={
    calcMedianAndAvg(1000,"1k.txt")
    calcMedianAndAvg(100000,"100k.txt")
    calcMedianAndAvg(10000000,"10Million.txt")
  }

  def calcMedianAndAvg(arraySize: Int, fileName: String): Unit ={
    //create the text file
    val textFile = new PrintWriter(fileName)
    var generateArr = for(i<- Array.fill[Int](arraySize)(scala.util.Random.nextInt(Int.MaxValue))) {
      textFile.println(i)
    }
    textFile.close()

    // Calculate the Average and Median of Text file
    val source = Source.fromFile(fileName, "UTF-8")
    val lineIterator = source.getLines
    var totalSumArr:BigInt = 0;
    var arrBuffer = new ArrayBuffer[BigInt]()
    var median :BigInt = 0;
    val startTime = System.nanoTime()

    for(line <- lineIterator) {
      arrBuffer += line.toInt
      totalSumArr += line.toInt
    }
    source.close()

    if(arrBuffer.length % 2 == 0) {
      median = (arrBuffer(arrBuffer.length /2 ) + arrBuffer((arrBuffer.length / 2) -1 ) )/2
    }
    else {
      median = arrBuffer.sortWith(_ < _).drop(arrBuffer.length / 2).head
    }

    val duration = (System.nanoTime - startTime) /1e9d
    val formatter = java.text.NumberFormat.getInstance
    val average = totalSumArr /arrBuffer.length
    println("The computation for array size " + formatter.format(arraySize) + " took: " + duration)
    println("Median for array size " + formatter.format(arraySize) + " is: " + formatter.format(median))
    println("Average for array size " + formatter.format(arraySize) + " is: " + formatter.format(average) + "\n")
  }

  //q9
  def minmax(values: Array[Int]):(Int, Int) = {
    return (values.min, values.max)
  }

  //q10
  class Car(val manufacturer: String, val modelName: String, val modelYear: Int = (-1), var license: String = ""){
    def this(manufacturer: String, modelName: String, modelYear: Int) = {
      this(manufacturer, modelName, modelYear, "")
    }

    def this(manufacturer: String, modelName: String) = {
      this(manufacturer, modelName, -1, "")
    }

    def this(manufacturer: String, modelName: String, licensePlate: String) = {
      this(manufacturer, modelName, -1, licensePlate)
    }
    // I would use the primary constructor that takes all four parameters. Using the primary constructor you can make
    // the auxillary constructors easily.
  }
}

