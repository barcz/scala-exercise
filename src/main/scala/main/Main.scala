package main

import java.io.File

import scala.collection.mutable
import scala.io.{Source, StdIn}
import scala.util.Try

object Main extends App {

  type Index = mutable.Map[String, List[String]]

  sealed trait IndexOrError

  case object NoParamsDefined extends IndexOrError

  case object NotADirectory extends IndexOrError

  openFilesInDir(args).fold(error => println(error), files => indexAndSearch(files, () => StdIn.readLine(), s => println(s)))

  def openFilesInDir(arguments: Array[String]): Either[IndexOrError, Array[File]] = for {
    fileName <- arguments.headOption.toRight(NoParamsDefined)
    files <- Try(new File(fileName)).filter(_.isDirectory).map(_.listFiles()).toOption.toRight(NotADirectory)
  } yield files

  def indexAndSearch(files: Array[File], inputString: () => String, output: String => Unit ) : Unit = {
    val index = indexFiles(files)

    def searchOrExit(): Unit = {
      print("search> ")
      val input = inputString.apply()
      if (!input.equals(":quit")) {
        val inputWords = input.split(" ")
        val result = search(index, inputWords)
        if (result.isEmpty) {
          output("no matches found")
        } else {
          output(result.map(r => s"${r._1} : ${r._2}%").mkString(" "))
        }
        searchOrExit()
      }
    }
    searchOrExit()
  }

  def indexFiles(files: Array[File]): Index =
    files.foldLeft(mutable.Map[String, List[String]]().empty)((i, f) => indexFile(f, i))

  def indexFile(f: File, indexToUse: Index): Index =
    Source.fromFile(f).getLines().flatMap(line => line.split(" ")).foldLeft(indexToUse)((i, w) => index(i, w, f.getName))

  def index(index: Index, word: String, fileName: String): Index = {
    if (index.contains(word)) index.update(word, fileName :: index(word.toLowerCase))
    else index.addOne(word -> List(fileName))
    index
  }

  def search(index: Index, words: Array[String]): Seq[(String, Int)] = {
    val numberOfWords = words.size
    words.flatMap(word => index.getOrElse(word.toLowerCase, Nil)).groupMapReduce(identity)(x => 1)(_ + _).view.mapValues(i => (i.floatValue / numberOfWords.floatValue * 100).intValue)
      .toSeq.sortWith((a, b) => a._2 > b._2).take(10)
  }
}
