package main

import java.io.File

import main.Main.indexAndSearch
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.{EitherValues, Inside}

import scala.collection.mutable

class MainTest extends AnyFunSuite with Matchers with EitherValues with Inside{

  test("Should return error if called without parameters") {
    val underTest: Either[Main.IndexOrError, Array[File]] = Main.openFilesInDir(Array.empty)
    underTest.left.value should be (Main.NoParamsDefined)
  }

  test("Should return error if the parameter is not an existing directory") {
    val underTest: Either[Main.IndexOrError, Array[File]] = Main.openFilesInDir(Array("not_a_real_file"))
    underTest.left.value should be (Main.NotADirectory)
  }

  test("Should return error if the parameter is not a directory") {
    val underTest: Either[Main.IndexOrError, Array[File]] = Main.openFilesInDir(Array("build.sbt"))
    underTest.left.value should be (Main.NotADirectory)
  }

  test("Should return all files from the defined directory") {
    val underTest: Either[Main.IndexOrError, Array[File]] = Main.openFilesInDir(Array("testDir"))
    assert(underTest.isRight)
    inside(underTest) {
      case Right(a) => a.map(_.getName) should contain theSameElementsAs Array("1.txt","2.txt","3.txt")
    }
  }

  test("Should index file") {
    val firstFile: File = new File("testDir/1.txt")
    val firstIndex = Main.indexFile(firstFile, mutable.Map.empty)
    val secondFile: File = new File("testDir/2.txt")
    val secondIndex = Main.indexFile(secondFile, firstIndex)
    secondIndex should contain key "red"
    secondIndex("red") should contain theSameElementsAs List("1.txt", "2.txt")
    secondIndex should contain key "apple"
    secondIndex("apple") should contain theSameElementsAs List("1.txt", "2.txt")
    secondIndex should contain key "lemon"
    secondIndex("lemon") should contain theSameElementsAs List("1.txt", "2.txt")
    secondIndex should contain key "one"
    secondIndex("one") should contain theSameElementsAs List("1.txt", "2.txt")
    secondIndex should contain key "two"
    secondIndex("two") should contain theSameElementsAs List("1.txt", "2.txt")
    secondIndex should contain key "three"
    secondIndex("three") should contain theSameElementsAs List("1.txt", "2.txt")
    secondIndex should contain key "four"
    secondIndex("four") should contain theSameElementsAs List("1.txt")
    secondIndex should contain key "five"
    secondIndex("five") should contain theSameElementsAs List("1.txt")
    secondIndex should contain key "yellow"
    secondIndex("yellow") should contain theSameElementsAs List("1.txt")
  }

  test("should get back the correct results") {

    val inputStrings = Array("one two", "yellow", "eleven",":quit").iterator
    val outputStrings = Array("3.txt : 100% 2.txt : 100% 1.txt : 100%", "1.txt : 100%", "no matches found").iterator
    Main.openFilesInDir(Array("testDir")).fold(error => println(error), files => indexAndSearch(files, () => inputStrings.next(), s => assert(s === outputStrings.next())))

  }

}
