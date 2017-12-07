import java.io.File

def getListOfFiles(dir: String): List[File] = {
  val d = new File(dir)
  if (d.exists && d.isDirectory) {
    d.listFiles.filter(_.isFile).toList
  } else {
    List[File]()
  }
}

def getMarks(filePathName: File) = {
  import org.htmlcleaner.HtmlCleaner

  import scala.io.Source

  val cleaner = new HtmlCleaner

  val fileContents = Source.fromFile(filePathName).getLines.mkString

  val rootNode = cleaner.clean(fileContents)

  val names = rootNode.getElementsByAttValue("class", "avatar", true, false)
    .map(a => a.getParent.getText.toString)

  val scores = rootNode.getElementsByAttValue("class", "hover-menu__trigger", true, false)
    .map(a => a.getElementsByName("span", false).head.getText.toString.split("/")(0))
    .map {
      case number if number.matches("\\d+") => number.toInt
      case _ => 0
    }

  names.zip(scores)
}

import java.io._

val pw = new PrintWriter(new File("E:\\Project Mark.csv"))
pw.println("ID,Name,Mark")

getListOfFiles("E:\\Final Project\\")
  .flatMap(getMarks)
  .foreach(a => pw.println(a._1 + "," + a._2))
pw.close()