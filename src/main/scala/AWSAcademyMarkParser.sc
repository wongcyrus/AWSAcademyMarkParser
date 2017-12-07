import java.io.File

def getListOfFiles(dir: String): List[File] = {
  val d = new File(dir)
  if (d.exists && d.isDirectory) {
    d.listFiles.filter(_.isFile).toList
  } else {
    List[File]()
  }
}

def getProjectMarks(filePathName: File) = {
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

def getAssessmentMarks(filePathName: File) = {
  import org.htmlcleaner.HtmlCleaner

  import scala.io.Source

  val cleaner = new HtmlCleaner

  val fileContents = Source.fromFile(filePathName).getLines.mkString

  val rootNode = cleaner.clean(fileContents)

  import scala.collection.JavaConverters._
  val headers = rootNode.getElementsByAttValue("class", "header", true, false).head
    .getChildTagList.asScala.map(_.getText).toList.mkString(",")

  val students = rootNode.getElementsByAttValue("class", "alternative_row", true, false)
    .map(_.getChildTagList.asScala.map(a => a.getText.toString)
      .map({
        case "C" => 1
        case "P" => 0
        case "NS" => 0
        case default  => default
      })
      .toList.mkString(",")).toList


  (headers, students)
}

def createProjectMarksCSV() = {
  import java.io._

  val pw = new PrintWriter(new File("E:\\Project Mark.csv"))
  pw.println("ID,Name,Mark")

  getListOfFiles("E:\\Final Project\\")
    .flatMap(getProjectMarks)
    .foreach(a => pw.println(a._1 + "," + a._2))
  pw.close()
}

def createAssessmentMarksCSV() = {
  import java.io._

  val pw = new PrintWriter(new File("E:\\Lab Mark.csv"))

  val files = getListOfFiles("E:\\Lab")
  val headers = getAssessmentMarks(files.head)._1
  pw.println(headers)
  files
    .flatMap(c => getAssessmentMarks(c)._2)
    .foreach(pw.println)
  pw.close()
}
createProjectMarksCSV()
createAssessmentMarksCSV()
