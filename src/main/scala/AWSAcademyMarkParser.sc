import java.io._

import com.itextpdf.text.pdf.PdfReader
import com.itextpdf.text.pdf.parser.PdfTextExtractor

val basePath = "E:\\Working\\"
val filename = "student-assessment-pdf"
val pdfReader = new PdfReader(basePath + filename + ".pdf")

def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
  val p = new java.io.PrintWriter(f)
  try {
    op(p)
  } finally {
    p.close()
  }
}

val content = (1 to pdfReader.getNumberOfPages).map(i => PdfTextExtractor.getTextFromPage(pdfReader, i)).fold("")(_ + _)
pdfReader.close

printToFile(new File(basePath + filename + "raw.txt")) { p =>
  p.write(content)
}

val segments = content.split("AWS CCA")
segments.length

def toInt(s: String): Option[Int] = {
  try {
    Some(s.toInt)
  } catch {
    case _: Exception => None
  }
}

def extractLabName(s: String) = {
  if (s.contains("Final Project"))
    Some("Final Project")
  else if (s.contains("Cloud Mid"))
    Some("Mid Curriculum Project")
  else
    None
}


def extractMark(s: String) = {
  val lines = s.split("\n")

  def captureMark(s: String): Option[Int] = {
    val segments = s.split(" ")
    val mark = toInt(segments(segments.length - 1))
    mark
  }

  val d = lines.filter(s => {
    val mark: Option[Int] = captureMark(s)
    s.contains("Cloud ") && !mark.isEmpty
  })

  if (d.length > 0) {
    captureMark(d.head)
  }
  else
    None
}

val emailRegex = """(?i)\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b""".r
val rawMarks = segments.map(s => {
  (emailRegex.findFirstMatchIn(s), extractLabName(s), extractMark(s))
}
).filter(r => !r._1.isEmpty) //filter first row

val mark = rawMarks.map(a => (a._1.get.matched, a._2.get, a._3.get)) //Materialize it

val groupByEmail = mark groupBy (_._1) mapValues (_ map { f => (f._2, f._3) })
printToFile(new File(basePath + filename + "groupByEmail.txt")) { p =>
  groupByEmail.map(a => {
    p.print(a._1 + ":\t")
    p.print(a._2.mkString(","))
    p.println()
  }
  )
}

val labName = Seq(
  "Mid Curriculum Project",
  "Final Project"
)

val maxMark =
  groupByEmail.map(a => (
    a._1,
    a._2.groupBy(_._1) //Group by Lab
      .map {
      x => x._1 -> x._2.map(_._2).max //emit map[lab,mark]
    })
  )


printToFile(new File(basePath + filename + ".csv")) { p =>
  p.println("Email," + labName.mkString(","))
  maxMark.map(s =>
    p.println((s._1 + "," + labName.map(l => s._2.getOrElse(l, 0)).mkString(","))
    )
  )
}