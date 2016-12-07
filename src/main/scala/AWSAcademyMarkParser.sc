import java.io._

import com.itextpdf.text.pdf.PdfReader
import com.itextpdf.text.pdf.parser.PdfTextExtractor

val basePath = "E:\\Working\\CodeMaker\\src\\main\\resources\\"
val filename = "beta-3-student-assessment-pdf"
val pdfReader = new PdfReader(basePath + filename + ".pdf");

def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
  val p = new java.io.PrintWriter(f)
  try {
    op(p)
  } finally {
    p.close()
  }
}

val content = (1 to pdfReader.getNumberOfPages).map(i => PdfTextExtractor.getTextFromPage(pdfReader, i)).fold("")(_ + _)

val segments = content.split("AWS Academy -")
segments.length

def toInt(s: String): Option[Int] = {
  try {
    Some(s.toInt)
  } catch {
    case e: Exception => None
  }
}

def extractLabName(s: String) = {
  if (s.contains("Final Project"))
    Some("Final Project")
  else if (s.contains("Optional Lab"))
    Some("Optional Lab")
  else if (s.contains("- Lab ")) {
    val segment = s.substring(s.indexOf("- Lab "))
    Some("Lab " + segment.split(" ")(2))
  }
  else
    None
}

def extractCourse(s: String) = {
  if (s.contains("Architecting on AWS"))
    Some("Architecting on AWS")
  else if (s.contains("Technical Essentials"))
    Some("Technical Essentials")
  else if (s.contains("Final Project"))
    Some("Architecting on AWS")
  else if (s.contains("Optional Lab"))
    Some("Architecting on AWS")
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

val r = """(?i)\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b""".r
val rawMarks = segments.map(s => {
  (r.findFirstMatchIn(s), extractCourse(s), extractLabName(s), extractMark(s))
}
).filter(r => !r._1.isEmpty) //filter first row

val mark = rawMarks.map(a => (a._1.get.matched, a._2.get, a._3.get, a._4.get)) //Materialize it

val groupByEmail = mark groupBy (_._1) mapValues (_ map { f => (f._2 + " " + f._3, f._4) })


val labName = Seq(
  "Technical Essentials Lab 1",
  "Technical Essentials Lab 2",
  "Technical Essentials Lab 3",
  "Architecting on AWS Lab 1",
  "Architecting on AWS Lab 2",
  "Architecting on AWS Lab 3",
  "Architecting on AWS Lab 4",
  "Architecting on AWS Lab 5",
  "Architecting on AWS Optional Lab",
  "Architecting on AWS Final Project"
)


val maxMark =
  groupByEmail.map(a => (
    a._1,
    a._2.groupBy(_._1) //Group by Lab
      .map {
      x => x._1 -> x._2.map(_._2).max //emit map[lab,mark]
    })
  )


printToFile(new File(basePath + filename + "raw.txt")) { p =>
  p.write(content)
}

printToFile(new File(basePath + filename + "groupByEmail.txt")) { p =>
  groupByEmail.map(a => {
    p.print(a._1 + ":\t")
    p.print(a._2.mkString(","))
    p.println()
  }
  )
}

printToFile(new File(basePath + filename + ".csv")) { p =>
  p.println("Email," + labName.mkString(","))
  maxMark.map(s =>
    p.println((s._1 + "," + labName.map(l => s._2.getOrElse(l, 0)).mkString(","))
    )
  )
}