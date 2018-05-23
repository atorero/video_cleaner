import java.io._
import java.nio.file.Files
import java.nio.file._
import java.util.Arrays

object Merger {

    def sameFiles(file1: File, file2: File) = {
        // FileUtils.contentEquals(file1, file2) 
        val f1 = Files.readAllBytes(file1.toPath)
        val f2 = Files.readAllBytes(file2.toPath)
        Arrays.equals(f1, f2)
    }

    def isInList(file: File, lengths: List[(File, Long)]) = 
        lengths.exists { case (file2, length) => 
            length == file.length && sameFiles(file, file2) }

    def move(file: File, dir: File) = Files.move(file.toPath, Paths.get(dir.getPath, file.getName))

    def merge(from: File, to: File) = {
        val filesTo = to.listFiles.toList
        val lengths = filesTo.map { f => (f, f.length) }

        val filesFrom = from.listFiles.toList
        filesFrom.foreach { f =>
            if (isInList(f, lengths)) f.delete()
            else move(f, to)
        }
    }
}

object Walker {

    def rightName(day: Int, month: Int, year: Int)  = f"$year-$month%02d-$day%02d"

    def walk(dir: File) = {
        val wrongPattern = "\\d{1,2}-\\d{2}-\\d{4}".r
        val files = dir.listFiles.toList
        files
            .filter{ _.isDirectory }
            .foreach { file =>
            file.getName match {
                case wrongPattern(month, day, year) =>
                    val correctFolder = rightName(day.toInt, month.toInt, year.toInt)
                    val correctFolderFile = Paths.get(file.getPath, correctFolder).toFile
                    if (correctFolderFile.exists) {
                        Merger.merge(file, correctFolderFile)
                        file.delete()
                    }
                    else Files.move(file.toPath, correctFolderFile.toPath)
                case _ =>
            }
        }
    }
}
