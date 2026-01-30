package tpagu.compiler
import munit.BaseFunSuite
import munit.Location
import java.nio.file.Paths
import java.nio.file.Files
import java.nio.file.StandardOpenOption

object GoldCopyErrors {
  val path = Paths.get(s"src/test/goldcopies/gc-errors")
  val path_files = Paths.get(s"src/test/goldcopies/gc-errors-files")
  
  def init() = {
      Files.deleteIfExists(path)
      Files.deleteIfExists(path_files)
      Files.createFile(path)
      Files.createFile(path_files)
  }

  def addError(fullName:String, old:String, newVal: String) = {
    Files.writeString(path, s"${fullName}\nold:${old}\nnew:${newVal}\n---------------------------\n\n", StandardOpenOption.APPEND)
    Files.writeString(path_files, s"${fullName}\n", StandardOpenOption.APPEND)
  }

  init()
}

trait GoldCopyFunSuite extends BaseFunSuite {
  def goldcopyTest(
      name: String
  )(body: => String)(implicit loc: Location): Unit = {
    test(name) {
      val suiteName = this.getClass.getSimpleName
      val filename = s"${suiteName}-${name}"
      val result = body
      val path = Paths.get(s"src/test/goldcopies/gc-files/${filename}")
      if (Files.exists(path)) {
        val content = Files.readString(path);
        if (result != content) {
          GoldCopyErrors.addError(filename,content, result)
        }
        assertEquals(result, content);
      } else {
        Files.writeString(path, result)
      }
    }
  }
}
