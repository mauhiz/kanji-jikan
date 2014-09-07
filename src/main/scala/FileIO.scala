import java.nio.file._

import scala.util.Try

trait ReadFile {
  protected def readIfPossible[A](path: Path)(doWithData: String => Option[A]): Option[A] = {
    Try {
      if (Files.exists(path) && Files.isReadable(path) && Files.isRegularFile(path)) {
        val data = new String(Files.readAllBytes(path), "UTF-8")
        doWithData(data)
      } else {
        None
      }
    }.toOption.flatten
  }
}

trait WriteFile {
  protected def writeIfPossible(path: Path, data: String) = {
    val parent = path.getParent
    Files.createDirectories(parent)
    Files.write(path, data.getBytes("UTF-8"), StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE)
  }
}

