package net.lag

import java.io.File
import org.specs.runner.SpecsFileRunner


object TestRunner extends SpecsFileRunner("src/test/scala/**/*.scala", ".*")

trait TestHelper {
  private val _folderName = new ThreadLocal[File]

  // recursively delete a folder. should be built in. bad java.
  private def deleteFolder(folder: File): Unit = {
    for (val f <- folder.listFiles) {
      if (f.isDirectory) {
        deleteFolder(f)
      } else {
        f.delete
      }
    }
    folder.delete
  }

  def withTempFolder(f: => Any): Unit = {
    val tempFolder = System.getProperty("java.io.tmpdir")
    var folder: File = null
    do {
      folder = new File(tempFolder, "scala-test-" + System.currentTimeMillis)
    } while (! folder.mkdir)
    _folderName.set(folder)

    try {
      f
    } finally {
      deleteFolder(folder)
    }
  }

  def folderName = { _folderName.get.getPath }
}
