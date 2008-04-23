package net.lag.configgy;

import java.io.{BufferedReader, File, FileInputStream, InputStream, InputStreamReader}


/**
 * An interface for finding config files and reading them into strings for
 * parsing. This is used to handle <code>include</code> directives in config
 * files.
 */
trait Importer {
    /**
     * Imports a requested file and returns the string contents of that file.
     * If the file couldn't be imported, throws a <code>ParseException</code>.
     */
    @throws(classOf[ParseException])
    def importFile(filename: String): String
   
    private val BUFFER_SIZE = 8192

    /**
     * Exhaustively reads an InputStream and converts it into a String (using
     * UTF-8 encoding). This is meant as a helper function for custom Importer
     * classes.
     *
     * <p> No exceptions are caught!
     */
    protected def streamToString(in: InputStream): String = {
        val reader = new BufferedReader(new InputStreamReader(in, "UTF-8"))
        val buffer = new Array[Char](BUFFER_SIZE)
        val out = new StringBuilder
        var n = 0
        while (n >= 0) {
            n = reader.read(buffer, 0, buffer.length)
            if (n >= 0) {
                out.append(buffer, 0, n)
            }
        }
        out.toString
    }
}


/**
 * An Importer that looks for imported config files in the filesystem.
 * This is the default importer.
 */
class FilesystemImporter(val baseFolder: String) extends Importer {
    def importFile(filename: String): String = {
        var f = new File(filename)
        if (! f.isAbsolute) {
            f = new File(baseFolder, filename)
        }
        try {
            streamToString(new FileInputStream(f))
        } catch {
            case x => throw new ParseException(x.toString)
        }
    }
}


/**
 * An Importer that looks for imported config files in the java resources
 * of the system class loader (usually the jar used to launch this app).
 */
class ResourceImporter extends Importer {
    def importFile(filename: String): String = {
        try {
            streamToString(ClassLoader.getSystemClassLoader.getResourceAsStream(filename))
        } catch {
            case x => throw new ParseException(x.toString)
        }
    }
}
