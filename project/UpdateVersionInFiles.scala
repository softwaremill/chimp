import sbt.{File, IO, Logger}

import scala.util.matching.Regex

object UpdateVersionInFiles {
  private val TextFileExtensions = Set(".scala", ".sc", ".md", ".rst")
  private val IgnoredDirectories = Set("target", ".scala-build", "_build", ".venv", "node_modules")

  def scalaCliDependencies(log: Logger, organization: String, version: String, roots: Seq[File]): Seq[File] = {
    val coordinate = s"""(\\Q$organization\\E:{1,3}[\\w.-]+:)[\\w.-]+""".r
    roots
      .flatMap(textFilesIn(log, _))
      .flatMap(rewrite(log, _)(content => coordinate.replaceAllIn(content, m => Regex.quoteReplacement(m.group(1) + version))))
  }

  def sphinxVersion(log: Logger, version: String, confFile: File): Seq[File] = {
    val shortVersion = version.split('.').take(2).mkString(".")
    if (!confFile.exists()) {
      log.warn(s"[UpdateVersionInFiles] ${confFile.getPath} does not exist, skipping...")
      Nil
    } else
      rewrite(log, confFile) { content =>
        replaceAssignment(replaceAssignment(content, "version", shortVersion), "release", version)
      }.toSeq
  }

  private def replaceAssignment(content: String, key: String, value: String): String =
    s"""(?m)^$key\\s*=\\s*(u?)(['"])[^'"]*\\2""".r
      .replaceAllIn(content, m => Regex.quoteReplacement(s"""$key = ${m.group(1)}${m.group(2)}$value${m.group(2)}"""))

  private def textFilesIn(log: Logger, root: File): Seq[File] =
    if (!root.exists()) {
      log.warn(s"[UpdateVersionInFiles] ${root.getPath} does not exist, skipping...")
      Nil
    } else if (!root.isDirectory) Seq(root)
    else
      Option(root.listFiles()).toSeq.flatten.flatMap {
        case d if d.isDirectory => if (IgnoredDirectories.contains(d.getName)) Nil else textFilesIn(log, d)
        case f if TextFileExtensions.exists(f.getName.endsWith(_)) => Seq(f)
        case _                                                     => Nil
      }

  private def rewrite(log: Logger, f: File)(update: String => String): Option[File] = {
    val current = IO.read(f)
    val updated = update(current)
    if (updated == current) None
    else {
      log.info(s"[UpdateVersionInFiles] Updating version in ${f.getPath}")
      IO.write(f, updated)
      Some(f)
    }
  }
}
