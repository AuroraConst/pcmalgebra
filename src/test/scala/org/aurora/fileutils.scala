package org.aurora

import scala.scalajs.js
import scala.scalajs.js.annotation.*

import scala.scalajs.js.Dynamic.global
import typings.fsExtra


@js.native
@JSImport("process", JSImport.Namespace)
private object Process extends js.Object {
  def cwd(): String = js.native
}

/**
  * FileReader object to read files from the file system and creates a string dsl for platorm independent paths
  */

object fileutils:
  def platform =  if (!js.isUndefined(global.process)) {
      global.process.platform.asInstanceOf[String]
    } else {
      "unknown"
    }
  val separator = platform match {
    case "win32" => "\\"
    case _ => "/"
  }

  extension (spath:String)
    def /(path: String): String = spath + separator + path



  def cwd = Process.cwd()
  def testResourcesPath = cwd / "src" / "test" / "resources"
  def createFileSync(path: String) =
    fsExtra.mod.createFileSync(path)
end fileutils