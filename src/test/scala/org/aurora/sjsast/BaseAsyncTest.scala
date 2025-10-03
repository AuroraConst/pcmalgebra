package org.aurora.sjsast


import org.scalatest._

import wordspec._
import matchers._
import scala.concurrent.Future
import org.aurora.utils.fileutils.createFileIfNotExists

export org.aurora.utils.{fileutils,fs}

class BaseAsyncTest extends wordspec.AsyncWordSpec with should.Matchers{
  import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

  private lazy val testResourcesPath = fileutils.testResourcesPath
  private lazy val basefilename = this.getClass.getSimpleName.replace("Test","")
  private lazy val fullyQualifiedName = this.getClass.getName.replace("Test","").replace(".",fileutils.separator)
  private lazy val testPath = s"$testResourcesPath${fileutils.separator}$fullyQualifiedName"
  
  def testfile(index:Int) = 
    val path = s"$testResourcesPath${fileutils.separator}$fullyQualifiedName-$index.aurora"
    createFileIfNotExists(path)
    fileutils.readFileSync(path)

}
