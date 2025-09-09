package org.aurora

import org.scalatest._
import wordspec._
import matchers._



class FirstTest extends AnyWordSpec with should.Matchers{
  "this" should {
    "work" in {
      info(fileutils.cwd)
      info(fileutils.testResourcesPath)

      fileutils.createFileSync(s"${fileutils.testResourcesPath}/test.txt")
      true should be(true)
    }
  }
}
